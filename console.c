/*
    $Id: console.c 3086 2023-09-03 06:23:08Z soci $

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

*/
#include "console.h"

#ifdef COLOR_OUTPUT
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 4
#endif
#else
#include <unistd.h>
#endif

bool console_use_color = false;
bool console_use_bold = false;

struct console_info_s {
    bool known;
    bool color;
#ifdef _WIN32
    bool use_ansi;
    int attributes;
    HANDLE handle;
    DWORD mode;
#endif
};

static struct console_info_s console_stdout, console_stderr;
static const struct console_info_s console_file = {
#ifdef _WIN32
    true, false, false, 0, INVALID_HANDLE_VALUE, 0
#else
    true, false
#endif
};
#ifdef _WIN32
static bool use_ansi;
static int old_attributes, current_attributes;
static HANDLE console_handle = INVALID_HANDLE_VALUE;
static DWORD console_mode;
#endif

enum terminal_e {
    TERMINAL_UNKNOWN, TERMINAL_OK, TERMINAL_DUMB
};

static enum terminal_e terminal = TERMINAL_UNKNOWN;

static bool terminal_detect(void) {
    if (terminal == TERMINAL_UNKNOWN) {
        char const *term = getenv("TERM");
        terminal = (term != NULL && strcmp(term, "dumb") != 0) ? TERMINAL_OK : TERMINAL_DUMB;
    }
    return terminal == TERMINAL_OK;
}

static bool console_known(FILE *f) {
    const struct console_info_s *console;

    if (f == stderr) {
        console = &console_stderr;
    } else if (f == stdout) {
        console = &console_stdout;
    } else {
        console = &console_file;
    }
    if (console->known) {
        console_use_color = console->color;
#ifdef _WIN32
        use_ansi = console->use_ansi;
        old_attributes = current_attributes = console->attributes;
        console_handle = console->handle;
        console_mode = console->mode;
#endif
        return true;
    }
    return false;
}

static void console_remember(FILE *f) {
    struct console_info_s *console;
    if (f == stderr) {
        console = &console_stderr;
    } else {
        console = &console_stdout;
    }
    console->color = console_use_color;
#ifdef _WIN32
    console->use_ansi = use_ansi;
    console->attributes = old_attributes;
    console->handle = console_handle;
    console->mode = console_mode;
#endif
    console->known = true;
}

#ifdef _WIN32
static UINT old_consoleoutputcp;
static UINT old_consolecp;

void console_init(unsigned int ansicp) {
    UINT consoleoutputcp = GetConsoleOutputCP();
    UINT consolecp = GetConsoleCP();
    old_consoleoutputcp = 0;
    old_consolecp = 0;
    if (consolecp != ansicp) {
        if (SetConsoleCP(ansicp)) old_consolecp = consolecp;
    }
    if (consoleoutputcp != ansicp) {
        if (SetConsoleOutputCP(ansicp)) old_consoleoutputcp = consoleoutputcp;
    }
    console_stdout.known = false;
    console_stderr.known = false;
}

static void console_destroy2(const struct console_info_s *console) {
    if (!console->known) return;
    if (console->handle == INVALID_HANDLE_VALUE) return;
    if (console->attributes >= 0) {
        SetConsoleTextAttribute(console->handle, (WORD)console->attributes);
    }
    if ((console->mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0) {
        SetConsoleMode(console->handle, console->mode ^ ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    }
}

void console_destroy(void) {
    if (old_consolecp != 0) SetConsoleCP(old_consolecp);
    if (old_consoleoutputcp != 0) SetConsoleOutputCP(old_consoleoutputcp);
    console_destroy2(&console_stdout);
    console_destroy2(&console_stderr);
}

static bool is_pty_name(const WCHAR *name, DWORD len) {
    DWORD i;
    int s = 0;
    len /= 2;
    for (i = 0; i < len; i++) {
        wchar_t c = name[i];
        switch (s) {
        case 0: case 1: case 2: case 3: s = (c == L"-pty"[s]) ? s + 1 : 0; break;
        case 4: s = (c >= L'0' && c <= L'9') ? s + 1 : 0; break;
        case 5: if (c < L'0' || c > L'9') s = (c == L'-') ? s + 1 : 0; break;
        default: if (c == L'\\') s = 0; break;
        }
    }
    return s == 6;
}

static bool is_pipe_pty1(HANDLE *handle) {
    typedef enum eFILE_INFO_BY_HANDLE_CLASS { eFileNameInfo = 2 } eFILE_INFO_BY_HANDLE_CLASS;
    typedef BOOL (WINAPI *lpGetFileInformationByHandleEx)(HANDLE, eFILE_INFO_BY_HANDLE_CLASS, LPVOID, DWORD);
    static lpGetFileInformationByHandleEx fnGetFileInformationByHandleEx;
    static bool unavailable;
    if (unavailable) {
        return false;
    }
    if (fnGetFileInformationByHandleEx == NULL) {
        HMODULE mhandle = GetModuleHandle("kernel32.dll");
        if (mhandle != NULL) {
            fnGetFileInformationByHandleEx = (lpGetFileInformationByHandleEx)GetProcAddress(mhandle, "GetFileInformationByHandleEx");
        }
    }
    if (fnGetFileInformationByHandleEx != NULL) {
        struct { DWORD length; WCHAR name[200]; } fni;
        if (fnGetFileInformationByHandleEx(handle, eFileNameInfo, &fni, sizeof fni) != 0) {
            return is_pty_name(fni.name, fni.length);
        }
    } else unavailable = true;
    return false;
}

static bool is_pipe_pty2(HANDLE *handle) {
    typedef enum eOBJECT_INFORMATION_CLASS { eObjectNameInformation = 1 } eOBJECT_INFORMATION_CLASS;
    typedef LONG (NTAPI *lpNtQueryObject)(HANDLE, eOBJECT_INFORMATION_CLASS, PVOID, ULONG, PULONG);
    static lpNtQueryObject fnNtQueryObject;
    static bool unavailable;
    if (unavailable) {
        return false;
    }
    if (fnNtQueryObject == NULL) {
        HANDLE mhandle = GetModuleHandle("ntdll.dll");
        if (mhandle != NULL) {
            fnNtQueryObject = (lpNtQueryObject)GetProcAddress(mhandle, "NtQueryObject");
        }
    }
    if (fnNtQueryObject != NULL) {
        DWORD len;
        struct { USHORT Length; USHORT MaximumLength; PWSTR Buffer; WCHAR pad[200]; } oni;
        if (fnNtQueryObject(handle, eObjectNameInformation, &oni, sizeof oni, &len) == 0) {
            return is_pty_name(oni.Buffer, oni.Length);
        }
    } else unavailable = true;
    return false;
}

static bool console_detect(FILE *f) {
    DWORD mode;
    CONSOLE_SCREEN_BUFFER_INFO console_info;

    use_ansi = false;
    old_attributes = current_attributes = -1;
    console_handle = INVALID_HANDLE_VALUE;
    console_mode = 0;

    console_handle = GetStdHandle(f == stderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
    if (console_handle == INVALID_HANDLE_VALUE) {
        return false;
    }
    mode = 0;
    if (GetConsoleMode(console_handle, &mode) == 0) {
        if (GetFileType(console_handle) == FILE_TYPE_PIPE) {
            if (is_pipe_pty1(console_handle) || is_pipe_pty2(console_handle)) {
                use_ansi = true;
                return true;
            }
        }
        return false;
    }
    if ((mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) == 0) {
        if (SetConsoleMode(console_handle, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0) {
            console_mode = mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            mode = 0;
            if (GetConsoleMode(console_handle, &mode) == 0) mode = 0;
        }
    }
    if ((mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0) {
        use_ansi = true;
        return true;
    }
    use_ansi = terminal_detect();
    if (use_ansi) {
        return true;
    }
    if (GetConsoleScreenBufferInfo(console_handle, &console_info)) {
        old_attributes = current_attributes = console_info.wAttributes;
        return true;
    }
    return false;
}

void console_use(FILE *f) {
    if (console_known(f)) return;
    console_use_color = console_detect(f);
    console_remember(f);
}

static const char *const ansi_sequences[8] = {
    "\33[1m", "\33[0;1m", "\33[7m", "\33[m", "\33[36m", "\33[31m", "\33[1;32m",
    "\33[35m"
};

void console_attribute(int c, FILE *f) {
    if (use_ansi) {
        fputs(ansi_sequences[c], f);
        return;
    }
    if (console_handle == INVALID_HANDLE_VALUE) {
        return;
    }
    if (fflush(f) != 0) setvbuf(f, NULL, _IOLBF, 1024);
    switch (c) {
    case 0: current_attributes |= FOREGROUND_INTENSITY; break;
    case 1: current_attributes = old_attributes | FOREGROUND_INTENSITY; break;
    case 2:
        if (!(current_attributes & FOREGROUND_BLUE) != !(current_attributes & BACKGROUND_BLUE)) current_attributes ^= FOREGROUND_BLUE | BACKGROUND_BLUE;
        if (!(current_attributes & FOREGROUND_GREEN) != !(current_attributes & BACKGROUND_GREEN)) current_attributes ^= FOREGROUND_GREEN | BACKGROUND_GREEN;
        if (!(current_attributes & FOREGROUND_RED) != !(current_attributes & BACKGROUND_RED)) current_attributes ^= FOREGROUND_RED | BACKGROUND_RED;
        if (!(current_attributes & FOREGROUND_INTENSITY) != !(current_attributes & BACKGROUND_INTENSITY)) current_attributes ^= FOREGROUND_INTENSITY | BACKGROUND_INTENSITY;
        break;
    case 3: current_attributes = old_attributes; break;
    case 4: current_attributes = FOREGROUND_GREEN | FOREGROUND_BLUE | (current_attributes & ~FOREGROUND_RED); break;
    case 5: current_attributes = FOREGROUND_RED | (current_attributes & ~(FOREGROUND_BLUE | FOREGROUND_GREEN)); break;
    case 6: current_attributes = FOREGROUND_GREEN | FOREGROUND_INTENSITY | (current_attributes & ~(FOREGROUND_BLUE | FOREGROUND_RED)); break;
    case 7: current_attributes = FOREGROUND_RED | FOREGROUND_BLUE | (current_attributes & ~FOREGROUND_GREEN); break;
    default: break;
    }
    SetConsoleTextAttribute(console_handle, (WORD)current_attributes);
}
#else
void console_init(void) {
    console_stdout.known = false;
    console_stderr.known = false;
}

void console_use(FILE *f) {
    if (console_known(f)) return;
    console_use_color = terminal_detect() && isatty(f == stderr ? STDERR_FILENO : STDOUT_FILENO) == 1;
    console_remember(f);
}
#endif

#endif
