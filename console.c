/*
    $Id: console.c 2522 2021-03-14 20:16:55Z soci $

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
#include <unistd.h>

bool console_use_color = false;
bool console_use_bold = false;

enum terminal_e {
    TERMINAL_UNKNOWN, TERMINAL_OK, TERMINAL_DUMB
};

static enum terminal_e terminal = TERMINAL_UNKNOWN;

static bool terminal_detect(FILE *f) {
    int fd;
    if (f == stderr) {
        fd = STDERR_FILENO;
    } else if (f == stdout) {
        fd = STDOUT_FILENO;
    } else {
        return false;
    }
    if (terminal == TERMINAL_UNKNOWN) {
        char const *term = getenv("TERM");
        terminal = (term != NULL && strcmp(term, "dumb") != 0) ? TERMINAL_OK : TERMINAL_DUMB;
    }
    return terminal == TERMINAL_OK && isatty(fd) == 1;
}

#ifdef _WIN32
#include <windows.h>

static bool use_ansi;
static BOOL utf8_console;
static UINT old_consoleoutputcp;
static UINT old_consolecp;
static HANDLE console_handle;
static int old_attributes, current_attributes;

void console_init(void) {
    utf8_console = IsValidCodePage(CP_UTF8);
    if (utf8_console) {
        old_consoleoutputcp = GetConsoleOutputCP();
        old_consolecp = GetConsoleCP();
        SetConsoleCP(CP_UTF8);
        SetConsoleOutputCP(CP_UTF8);
    }
}

void console_destroy(void) {
    if (utf8_console) {
        SetConsoleCP(old_consolecp);
        SetConsoleOutputCP(old_consoleoutputcp);
    }
}

void console_use(FILE *f) {
    CONSOLE_SCREEN_BUFFER_INFO console_info;
    DWORD handle;

    use_ansi = terminal_detect(f);
    if (use_ansi) {
        console_use_color = true;
        return;
    }
    console_use_color = false;
    if (f == stderr) {
        handle = STD_ERROR_HANDLE;
    } else if (f == stdout) {
        handle = STD_OUTPUT_HANDLE;
    } else {
        return;
    }
    console_handle = GetStdHandle(handle);
    if (console_handle == INVALID_HANDLE_VALUE) {
        return;
    }
    if (GetConsoleScreenBufferInfo(console_handle, &console_info)) {
        old_attributes = current_attributes = console_info.wAttributes;
        console_use_color = true;
        return;
    }
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
    fflush(f);
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
void console_use(FILE *f) {
    console_use_color = terminal_detect(f);
}
#endif

#endif
