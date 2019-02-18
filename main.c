/*
    Turbo Assembler 6502/65C02/65816/DTV
    $Id: main.c 1866 2019-02-09 09:26:10Z soci $

    6502/65C02 Turbo Assembler  Version 1.3
    (c) 1996 Taboo Productions, Marek Matula

    6502/65C02 Turbo Assembler  Version 1.35  ANSI C port
    (c) 2000 BiGFooT/BReeZe^2000

    6502/65C02/65816/DTV Turbo Assembler  Version 1.4x
    (c) 2001-2014 Soci/Singular (soci@c64.rulez.org)

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

#include "64tass.h"
#ifdef _WIN32
#include <windows.h>
#include <wincon.h>
#endif
#include <locale.h>
#include "wchar.h"
#include <string.h>

#include "error.h"
#include "unicode.h"

#ifdef _WIN32
static UINT oldcodepage;
static UINT oldcodepage2;

static void myexit(void) {
    SetConsoleCP(oldcodepage2);
    SetConsoleOutputCP(oldcodepage);
}

static int wmain(int argc, wchar_t *argv2[]) {
    int i, r;
    char **argv;

    if (IsValidCodePage(CP_UTF8)) {
        oldcodepage = GetConsoleOutputCP();
        oldcodepage2 = GetConsoleCP();
        SetConsoleCP(CP_UTF8);
        SetConsoleOutputCP(CP_UTF8);
        atexit(myexit);
    }

    argv = (char **)malloc((argc < 1 ? 1 : argc) * sizeof *argv);
    if (argv == NULL) err_msg_out_of_memory2();
    for (i = 0; i < argc; i++) {
        uchar_t c = 0, lastchar;
        wchar_t *p = argv2[i];
        uint8_t *c2;

        while (*p != 0) p++;
        c2 = (uint8_t *)malloc((p - argv2[i]) * 4 / (sizeof *p) + 1);
        if (c2 == 0) err_msg_out_of_memory2();
        p = argv2[i];
        argv[i] = (char *)c2;

        while (*p != 0) {
            lastchar = c;
            c = *p++;
            if (c >= 0xd800 && c < 0xdc00) {
                if (lastchar < 0xd800 || lastchar >= 0xdc00) continue;
                c = 0xfffd;
            } else if (c >= 0xdc00 && c < 0xe000) {
                if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                    c ^= 0x360dc00 ^ (lastchar << 10);
                    c += 0x10000;
                } else
                    c = 0xfffd;
            } else if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                c = 0xfffd;
            }
            if (c != 0 && c < 0x80) *c2++ = c; else c2 = utf8out(c, c2);
        }
        *c2++ = 0;
        argv[i] = (char *)realloc(argv[i], (char *)c2 - argv[i]);
        if (argv[i] == NULL) err_msg_out_of_memory2();
    }
    if (argc < 1) {
        argv[0] = (char *)malloc(7);
        if (argv[0] == NULL) err_msg_out_of_memory2();
        strcpy(argv[0], "64tass");
        argc = 1;
    }
    r = main2(&argc, &argv);

    for (i = 0; i < argc; i++) free(argv[i]);
    free(argv);
    return r;
}
#else
int main(int argc, char *argv[]) {
    int i, r;
    char **uargv;

    setlocale(LC_ALL, "");
    setlocale(LC_NUMERIC, "C");

    uargv = (char **)malloc((argc < 1 ? 1 : (unsigned int)argc) * sizeof *uargv);
    if (uargv == NULL) err_msg_out_of_memory2();
    for (i = 0; i < argc; i++) {
        const char *s = argv[i];
        mbstate_t ps;
        uint8_t *p;
        size_t n = strlen(s), j = 0;
        size_t len = n + 64;
        uint8_t *data = (uint8_t *)malloc(len);
        if (data == NULL || len < 64) err_msg_out_of_memory2();

        memset(&ps, 0, sizeof ps);
        p = data;
        for (;;) {
            ssize_t l;
            wchar_t w;
            uchar_t ch;
            if (p + 6*6 + 1 > data + len) {
                size_t o = (size_t)(p - data);
                len += 1024;
                data = (uint8_t*)realloc(data, len);
                if (data == NULL) err_msg_out_of_memory2();
                p = data + o;
            }
            l = (ssize_t)mbrtowc(&w, s + j, n - j,  &ps);
            if (l < 1) {
                w = (uint8_t)s[j];
                if (w == 0 || l == 0) break;
                l = 1;
            }
            j += (size_t)l;
            ch = (uchar_t)w;
            if (ch != 0 && ch < 0x80) *p++ = (uint8_t)ch; else p = utf8out(ch, p);
        }
        *p++ = 0;
        uargv[i] = (char *)data;
    }
    if (argc < 1) {
        argv[0] = (char *)malloc(7);
        if (argv[0] == NULL) err_msg_out_of_memory2();
        memcpy(argv[0], "64tass", 7);
        argc = 1;
    }
    r = main2(&argc, &uargv);

    for (i = 0; i < argc; i++) free(uargv[i]);
    free(uargv);
    return r;
}
#endif


#ifdef __MINGW32__

#include <shellapi.h>

int main(void)
{
  LPWSTR commandLine = GetCommandLineW();
  int argcw = 0;
  LPWSTR *argvw = CommandLineToArgvW(commandLine, &argcw);
  if (!argvw)
    return EXIT_FAILURE;

  int result = wmain(argcw, argvw);
  LocalFree(argvw);
  return result;
}
#endif /* __MINGW32__ */
