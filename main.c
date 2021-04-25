/*
    Turbo Assembler 6502/65C02/65816/DTV
    $Id: main.c 2620 2021-04-25 12:05:16Z soci $

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
#include <string.h>
#include <signal.h>
#if defined _MSC_VER || defined __VBCC__ || defined __WATCOMC__
#define alarm(a)
#elif defined __MINGW32__
extern unsigned int alarm(unsigned int);
#else
#include <unistd.h>
#endif

#include "wchar.h"
#include "error.h"
#include "unicode.h"
#include "console.h"

static void signal_handler(int signum) {
#if defined _POSIX_C_SOURCE || _POSIX_VERSION >= 199506L
    struct sigaction sa;
    sa.sa_handler = SIG_DFL;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
    sigaction(signum, &sa, NULL);
#else
    signal(signum, SIG_DFL);
#endif
    signal_received = true;
    alarm(1);
}

static inline void install_signal_handler(void) {
#if defined _POSIX_C_SOURCE || _POSIX_VERSION >= 199506L
    struct sigaction sa, osa;
    sa.sa_handler = signal_handler;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGINT, NULL, &osa) == 0 && osa.sa_handler != SIG_IGN) {
        sigaction(SIGINT, &sa, NULL);
    }
    if (sigaction(SIGTERM, NULL, &osa) == 0 && osa.sa_handler != SIG_IGN) {
        sigaction(SIGTERM, &sa, NULL);
    }
    if (sigaction(SIGPIPE, NULL, &osa) == 0 && osa.sa_handler != SIG_IGN) {
        sa.sa_handler = SIG_IGN;
        sigaction(SIGPIPE, &sa, NULL);
    }
#else
    if (signal(SIGINT, signal_handler) == SIG_IGN) signal(SIGINT, SIG_IGN);
    if (signal(SIGTERM, signal_handler) == SIG_IGN) signal(SIGTERM, SIG_IGN);
#ifdef SIGPIPE
    signal(SIGPIPE, SIG_IGN);
#endif
#endif
}

#ifdef _WIN32
static const wchar_t *prgname(const wchar_t *name) {
    const wchar_t *p = name;
    while (*p != 0) p++;
    while (p != name) {
        p--;
        if (*p == '/' || *p == '\\' || *p == ':') return p + 1;
    }
    return p;
}

#ifdef __MINGW32__
static 
#endif
int wmain(int argc, wchar_t *argv2[]) {
    int i, r;
    char **argv;

    install_signal_handler();
    console_init();
    atexit(console_destroy);

    argv = (char **)malloc((argc < 1 ? 1 : (unsigned int)argc) * sizeof *argv);
    if (argv == NULL) err_msg_out_of_memory2();
    for (i = 0; i < argc; i++) {
        uchar_t c = 0, lastchar;
        const wchar_t *s = (i == 0) ? prgname(*argv2) : argv2[i];
        const wchar_t *p = s;
        uint8_t *c2;

        while (*p != 0) p++;
        c2 = (uint8_t *)malloc((size_t)(p - s) * 4 / (sizeof *p) + 1);
        if (c2 == NULL) err_msg_out_of_memory2();
        p = s;
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
            if (c != 0 && c < 0x80) *c2++ = (uint8_t)c; else c2 += utf8out(c, c2);
        }
        *c2++ = 0;
        argv[i] = (char *)realloc(argv[i], (size_t)((char *)c2 - argv[i]));
        if (argv[i] == NULL) err_msg_out_of_memory2();
    }
    if (argc < 1) {
        argv[0] = (char *)malloc(7);
        if (argv[0] == NULL) err_msg_out_of_memory2();
        memcpy(argv[0], "64tass", 7);
        argc = 1;
    }
    r = main2(&argc, &argv);

    for (i = 0; i < argc; i++) free(argv[i]);
    free(argv);
    return r;
}

#ifdef __MINGW32__
#include <windows.h>
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

#else /* _WIN32 */
#include <locale.h>
#include <unistd.h>
#if _POSIX_VERSION >= 200112L
#include <sys/resource.h>
#endif

static const char *prgname(const char *name) {
    const char *newp = strrchr(name, '/');
    if (newp != NULL) return newp + 1;
#if defined _WIN32 || defined __WIN32__ || defined __MSDOS__ || defined __DOS__
    newp = strrchr(name, '\\');
    if (newp != NULL) return newp + 1;
    newp = strrchr(name, ':');
    if (newp != NULL) return newp + 1;
#endif
    return name;
}

int main(int argc, char *argv[]) {
    int i, r;
    char **uargv;

#if _POSIX_VERSION >= 200112L
    struct rlimit rlim;
    if (getrlimit(RLIMIT_DATA, &rlim) == 0) {
        if (rlim.rlim_cur > 2147483647) rlim.rlim_cur = 2147483647;
        if (rlim.rlim_max > 2147483647) rlim.rlim_max = 2147483647;
        setrlimit(RLIMIT_DATA, &rlim);
    }
#endif

    install_signal_handler();
    setlocale(LC_CTYPE, "");

    uargv = (char **)malloc((argc < 1 ? 1 : (unsigned int)argc) * sizeof *uargv);
    if (uargv == NULL) err_msg_out_of_memory2();
    for (i = 0; i < argc; i++) {
        const char *s = (i == 0) ? prgname(*argv) : argv[i];
        mbstate_t ps;
        size_t p;
        size_t n = strlen(s), j = 0;
        size_t len = n + 64;
        uint8_t *data = (uint8_t *)malloc(len);
        if (data == NULL || len < 64) err_msg_out_of_memory2();

        memset(&ps, 0, sizeof ps);
        p = 0;
        for (;;) {
            ssize_t l;
            wchar_t w;
            uchar_t ch;
            if (p + 6*6 + 1 > len) {
                len += 1024;
                data = (uint8_t*)realloc(data, len);
                if (data == NULL) err_msg_out_of_memory2();
            }
            l = (ssize_t)mbrtowc(&w, s + j, n - j,  &ps);
            if (l < 1) {
                w = (uint8_t)s[j];
                if (w == 0 || l == 0) break;
                l = 1;
            }
            j += (size_t)l;
            ch = (uchar_t)w;
            if (ch != 0 && ch < 0x80) data[p++] = (uint8_t)ch; else p += utf8out(ch, data + p);
        }
        data[p] = 0;
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

