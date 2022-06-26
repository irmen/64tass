/*
    Turbo Assembler 6502/65C02/65816/DTV
    $Id: main.c 2786 2022-05-25 04:08:01Z soci $

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

#include "main.h"
#include <string.h>
#include <signal.h>
#ifdef SIGALRM
#include <unistd.h>
#endif

#include "wchar.h"
#include "error.h"
#include "unicode.h"
#include "console.h"
#include "64tass.h"

bool signal_received = false;

static void signal_reset(int signum) {
#if defined _POSIX_C_SOURCE || _POSIX_VERSION >= 199506L
#ifdef SA_RESETHAND
    (void)signum;
#else
    struct sigaction sa;
    sa.sa_handler = SIG_DFL;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
    sigaction(signum, &sa, NULL);
#define SA_RESETHAND 0
#endif
#else
    signal(signum, SIG_DFL);
#endif
}

static void signal_set(int signum, void (*handler)(int)) {
#if defined _POSIX_C_SOURCE || _POSIX_VERSION >= 199506L
    struct sigaction sa, osa;
    sa.sa_handler = handler;
    sa.sa_flags = (int)SA_RESETHAND;
    sigemptyset(&sa.sa_mask);
    if (sigaction(signum, NULL, &osa) == 0 && osa.sa_handler != SIG_IGN) {
        sigaction(signum, &sa, NULL);
    }
#else
    if (signal(signum, handler) == SIG_IGN) signal(signum, SIG_IGN);
#endif
}

static void signal_handler(int signum) {
#ifdef SIGALRM
    static int signal_number;
    signal_reset(signum);
    if (signum == SIGALRM) {
        if (raise(signal_number) != 0) abort();
    }
    signal_set(SIGALRM, signal_handler);
    signal_number = signum;
    alarm(1);
#else
    signal_reset(signum);
#endif
    signal_received = true;
}

static inline void install_signal_handler(void) {
    signal_set(SIGINT, signal_handler);
    signal_set(SIGTERM, signal_handler);
#ifdef SIGPIPE
    signal_set(SIGPIPE, SIG_IGN);
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

    if (argc < 1) {
        static wchar_t *argvd[1] = { (wchar_t *)L"64tass" };
        argv2 = argvd;
        argc = 1;
    }
    new_array(&argv, (unsigned int)argc);
    for (i = 0; i < argc; i++) {
        unichar_t c = 0, lastchar;
        const wchar_t *s = (i == 0) ? prgname(*argv2) : argv2[i];
        const wchar_t *p = s;
        uint8_t *c2;
        size_t l;

        while (*p != 0) p++;
        l = (size_t)(p - s) * 4 / (sizeof *p) + 1;
        new_array(&c2, l);
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
        l = (size_t)((char *)c2 - argv[i]);
        resize_array(&argv[i], l);
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
  int result, argcw = 0;
  LPWSTR *argvw = CommandLineToArgvW(commandLine, &argcw);
  if (argvw == NULL) return EXIT_FAILURE;
  result = wmain(argcw, argvw);
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

    if (argc < 1) {
        static char *argvd[1] = { (char *)"64tass" };
        argv = argvd;
        argc = 1;
    }
    new_array(&uargv, (unsigned int)argc);
    for (i = 0; i < argc; i++) {
        const char *s = (i == 0) ? prgname(*argv) : argv[i];
        mbstate_t ps;
        size_t p;
        size_t n = strlen(s), j = 0;
        size_t len;
        uint8_t *data;
        if (add_overflow(n, 64, &len)) err_msg_out_of_memory();
        new_array(&data, len);

        memset(&ps, 0, sizeof ps);
        p = 0;
        for (;;) {
            ssize_t l;
            wchar_t w;
            unichar_t ch;
            if (p + 6*6 + 1 > len) {
                if (add_overflow(n, 1024, &len)) err_msg_out_of_memory();
                resize_array(&data, len);
            }
            l = (ssize_t)mbrtowc(&w, s + j, n - j,  &ps);
            if (l < 1) {
                w = (uint8_t)s[j];
                if (w == 0 || l == 0) break;
                l = 1;
            }
            j += (size_t)l;
            ch = (unichar_t)w;
            if (ch != 0 && ch < 0x80) data[p++] = (uint8_t)ch; else p += utf8out(ch, data + p);
        }
        data[p] = 0;
        uargv[i] = (char *)data;
    }
    r = main2(&argc, &uargv);

    for (i = 0; i < argc; i++) free(uargv[i]);
    free(uargv);
    return r;
}
#endif

