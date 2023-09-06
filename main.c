/*
    Turbo Assembler 6502/65C02/65816/DTV
    $Id: main.c 3086 2023-09-03 06:23:08Z soci $

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
#include <locale.h>
#ifdef SIGALRM
#include <unistd.h>
#endif
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <shellapi.h>
#endif

#include "wchar.h"
#include "error.h"
#include "unicode.h"
#include "console.h"
#include "64tass.h"

bool signal_received = false;

#if defined _POSIX_C_SOURCE || _POSIX_VERSION >= 199506L
#ifdef SA_RESETHAND
#define signal_reset(signum) do {} while (false)
#else
static void signal_reset(int signum) {
    struct sigaction sa;
    sa.sa_handler = SIG_DFL;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
    sigaction(signum, &sa, NULL);
}
#define SA_RESETHAND 0
#endif
#else
#define signal_reset(signum) signal((signum), SIG_DFL)
#endif

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

static const char *prgname(const char *name) {
    const char *newp = name;
    while (*name != '\0') {
        char c = *name++;
#if defined _WIN32 || defined __MSDOS__ || defined __DOS__
        if (c == '/' || c == '\\' || c == ':') newp = name;
#else
        if (c == '/') newp = name;
#endif
    }
    return newp;
}

#ifdef _WIN32
static const wchar_t *wprgname(const wchar_t *name) {
    const wchar_t *newp = name;
    while (*name != '\0') {
        wchar_t c = *name++;
        if (c == L'/' || c == L'\\' || c == L':') newp = name;
    }
    return newp;
}

static unsigned int get_codepage(const char *locale) {
    unsigned int cp = 0;
    while (*locale != '\0' && *locale != '.') locale++;
    if (*locale == '.') {
        locale++;
        if (*locale >= '0' && *locale <= '9') {
            char *s;
            unsigned long int ncp = strtoul(locale, &s, 10);
            if (ncp > 0 && ncp <= 65535) {
                cp = (unsigned int)ncp;
                locale = s;
            }
        } else if ((*locale | 0x20) == 'u' && (locale[1] | 0x20) == 't' && (locale[2] | 0x20) == 'f') {
            locale += 3;
            if (*locale == '-') locale++;
            if (*locale == '8') {
                locale++;
                cp = CP_UTF8;
            }
        }
    }
    return (*locale == '\0' || *locale == '@') ? cp : 0;
}

static unsigned int locale_init(void) {
    char temp[16];
    const char *env = getenv("LC_ALL");
    unsigned int cp;
    if (env == NULL) env = getenv("LC_TYPE");
    if (env == NULL) env = getenv("LANG");
    if (env == NULL) env = ".ACP";
    if (strcmp(env, ".ACP") == 0) {
        cp = GetACP();
    } else if (strcmp(env, ".OCP") == 0) {
        cp = GetOEMCP();
    } else {
        cp = 0;
    }
    if (cp != 0) {
        sprintf(temp, ".%u", cp);
        env = temp;
    } else {
        cp = get_codepage(env);
        if (!IsValidCodePage(cp)) cp = 0;
    }
    env = setlocale(LC_CTYPE, env);
    if (cp == 0) {
        if (env == NULL) env = setlocale(LC_CTYPE, NULL);
        if (env != NULL) cp = get_codepage(env);
    }
    if (!IsValidCodePage(cp)) cp = 0;
    return cp != 0 ? cp : GetACP();
}

#ifdef __MINGW32__
static
#endif
int wmain(int argc, wchar_t *argv2[]) {
    int i, r;
    char **argv;

    install_signal_handler();
    codepage = locale_init();
    console_init(codepage);
    atexit(console_destroy);
    unicode_init();

    if (argc < 1) {
        static wchar_t *argvd[1] = { (wchar_t *)L"64tass" };
        argv2 = argvd;
        argc = 1;
    }
    new_array(&argv, (unsigned int)argc);
    for (i = 0; i < argc; i++) {
        unichar_t c = 0, lastchar;
        const wchar_t *s = (i == 0) ? wprgname(*argv2) : argv2[i];
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
int main(int argc, char *argv[])
{
  LPWSTR commandLine = GetCommandLineW();
  int result, argcw = 0;
  LPWSTR *argvw = CommandLineToArgvW(commandLine, &argcw);
  if (argvw == NULL) {
      int i, r;
      char **uargv;
      unsigned int cp;

      install_signal_handler();
      cp = locale_init();
      console_init(cp);
      atexit(console_destroy);
      unicode_init();

      if (argc < 1) {
          static char *argvd[1] = { (char *)"64tass" };
          argv = argvd;
          argc = 1;
      }
      new_array(&uargv, (unsigned int)argc);
      codepage = GetACP();
      for (i = 0; i < argc; i++) {
          const char *s = (i == 0) ? prgname(*argv) : argv[i];
          uint8_t *data;
          data = char_to_utf8(s);
          if (data == (uint8_t *)s) {
              size_t len = strlen(s);
              data = inc_overflow(&len, 1) ? NULL : allocate_array(uint8_t, len);
              if (data != NULL) {
                  memcpy(data, s, len);
              }
          }
          if (data == NULL) err_msg_out_of_memory();
          uargv[i] = (char *)data;
      }
      codepage = cp;
      r = main2(&argc, &uargv);

      for (i = 0; i < argc; i++) free(uargv[i]);
      free(uargv);
      return r;
  }
  result = wmain(argcw, argvw);
  LocalFree(argvw);
  return result;
}
#endif /* __MINGW32__ */

#else /* _WIN32 */
#include <unistd.h>
#if _POSIX_VERSION >= 200112L
#include <sys/resource.h>
#endif

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
    console_init();

    if (argc < 1) {
        static char *argvd[1] = { (char *)"64tass" };
        argv = argvd;
        argc = 1;
    }
    new_array(&uargv, (unsigned int)argc);
    for (i = 0; i < argc; i++) {
        const char *s = (i == 0) ? prgname(*argv) : argv[i];
        uint8_t *data = char_to_utf8(s);
        if (data == (uint8_t *)s) {
            size_t len = strlen(s);
            data = inc_overflow(&len, 1) ? NULL : allocate_array(uint8_t, len);
            if (data != NULL) {
                memcpy(data, s, len);
            }
        }
        if (data == NULL) err_msg_out_of_memory();
        uargv[i] = (char *)data;
    }
    r = main2(&argc, &uargv);

    for (i = 0; i < argc; i++) free(uargv[i]);
    free(uargv);
    return r;
}
#endif

