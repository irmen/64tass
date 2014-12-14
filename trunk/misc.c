/*
    $Id$

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

#define _MISC_C_
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "misc.h"
#include <string.h>
#include "64tass.h"
#include "opcodes.h"
#include "getopt.h"
#include "section.h"
#include "encoding.h"
#include "file.h"
#include "eval.h"
#include "variables.h"
#include "ternary.h"
#include "unicode.h"

#include "codeobj.h"

struct arguments_s arguments={1,1,0,1,1,0,0,0,0,"a.out",&c6502,NULL,NULL, OUTPUT_CBM, 8};

/* --------------------------------------------------------------------------- */
int str_hash(const str_t *s) {
    size_t l = s->len;
    const uint8_t *s2 = s->data;
    unsigned int h;
    if (!l) return 0;
    h = *s2 << 7;
    while (l--) h = (1000003 * h) ^ *s2++;
    h ^= s->len;
    return h & ((~(unsigned int)0) >> 1);
}

int str_cmp(const str_t *s1, const str_t *s2) {
    if (s1->len != s2->len) return s1->len - s2->len;
    if (s1->data == s2->data) return 0;
    return memcmp(s1->data, s2->data, s1->len);
}

void str_cfcpy(str_t *s1, const str_t *s2) {
    size_t i, l;
    const uint8_t *d;
    static str_t cache;
    if (!s2) {
        if (s1) {
            if (s1->len != cache.len) {
                s1->data = (uint8_t *)realloc((uint8_t *)s1->data, s1->len);
                if (!s1->data) err_msg_out_of_memory();
            }
        } else free((uint8_t *)cache.data);
        memset(&cache, 0, sizeof(cache));
        return;
    }
    l = s2->len; d = s2->data;
    if (arguments.casesensitive) {
        for (i = 0; i < l; i++) {
            if (d[i] & 0x80) {
                unfkc(&cache, s2, 0);
                s1->len = cache.len;
                s1->data = cache.data;
                return;
            }
        }
        s1->len = l;
        s1->data = d;
        return;
    }
    for (i = 0; i < l; i++) {
        uint8_t *s, ch = d[i];
        if (ch < 'A' || (ch > 'Z' && ch < 0x80)) continue;
        if (ch & 0x80) {
            unfkc(&cache, s2, 1);
            s1->len = cache.len;
            s1->data = cache.data;
            return;
        }
        if (l > cache.len) {
            cache.data = (uint8_t *)realloc((uint8_t *)cache.data, l);
            if (!cache.data) err_msg_out_of_memory();
            cache.len = l;
        }
        s = (uint8_t *)cache.data;
        if (i) memcpy(s, d, i);
        s1->data = s;
        for (; i < l; i++) {
            ch = d[i];
            if (ch < 'A') {
                s[i] = ch;
                continue;
            }
            if (ch <= 'Z') {
                s[i] = ch | 0x20;
                continue;
            }
            if (ch & 0x80) {
                unfkc(&cache, s2, 1);
                s1->len = cache.len;
                s1->data = cache.data;
                return;
            }
            s[i] = ch;
        }
        s1->len = l;
        return;
    }
    s1->len = l;
    s1->data = d;
    return;
}

void str_cpy(str_t *s1, const str_t *s2) {
    s1->len = s2->len;
    if (s2->data) {
        uint8_t *s = (uint8_t *)malloc(s2->len);
        if (!s) err_msg_out_of_memory();
        memcpy(s, s2->data, s2->len);
        s1->data = s;
    } else s1->data = NULL;
}

void tfree(void) {
    destroy_eval();
    destroy_variables();
    destroy_section();
    destroy_file();
    err_destroy();
    destroy_encoding();
    destroy_values();
    destroy_ternary();
    unfc(NULL);
    unfkc(NULL, NULL, 0);
    str_cfcpy(NULL, NULL);
}

void tinit(void) {
    objects_init();
    err_init();
    init_section();
    init_file();
    init_values();
    init_variables();
    init_eval();
    init_ternary();
}

/* ------------------------------------------------------------------ */
static const char *short_options= "wqnbfXaTCBicxtel:L:I:msV?o:D:";

static const struct option long_options[]={
    {"no-warn"          , no_argument      , 0, 'w'},
    {"quiet"            , no_argument      , 0, 'q'},
    {"nonlinear"        , no_argument      , 0, 'n'},
    {"nostart"          , no_argument      , 0, 'b'},
    {"flat"             , no_argument      , 0, 'f'},
    {"long-address"     , no_argument      , 0, 'X'},
    {"atari-xex"        , no_argument      , 0,  0x107},
    {"apple-ii"         , no_argument      , 0,  0x108},
    {"ascii"            , no_argument      , 0, 'a'},
    {"tasm-compatible"  , no_argument      , 0, 'T'},
    {"case-sensitive"   , no_argument      , 0, 'C'},
    {"long-branch"      , no_argument      , 0, 'B'},
    {"m65xx"            , no_argument      , 0,  0x101},
    {"m6502"            , no_argument      , 0, 'i'},
    {"m65c02"           , no_argument      , 0, 'c'},
    {"m65ce02"          , no_argument      , 0,  0x106},
    {"m65816"           , no_argument      , 0, 'x'},
    {"m65dtv02"         , no_argument      , 0, 't'},
    {"m65el02"          , no_argument      , 0, 'e'},
    {"mr65c02"          , no_argument      , 0,  0x104},
    {"mw65c02"          , no_argument      , 0,  0x105},
    {"labels"           , required_argument, 0, 'l'},
    {"list"             , required_argument, 0, 'L'},
    {""                 , required_argument, 0, 'I'},
    {"no-monitor"       , no_argument      , 0, 'm'},
    {"no-source"        , no_argument      , 0, 's'},
    {"tab-size"         , required_argument, 0,  0x109},
    {"version"          , no_argument      , 0, 'V'},
    {"usage"            , no_argument      , 0,  0x102},
    {"help"             , no_argument      , 0,  0x103},
    { 0, 0, 0, 0}
};

int testarg(int argc,char *argv[], struct file_s *fin) {
    int opt, longind, tab;
    size_t max_lines = 0, fp = 0;

    while ((opt = getopt_long(argc, argv, short_options, long_options, &longind)) != -1) {
        switch (opt) {
            case 'w':arguments.warning=0;break;
            case 'q':arguments.quiet=0;break;
            case 'X':arguments.longaddr=1;break;
            case 'n':arguments.output_mode = OUTPUT_NONLINEAR;break;
            case 0x107:arguments.output_mode = OUTPUT_XEX;break;
            case 0x108:arguments.output_mode = OUTPUT_APPLE;break;
            case 'b':arguments.output_mode = OUTPUT_RAW;break;
            case 'f':arguments.output_mode = OUTPUT_FLAT;break;
            case 'a':arguments.toascii=1;break;
            case 'T':arguments.tasmcomp=1;break;
            case 'o':arguments.output=optarg;break;
            case 'D':
                {
                    size_t len = strlen(optarg) + 1;

                    if (fin->lines >= max_lines) {
                        max_lines += 1024;
                        fin->line = (size_t *)realloc(fin->line, max_lines * sizeof(fin->line[0]));
                        if (!fin->line || max_lines < 1024) err_msg_out_of_memory(); /* overflow */
                    }
                    fin->line[fin->lines++] = fp;

                    if (len < 1 || fp + len < len) err_msg_out_of_memory();
                    if (fp + len > fin->len) {
                        fin->len = fp + len + 1024;
                        fin->data=(uint8_t*)realloc(fin->data, fin->len);
                        if (!fin->data || fin->len < 1024) err_msg_out_of_memory();
                    }
                    memcpy(fin->data + fp, optarg, len);
                    fp += len;
                }
                break;
            case 'B':arguments.longbranch=1;break;
            case 0x101:arguments.cpumode = &c6502;break;
            case 'i':arguments.cpumode = &c6502i;break;
            case 'c':arguments.cpumode = &c65c02;break;
            case 0x106:arguments.cpumode = &c65ce02;break;
            case 'x':arguments.cpumode = &w65816;break;
            case 't':arguments.cpumode = &c65dtv02;break;
            case 'e':arguments.cpumode = &c65el02;break;
            case 0x104:arguments.cpumode = &r65c02;break;
            case 0x105:arguments.cpumode = &w65c02;break;
            case 'l':arguments.label=optarg;break;
            case 'L':arguments.list=optarg;break;
            case 'I':include_list_add(optarg);break;
            case 'm':arguments.monitor=0;break;
            case 's':arguments.source=0;break;
            case 'C':arguments.casesensitive=1;break;
            case 0x109:tab = atoi(optarg); if (tab > 0 && tab <= 64) arguments.tab_size = tab; break;
            case 0x102:puts(
             /* 12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
               "Usage: 64tass [-abBCfnTqwWcitxmse?V] [-D <label>=<value>] [-o <file>]\n"
               "        [-l <file>] [-L <file>] [-I <path>] [--ascii] [--nostart]\n"
               "        [--long-branch] [--case-sensitive] [--flat] [--atari-xex] [--apple-ii]\n"
               "        [--nonlinear] [--tasm-compatible] [--quiet] [--no-warn] [--long-address]\n"
               "        [--m65c02] [--m6502] [--m65xx] [--m65dtv02] [--m65816] [--m65el02]\n"
               "        [--mr65c02] [--mw65c02] [--m65ce02] [--labels=<file>] [--list=<file>]\n"
               "        [--no-monitor] [--no-source] [--tab-size=<value>] [--help] [--usage]\n"
               "        [--version] SOURCES");
                   return 0;

            case 'V':puts("64tass Turbo Assembler Macro V" VERSION);
                     return 0;
            case 0x103:
            case '?':if (optopt == '?' || opt == 0x103) { puts(
               "Usage: 64tass [OPTIONS...] SOURCES\n"
               "64tass Turbo Assembler Macro V" VERSION "\n"
               "\n"
               "  -a, --ascii           Source is not in PETASCII\n"
               "  -B, --long-branch     Automatic bxx *+3 jmp $xxxx\n"
               "  -C, --case-sensitive  Case sensitive labels\n"
               "  -D <label>=<value>    Define <label> to <value>\n"
               "  -I <path>             Include search path\n"
               "  -q, --quiet           Display errors/warnings\n"
               "  -T, --tasm-compatible Enable TASM compatible mode\n"
               "  -w, --no-warn         Suppress warnings\n"
               "\n"
               " Output selection:\n"
               "  -o <file>             Place output into <file>\n"
               "  -b, --nostart         Strip starting address\n"
               "  -f, --flat            Generate flat output file\n"
               "  -n, --nonlinear       Generate nonlinear output file\n"
               "  -X, --long-address    Use 3 byte start/len address\n"
               "      --atari-xex       Output Atari XEX file\n"
               "      --apple-ii        Output Apple II file\n"
               "\n"
               " Target selection:\n"
               "  -c, --m65c02          CMOS 65C02\n"
               "      --m65ce02         CSG 65CE02\n"
               "  -e, --m65el02         65EL02\n"
               "  -i, --m6502           NMOS 65xx\n"
               "      --m65xx           Standard 65xx (default)\n"
               "  -t, --m65dtv02        65DTV02\n"
               "  -x, --m65816          W65C816\n"
               "      --mr65c02         R65C02\n"
               "      --mw65c02         W65C02\n"
               "\n"
               " Source listing:\n"
               "  -l, --labels=<file>   List labels into <file>\n"
               "  -L, --list=<file>     List into <file>\n"
               "  -m, --no-monitor      Don't put monitor code into listing\n"
               "  -s, --no-source       Don't put source code into listing\n"
               "      --tab-size=<n>    Override the default tab size (8)\n"
               "\n"
               " Misc:\n"
               "  -?, --help            Give this help list\n"
               "      --usage           Give a short usage message\n"
               "  -V, --version         Print program version\n"
               "\n"
               "Mandatory or optional arguments to long options are also mandatory or optional\n"
               "for any corresponding short options.\n"
               "\n"
               "Report bugs to <soci" "\x40" "c64.rulez.org>.");
               return 0;
            }
            default:fputs("Try `64tass --help' or `64tass --usage' for more information.\n", stderr);
                    return -1;
        }
    }
    if (fin->lines != max_lines) {
        fin->line = (size_t *)realloc(fin->line, fin->lines * sizeof(fin->line[0]));
        if (!fin->lines) err_msg_out_of_memory();
    }
    closefile(fin);
    if (fp != fin->len) {
        fin->len = fp;
        if (fin->len) {
            fin->data = (uint8_t*)realloc(fin->data, fin->len);
            if (!fin->data) err_msg_out_of_memory();
        }
    }
    fin->coding = E_UTF8;
    if (argc <= optind) {
        fputs("Usage: 64tass [OPTIONS...] SOURCES\n"
              "Try `64tass --help' or `64tass --usage' for more information.\n", stderr);
        return -1;
    }
    return optind;
}
