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

#include "codeobj.h"

struct arguments_s arguments={1,1,0,1,1,0,0,0,0,"a.out",OPCODES_C6502,NULL,NULL, OUTPUT_CBM};

const uint8_t whatis[256]={
    WHAT_EOL,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_HASHMARK,WHAT_EXPRESSION,WHAT_EXPRESSION,0,WHAT_EXPRESSION,WHAT_EXPRESSION,0,WHAT_STAR,WHAT_EXPRESSION,WHAT_COMA,WHAT_EXPRESSION,WHAT_COMMAND,0,
    WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,0,WHAT_COMMENT,WHAT_EXPRESSION,WHAT_EQUAL,WHAT_EXPRESSION,0,
    WHAT_EXPRESSION,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,
    WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_EXPRESSION,0,0,0,WHAT_LBL,
    0,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,
    WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

/* --------------------------------------------------------------------------- */
unsigned int utf8in(const uint8_t *c, uint32_t *out) { /* only for internal use with validated utf-8! */
    uint32_t ch;
    int i, j;
    ch = c[0];

    if (ch < 0xe0) {
        ch ^= 0xc0;i = 2;
    } else if (ch < 0xf0) {
        ch ^= 0xe0;i = 3;
    } else if (ch < 0xf8) {
        ch ^= 0xf0;i = 4;
    } else if (ch < 0xfc) {
        ch ^= 0xf8;i = 5;
    } else {
        ch ^= 0xfc;i = 6;
    }

    for (j = 1;j < i; j++) {
        ch = (ch << 6) ^ c[j] ^ 0x80;
    }
    *out = ch;
    return i;
}

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

int str_casehash(const str_t *s) {
    size_t l = s->len;
    const uint8_t *s2 = s->data;
    uint8_t c;
    unsigned int h;
    if (!l) return 0;
    c = *s2;
    if (c >= 'a' && c <= 'z') c -= 32;
    h = c << 7;
    while (l--) {
        c = *s2++;
        if (c >= 'a' && c <= 'z') c -= 32;
        h = (1000003 * h) ^ c;
    }
    h ^= s->len;
    return h & ((~(unsigned int)0) >> 1);
}

int str_cmp(const str_t *s1, const str_t *s2) {
    if (s1->len != s2->len) return s1->len - s2->len;
    if (s1->data == s2->data) return 0;
    if (!s1->data && s2->data) return -1;
    if (s1->data && !s2->data) return 1;
    return memcmp(s1->data, s2->data, s1->len);
}

int str_casecmp(const str_t *s1, const str_t *s2) {
    if (s1->len != s2->len) return s1->len - s2->len;
    if (s1->data == s2->data) return 0;
    if (!s1->data && s2->data) return -1;
    if (s1->data && !s2->data) return 1;
    return strncasecmp((const char *)s1->data, (const char *)s2->data, s1->len);
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

uint8_t *utf8out(uint32_t i, uint8_t *c) {
    if (i < 0x800) {
        *c++=0xc0 | (i >> 6);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x10000) {
        *c++=0xe0 | (i >> 12);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x200000) {
        *c++=0xf0 | (i >> 18);
        *c++=0x80 | ((i >> 12) & 0x3f);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x4000000) {
        *c++=0xf8 | (i >> 24);
        *c++=0x80 | ((i >> 18) & 0x3f);
        *c++=0x80 | ((i >> 12) & 0x3f);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i & ~0x7fffffff) return c;
    *c++=0xfc | (i >> 30);
    *c++=0x80 | ((i >> 24) & 0x3f);
    *c++=0x80 | ((i >> 18) & 0x3f);
    *c++=0x80 | ((i >> 12) & 0x3f);
    *c++=0x80 | ((i >> 6) & 0x3f);
    *c++=0x80 | (i & 0x3f);
    return c;
}

void tfree(void) {
    destroy_variables();
    destroy_section();
    destroy_file();
    err_destroy();
    destroy_encoding();
    destroy_values();
    destroy_ternary();
}

void tinit(void) {
    root_label.constant = 1;
    root_label.defpass = 1;
    root_label.usepass = 1;
    root_label.parent = NULL;
    root_label.name.data = NULL;
    root_label.name.len = 0;
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
    {"atari-xex"        , no_argument      , 0,   7},
    {"ascii"            , no_argument      , 0, 'a'},
    {"tasm-compatible"  , no_argument      , 0, 'T'},
    {"case-sensitive"   , no_argument      , 0, 'C'},
    {"long-branch"      , no_argument      , 0, 'B'},
    {"m65xx"            , no_argument      , 0,   1},
    {"m6502"            , no_argument      , 0, 'i'},
    {"m65c02"           , no_argument      , 0, 'c'},
    {"m65ce02"          , no_argument      , 0,   6},
    {"m65816"           , no_argument      , 0, 'x'},
    {"m65dtv02"         , no_argument      , 0, 't'},
    {"m65el02"          , no_argument      , 0, 'e'},
    {"mr65c02"          , no_argument      , 0,   4},
    {"mw65c02"          , no_argument      , 0,   5},
    {"labels"           , required_argument, 0, 'l'},
    {"list"             , required_argument, 0, 'L'},
    {""                 , required_argument, 0, 'I'},
    {"no-monitor"       , no_argument      , 0, 'm'},
    {"no-source"        , no_argument      , 0, 's'},
    {"version"          , no_argument      , 0, 'V'},
    {"usage"            , no_argument      , 0,   2},
    {"help"             , no_argument      , 0,   3},
    { 0, 0, 0, 0}
};

int testarg(int argc,char *argv[],struct file_s *fin) {
    int opt, longind;
    enum {UNKNOWN, UTF8, ISO1} type = UNKNOWN;

    while ((opt = getopt_long(argc, argv, short_options, long_options, &longind)) != -1)
        switch (opt)
        {
            case 'w':arguments.warning=0;break;
            case 'q':arguments.quiet=0;break;
            case 'X':arguments.longaddr=1;break;
            case 'n':arguments.output_mode = OUTPUT_NONLINEAR;break;
            case 7:arguments.output_mode = OUTPUT_XEX;break;
            case 'b':arguments.output_mode = OUTPUT_RAW;break;
            case 'f':arguments.output_mode = OUTPUT_FLAT;break;
            case 'a':arguments.toascii=1;break;
            case 'T':arguments.tasmcomp=1;break;
            case 'o':arguments.output=optarg;break;
            case 'D':
                {
                    const uint8_t *c = (uint8_t *)optarg;
                    uint8_t *p, ch2;
                    uint32_t ch = 0;

                    p = fin->data + fin->p; 
                    while ((ch=*c++)) {
                        int i, j;
                        if (p + 6*6 + 1 > fin->data + fin->len) {
                            size_t o = p - fin->data;
                            fin->len += 1024;
                            if (!(fin->data=(uint8_t*)realloc(fin->data, fin->len))) exit(1);
                            p = fin->data + o;
                        }
                        switch (type) {
                        case UNKNOWN:
                        case UTF8:
                            if (ch < 0x80) {
                                break;
                            } else if (ch < 0xc0) {
                                if (type == UNKNOWN) {
                                    type = ISO1; break;
                                }
                                ch = 0xfffd; i = 0;
                            } else if (ch < 0xe0) {
                                ch ^= 0xc0;i = 1;
                            } else if (ch < 0xf0) {
                                ch ^= 0xe0;i = 2;
                            } else if (ch < 0xf8) {
                                ch ^= 0xf0;i = 3;
                            } else if (ch < 0xfc) {
                                ch ^= 0xf8;i = 4;
                            } else if (ch < 0xfe) {
                                ch ^= 0xfc;i = 5;
                            } else {
                                type = ISO1; break;
                            }

                            for (j = i; i; i--) {
                                ch2 = *c++;
                                if (ch2 < 0x80 || ch2 >= 0xc0) {
                                    if (type == UNKNOWN) {
                                        type = ISO1;
                                        i = (j - i) * 6;
                                        p = utf8out(((~0x7f >> j) & 0xff) | (ch >> i), p);
                                        for (;i; i-= 6) {
                                            p = utf8out(((ch >> (i-6)) & 0x3f) | 0x80, p);
                                        }
                                        ch = ch2; j = 0;
                                        break;
                                    }
                                    c--;
                                    ch = 0xfffd;break;
                                }
                                ch = (ch << 6) ^ ch2 ^ 0x80;
                            }
                            if (j) type = UTF8;
                            if (ch == 0xfeff) continue;
                        case ISO1:
                            break;
                        }
                        if (ch && ch < 0x80) *p++ = ch; else p = utf8out(ch, p);
                    }
                    if (p >= fin->data + fin->len) {
                        size_t o = p - fin->data;
                        fin->len += 1024;
                        if (!(fin->data=(uint8_t*)realloc(fin->data, fin->len))) exit(1);
                        p = fin->data + o;
                    }
                    *p++ = 0;
                    fin->p = p - fin->data;
                }
                break;
            case 'B':arguments.longbranch=1;break;
            case 1:arguments.cpumode=OPCODES_C6502;break;
            case 'i':arguments.cpumode=OPCODES_C6502I;break;
            case 'c':arguments.cpumode=OPCODES_C65C02;break;
            case 6:arguments.cpumode=OPCODES_C65CE02;break;
            case 'x':arguments.cpumode=OPCODES_C65816;break;
            case 't':arguments.cpumode=OPCODES_C65DTV02;break;
            case 'e':arguments.cpumode=OPCODES_C65EL02;break;
            case 4:arguments.cpumode=OPCODES_CR65C02;break;
            case 5:arguments.cpumode=OPCODES_CW65C02;break;
            case 'l':arguments.label=optarg;break;
            case 'L':arguments.list=optarg;break;
            case 'I':include_list_add(optarg);break;
            case 'm':arguments.monitor=0;break;
            case 's':arguments.source=0;break;
            case 'C':arguments.casesensitive=1;break;
            case 2:puts(
             /* 12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
               "Usage: 64tass [-abBCfnTqwWcitxmse?V] [-D <label>=<value>] [-o <file>]\n"
               "        [-l <file>] [-L <file>] [-I <path>] [--ascii] [--nostart]\n"
               "        [--long-branch] [--case-sensitive] [--flat] [--atari-xex] [--nonlinear]\n"
               "        [--tasm-compatible] [--quiet] [--no-warn] [--long-address] [--m65c02]\n"
               "        [--m6502] [--m65xx] [--m65dtv02] [--m65816] [--m65el02] [--mr65c02]\n"
               "        [--mw65c02] [--m65ce02] [--labels=<file>] [--list=<file>]\n"
               "        [--no-monitor] [--no-source] [--help] [--usage] [--version]\n"
               "        SOURCES");
                   destroy_eval();
                   tfree();
                   exit(0);

            case 'V':puts("64tass Turbo Assembler Macro V" VERSION);
                     destroy_eval();
                     tfree();
                     exit(0);
            case 3:
            case '?':if (optopt=='?' || opt==3) { puts(
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
               destroy_eval();
               tfree();
               exit(0);
            }
            default:fputs("Try `64tass --help' or `64tass --usage' for more information.\n", stderr);
                    destroy_eval();
                    tfree();
                    exit(1);
        }
    closefile(fin); fin->len = fin->p; fin->p = 0;
    if (fin->data && !(fin->data=(uint8_t*)realloc(fin->data, fin->len))) exit(1);
    if (argc <= optind) {
        fputs("Usage: 64tass [OPTIONS...] SOURCES\n"
              "Try `64tass --help' or `64tass --usage' for more information.\n", stderr);
        destroy_eval();
        tfree();
        exit(1);
    }
    return optind;
}
