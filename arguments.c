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

#include "arguments.h"
#include <string.h>
#include "64tass.h"
#include "opcodes.h"
#include "my_getopt.h"
#include "file.h"
#include "error.h"
#include "unicode.h"

struct arguments_s arguments = {
    true,        /* warning */
    true,        /* caret */
    true,        /* quiet */
    false,       /* toascii */
    true,        /* monitor */
    true,        /* source */
    false,       /* linenum */
    false,       /* longbranch */
    false,       /* longaddr */
    false,       /* tasmcomp */
    false,       /* verbose */
    0x20,        /* caseinsensitive */
    "a.out",     /* output */
    &c6502,      /* cpumode */
    NULL,        /* label */
    NULL,        /* list */
    NULL,        /* make */
    NULL,        /* error */
    OUTPUT_CBM,  /* output_mode */
    8,           /* tab_size */
    LABEL_64TASS /* label_mode */
};

struct diagnostics_s diagnostics = {
    false,       /* shadow */
    false,       /* strict_bool */
    false,       /* optimize */
    true,        /* implied_reg */
};

struct w_options_s {
    const char *name;
    bool *opt;
};

static const struct w_options_s w_options[] = {
    {"optimize",     &diagnostics.optimize},
    {"shadow",       &diagnostics.shadow},
    {"strict-bool",  &diagnostics.strict_bool},
    {"implied-reg",  &diagnostics.implied_reg},
    {NULL          , NULL}
};

static bool woption(const char *n, const char *s) {
    bool no = (s[0] == 'n') && (s[1] == 'o') && (s[2] == '-');
    const struct w_options_s *w = w_options;

    while (w->name != NULL) {
        if (!strcmp(w->name, no ? s + 3 : s)) {
            *w->opt = !no;
            return false;
        }
        w++;
    }
    printable_print((const uint8_t *)n, stderr);
    fputs(": unrecognized option '-W", stderr);
    printable_print((const uint8_t *)s, stderr);
    fputs("'\n", stderr);
    return true;
}

static const char *short_options = "wqnbfXaTCBicxtel:L:I:M:msV?o:D:E:W:";

static const struct my_option long_options[] = {
    {"no-warn"          , my_no_argument      , NULL, 'w'},
    {"quiet"            , my_no_argument      , NULL, 'q'},
    {"nonlinear"        , my_no_argument      , NULL, 'n'},
    {"nostart"          , my_no_argument      , NULL, 'b'},
    {"flat"             , my_no_argument      , NULL, 'f'},
    {"long-address"     , my_no_argument      , NULL, 'X'},
    {"atari-xex"        , my_no_argument      , NULL,  0x107},
    {"apple-ii"         , my_no_argument      , NULL,  0x108},
    {"intel-hex"        , my_no_argument      , NULL,  0x10e},
    {"s-record"         , my_no_argument      , NULL,  0x10f},
    {"cbm-prg"          , my_no_argument      , NULL,  0x10c},
    {"ascii"            , my_no_argument      , NULL, 'a'},
    {"tasm-compatible"  , my_no_argument      , NULL, 'T'},
    {"case-sensitive"   , my_no_argument      , NULL, 'C'},
    {"long-branch"      , my_no_argument      , NULL, 'B'},
    {"m65xx"            , my_no_argument      , NULL,  0x101},
    {"m6502"            , my_no_argument      , NULL, 'i'},
    {"m65c02"           , my_no_argument      , NULL, 'c'},
    {"m65ce02"          , my_no_argument      , NULL,  0x106},
    {"m65816"           , my_no_argument      , NULL, 'x'},
    {"m65dtv02"         , my_no_argument      , NULL, 't'},
    {"m65el02"          , my_no_argument      , NULL, 'e'},
    {"mr65c02"          , my_no_argument      , NULL,  0x104},
    {"mw65c02"          , my_no_argument      , NULL,  0x105},
    {"m4510"            , my_no_argument      , NULL,  0x111},
    {"labels"           , my_required_argument, NULL, 'l'},
    {"output"           , my_required_argument, NULL, 'o'},
    {"error"            , my_required_argument, NULL, 'E'},
    {"vice-labels"      , my_no_argument      , NULL,  0x10b},
    {"dump-labels"      , my_no_argument      , NULL,  0x10d},
    {"list"             , my_required_argument, NULL, 'L'},
    {"verbose-list"     , my_no_argument      , NULL,  0x110},
    {"no-monitor"       , my_no_argument      , NULL, 'm'},
    {"no-source"        , my_no_argument      , NULL, 's'},
    {"line-numbers"     , my_no_argument      , NULL,  0x112},
    {"no-caret-diag"    , my_no_argument      , NULL,  0x10a},
    {"tab-size"         , my_required_argument, NULL,  0x109},
    {"version"          , my_no_argument      , NULL, 'V'},
    {"usage"            , my_no_argument      , NULL,  0x102},
    {"help"             , my_no_argument      , NULL,  0x103},
    {NULL               , my_no_argument      , NULL,  0}
};

int testarg(int argc,char *argv[], struct file_s *fin) {
    int opt, tab;
    size_t max_lines = 0, fp = 0;

    for (;;) {
        opt = my_getopt_long(argc, argv, short_options, long_options, NULL);
        if (opt == -1) break;
        switch (opt) {
        case 'W':
            if (woption(argv[0], my_optarg)) goto exit;
            break;
        case 'w':arguments.warning = false;break;
        case 'q':arguments.quiet = false;break;
        case 'X':arguments.longaddr = true;break;
        case 'n':arguments.output_mode = OUTPUT_NONLINEAR;break;
        case 0x107:arguments.output_mode = OUTPUT_XEX;break;
        case 0x108:arguments.output_mode = OUTPUT_APPLE;break;
        case 0x10e:arguments.output_mode = OUTPUT_IHEX;break;
        case 0x10f:arguments.output_mode = OUTPUT_SREC;break;
        case 0x10c:arguments.output_mode = OUTPUT_CBM;break;
        case 'b':arguments.output_mode = OUTPUT_RAW;break;
        case 'f':arguments.output_mode = OUTPUT_FLAT;break;
        case 'a':arguments.toascii = true;break;
        case 'T':arguments.tasmcomp = true;break;
        case 'o':arguments.output = my_optarg;break;
        case 0x10a:arguments.caret = false;break;
        case 'D':
            {
                size_t len = strlen(my_optarg) + 1;

                if (fin->lines >= max_lines) {
                    max_lines += 1024;
                    if (/*max_lines < 1024 ||*/ max_lines > SIZE_MAX / sizeof *fin->line) err_msg_out_of_memory(); /* overflow */
                    fin->line = (size_t *)reallocx(fin->line, max_lines * sizeof *fin->line);
                }
                fin->line[fin->lines++] = fp;

                if (len < 1 || fp + len < len) err_msg_out_of_memory();
                if (fp + len > fin->len) {
                    fin->len = fp + len + 1024;
                    if (fin->len < 1024) err_msg_out_of_memory();
                    fin->data = (uint8_t*)reallocx(fin->data, fin->len);
                }
                memcpy(fin->data + fp, my_optarg, len);
                fp += len;
            }
            break;
        case 'B': arguments.longbranch = true;break;
        case 0x101: arguments.cpumode = &c6502;break;
        case 'i': arguments.cpumode = &c6502i;break;
        case 'c': arguments.cpumode = &c65c02;break;
        case 0x106: arguments.cpumode = &c65ce02;break;
        case 'x': arguments.cpumode = &w65816;break;
        case 't': arguments.cpumode = &c65dtv02;break;
        case 'e': arguments.cpumode = &c65el02;break;
        case 0x104: arguments.cpumode = &r65c02;break;
        case 0x105: arguments.cpumode = &w65c02;break;
        case 0x111: arguments.cpumode = &c4510;break;
        case 'l': arguments.label = my_optarg;break;
        case 0x10b: arguments.label_mode = LABEL_VICE; break;
        case 0x10d: arguments.label_mode = LABEL_DUMP; break;
        case 'E': arguments.error = my_optarg;break;
        case 'L': arguments.list = my_optarg;break;
        case 'M': arguments.make = my_optarg;break;
        case 'I': include_list_add(my_optarg);break;
        case 'm': arguments.monitor = false;break;
        case 's': arguments.source = false;break;
        case 0x112: arguments.linenum = true;break;
        case 'C': arguments.caseinsensitive = 0;break;
        case 0x110: arguments.verbose = true;break;
        case 0x109:tab = atoi(my_optarg); if (tab > 0 && tab <= 64) arguments.tab_size = tab; break;
        case 0x102:puts(
         /* 12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
           "Usage: 64tass [-abBCfnTqwWcitxmse?V] [-D <label>=<value>] [-o <file>]\n"
           "        [-E <file>] [-I <path>] [-l <file>] [-L <file>] [-M <file>] [--ascii]\n"
           "        [--nostart] [--long-branch] [--case-sensitive] [--cbm-prg] [--flat]\n"
           "        [--atari-xex] [--apple-ii] [--intel-hex] [--s-record] [--nonlinear]\n"
           "        [--tasm-compatible] [--quiet] [--no-warn] [--long-address] [--m65c02]\n"
           "        [--m6502] [--m65xx] [--m65dtv02] [--m65816] [--m65el02] [--mr65c02]\n"
           "        [--mw65c02] [--m65ce02] [--m4510] [--labels=<file>] [--vice-labels]\n"
           "        [--dump-labels] [--list=<file>] [--no-monitor] [--no-source]\n"
           "        [--line-numbers] [--tab-size=<value>] [--verbose-list] [-W<option>]\n"
           "        [--errors=<file>] [--output=<file>] [--help] [--usage]\n"
           "        [--version] SOURCES");
               return 0;

        case 'V':puts("64tass Turbo Assembler Macro V" VERSION);
                 return 0;
        case 0x103:
        case '?':if (my_optopt == '?' || opt == 0x103) { puts(
           "Usage: 64tass [OPTIONS...] SOURCES\n"
           "64tass Turbo Assembler Macro V" VERSION "\n"
           "\n"
           "  -a, --ascii           Source is not in PETASCII\n"
           "  -B, --long-branch     Automatic bxx *+3 jmp $xxxx\n"
           "  -C, --case-sensitive  Case sensitive labels\n"
           "  -D <label>=<value>    Define <label> to <value>\n"
           "  -E, --error=<file>    Place errors into <file>\n"
           "  -I <path>             Include search path\n"
           "  -M <file>             Makefile dependencies to <file>\n"
           "  -q, --quiet           Do not output summary and header\n"
           "  -T, --tasm-compatible Enable TASM compatible mode\n"
           "  -w, --no-warn         Suppress warnings\n"
           "      --no-caret-diag   Suppress source line display\n"
           "  -Woptimize            Optimization warnings\n"
           "  -Wshadow              Check symbol shadowing\n"
           "  -Wstrict-bool         No implicit bool conversions\n"
           "\n"
           " Output selection:\n"
           "  -o, --output=<file>   Place output into <file>\n"
           "  -b, --nostart         Strip starting address\n"
           "  -f, --flat            Generate flat output file\n"
           "  -n, --nonlinear       Generate nonlinear output file\n"
           "  -X, --long-address    Use 3 byte start/len address\n"
           "      --cbm-prg         Output CBM program file\n"
           "      --atari-xex       Output Atari XEX file\n"
           "      --apple-ii        Output Apple II file\n"
           "      --intel-hex       Output Intel HEX file\n"
           "      --s-record        Output Motorola S-record file\n"
           "\n"
           " Target CPU selection:\n"
           "      --m65xx           Standard 65xx (default)\n"
           "  -c, --m65c02          CMOS 65C02\n"
           "      --m65ce02         CSG 65CE02\n"
           "  -e, --m65el02         65EL02\n"
           "  -i, --m6502           NMOS 65xx\n"
           "  -t, --m65dtv02        65DTV02\n"
           "  -x, --m65816          W65C816\n"
           "      --mr65c02         R65C02\n"
           "      --mw65c02         W65C02\n"
           "      --m4510           CSG 4510\n"
           "\n"
           " Source listing and labels:\n"
           "  -l, --labels=<file>   List labels into <file>\n"
           "      --vice-labels     Labels in VICE format\n"
           "      --dump-labels     Dump for debugging\n"
           "  -L, --list=<file>     List into <file>\n"
           "  -m, --no-monitor      Don't put monitor code into listing\n"
           "  -s, --no-source       Don't put source code into listing\n"
           "      --line-numbers    Put line numbers into listing\n"
           "      --tab-size=<n>    Override the default tab size (8)\n"
           "      --verbose-list    List unused lines as well\n"
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
            /* fall through */
        default:
        exit:
            fputs("Try '64tass --help' or '64tass --usage' for more information.\n", stderr);
            return -1;
        }
    }

    switch (arguments.output_mode) {
    case OUTPUT_RAW:
    case OUTPUT_NONLINEAR:
    case OUTPUT_CBM: all_mem2 = arguments.longaddr ? 0xffffff : 0xffff; break;
    case OUTPUT_IHEX:
    case OUTPUT_SREC:
    case OUTPUT_FLAT: all_mem2 = 0xffffffff; break;
    case OUTPUT_APPLE:
    case OUTPUT_XEX: all_mem2 = 0xffff; break;
    }
    if (dash_name(arguments.output)) arguments.quiet = false;
    if (fin->lines != max_lines) {
        fin->line = (size_t *)reallocx(fin->line, fin->lines * sizeof *fin->line);
    }
    closefile(fin);
    if (fp != fin->len) {
        fin->len = fp;
        if (fin->len != 0) {
            fin->data = (uint8_t*)reallocx(fin->data, fin->len);
        }
    }
    fin->coding = E_UTF8;
    if (argc <= my_optind) {
        fputs("Usage: 64tass [OPTIONS...] SOURCES\n"
              "Try '64tass --help' or '64tass --usage' for more information.\n", stderr);
        return -1;
    }
    return my_optind;
}
