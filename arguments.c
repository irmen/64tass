/*
    $Id: arguments.c 3115 2023-09-07 06:06:04Z soci $

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
#include <ctype.h>
#include "64tass.h"
#include "opcodes.h"
#include "my_getopt.h"
#include "error.h"
#include "unicode.h"
#include "version.h"

struct arguments_s arguments;
struct diagnostics_s diagnostics;
struct diagnostics_s diagnostic_errors;

static const struct arguments_s arguments_default = {
    true,        /* quiet */
    false,       /* to_ascii */
    false,       /* longbranch */
    false,       /* tasmcomp */
    0x20,        /* caseinsensitive */
    NULL,        /* output */
    0,           /* output_len */
    &c6502,      /* cpumode */
    NULL,        /* symbol_output */
    0,           /* symbol_output_len */
    NULL,        /* include */
    {            /* list */
        {0,0,0}, /* name_pos */
        NULL,    /* name */
        true,    /* monitor */
        true,    /* source */
        false,   /* linenum */
        false,   /* verbose */
        false    /* append */
    },
    {            /* make */
        {0,0,0}, /* name_pos */
        NULL,    /* name */
        false,   /* phony */
        false    /* append */
    },
    {            /* defines */
        NULL,    /* data */
        0,       /* len */
    },
    {            /* commandline */
        NULL,    /* data */
        0,       /* len */
    },
    {            /* error */
        {0,0,0}, /* name_pos */
        NULL,    /* name */
        CARET_ALWAYS, /* caret */
        true,    /* warning */
        false,   /* no_output */
        false    /* append */
    },
    8,           /* tab_size */
};

static const struct diagnostics_s diagnostics_default = {
    false,       /* shadow */
    false,       /* strict_bool */
    false,       /* optimize */
    false,       /* implied_reg */
    true,        /* jmp_bug */
    {
        true,    /* wrap-pc */
        true,    /* wrap-mem */
        true,    /* wrap-addr */
        true,    /* wrap-dpage */
        true,    /* wrap-bank0 */
        true,    /* wrap-pbank */
    },
    true,        /* label_left */
    false,       /* branch_page */
    true,        /* deprecated */
    false,       /* old_equal */
    true,        /* portable */
    {
        false,   /* unused-macro */
        false,   /* unused-const */
        false,   /* unused-label */
        false,   /* unused-variable */
    },
    false,       /* case_symbol */
    false,       /* immediate */
    true,        /* float_compare */
    false,       /* leading_zeros */
    false,       /* alias */
    true,        /* pitfalls */
    true,        /* star_assign */
    true,        /* ignored */
    false,       /* long_branch */
    false,       /* altmode */
    false,       /* align */
    true,        /* page */
    false,       /* macro_prefix */
    false,       /* float_round */
    true,        /* size_larger */
    true         /* priority */
};

static const struct diagnostics_s diagnostic_errors_default = {
    false,       /* shadow */
    false,       /* strict_bool */
    false,       /* optimize */
    false,       /* implied_reg */
    false,       /* jmp_bug */
    {
        false,   /* wrap-pc */
        false,   /* wrap-mem */
        false,   /* wrap-addr */
        false,   /* wrap-dpage */
        false,   /* wrap-bank0 */
        false,   /* wrap-pbank */
    },
    false,       /* label_left */
    false,       /* branch_page */
    false,       /* deprecated */
    false,       /* old_equal */
    false,       /* portable */
    {
        false,   /* unused-macro */
        false,   /* unused-const */
        false,   /* unused-label */
        false,   /* unused-variable */
    },
    false,       /* case_symbol */
    false,       /* immediate */
    false,       /* float_compare */
    false,       /* leading_zeros */
    false,       /* alias */
    false,       /* pitfalls */
    false,       /* star_assign */
    false,       /* ignored */
    false,       /* long_branch */
    false,       /* altmode */
    false,       /* align */
    true,        /* page */
    false,       /* macro_prefix */
    false,       /* float_round */
    false,       /* size_larger */
    false        /* priority */
};

static struct diagnostics_s diagnostic_no_all;
static struct diagnostics_s diagnostic_all;

static const struct diagnostics_s diagnostic_all_default = {
    true,        /* shadow */
    true,        /* strict_bool */
    false,       /* optimize */
    true,        /* implied_reg */
    true,        /* jmp_bug */
    {
        true,    /* wrap-pc */
        true,    /* wrap-mem */
        true,    /* wrap-addr */
        true,    /* wrap-dpage */
        true,    /* wrap-bank0 */
        true,    /* wrap-pbank */
    },
    true,        /* label_left */
    false,       /* branch_page */
    true,        /* deprecated */
    true,        /* old_equal */
    true,        /* portable */
    {
        false,   /* unused-macro */
        false,   /* unused-const */
        false,   /* unused-label */
        false,   /* unused-variable */
    },
    false,       /* case_symbol */
    false,       /* immediate */
    true,        /* float_compare */
    true,        /* leading_zeros */
    false,       /* alias */
    true,        /* pitfalls */
    true,        /* star_assign */
    true,        /* ignored */
    false,       /* long_branch */
    false,       /* altmode */
    false,       /* align */
    true,        /* page */
    false,       /* macro_prefix */
    true,        /* float_round */
    true,        /* size_larger */
    true         /* priority */
};

static struct diagnostics_s diagnostic_no_error_all;
static struct diagnostics_s diagnostic_error_all;

static const struct diagnostics_s diagnostic_error_all_default = {
    true,        /* shadow */
    true,        /* strict_bool */
    true,        /* optimize */
    true,        /* implied_reg */
    true,        /* jmp_bug */
    {
        true,    /* wrap-pc */
        true,    /* wrap-mem */
        true,    /* wrap-addr */
        true,    /* wrap-dpage */
        true,    /* wrap-bank0 */
        true,    /* wrap-pbank */
    },
    true,        /* label_left */
    true,        /* branch_page */
    true,        /* deprecated */
    true,        /* old_equal */
    true,        /* portable */
    {
        true,    /* unused-macro */
        true,    /* unused-const */
        true,    /* unused-label */
        true,    /* unused-variable */
    },
    true,        /* case_symbol */
    true,        /* immediate */
    true,        /* float_compare */
    true,        /* leading_zeros */
    true,        /* alias */
    true,        /* pitfalls */
    true,        /* star_assign */
    true,        /* ignored */
    true,        /* long_branch */
    true,        /* altmode */
    true,        /* align */
    true,        /* page */
    true,        /* macro_prefix */
    true,        /* float_round */
    true,        /* size_larger */
    true         /* priority */
};

struct w_options_s {
    const char *const name;
    bool *const opt;
};

static const struct w_options_s w_options[] = {
    {"optimize",        &diagnostics.optimize},
    {"shadow",          &diagnostics.shadow},
    {"strict-bool",     &diagnostics.strict_bool},
    {"implied-reg",     &diagnostics.implied_reg},
    {"jmp-bug",         &diagnostics.jmp_bug},
    {"pc-wrap",         &diagnostics.wrap.pc},
    {"mem-wrap",        &diagnostics.wrap.mem},
    {"wrap-pc",         &diagnostics.wrap.pc},
    {"wrap-mem",        &diagnostics.wrap.mem},
    {"wrap-addr",       &diagnostics.wrap.addr},
    {"wrap-dpage",      &diagnostics.wrap.dpage},
    {"wrap-bank0",      &diagnostics.wrap.bank0},
    {"wrap-pbank",      &diagnostics.wrap.pbank},
    {"label-left",      &diagnostics.label_left},
    {"branch-page",     &diagnostics.branch_page},
    {"deprecated",      &diagnostics.deprecated},
    {"old-equal",       &diagnostics.old_equal},
    {"portable",        &diagnostics.portable},
    {"unused-macro",    &diagnostics.unused.macro},
    {"unused-const",    &diagnostics.unused.consts},
    {"unused-label",    &diagnostics.unused.label},
    {"unused-variable", &diagnostics.unused.variable},
    {"case-symbol",     &diagnostics.case_symbol},
    {"immediate",       &diagnostics.immediate},
    {"float-compare",   &diagnostics.float_compare},
    {"leading-zeros",   &diagnostics.leading_zeros},
    {"alias",           &diagnostics.alias},
    {"pitfalls",        &diagnostics.pitfalls},
    {"star-assign",     &diagnostics.star_assign},
    {"ignored",         &diagnostics.ignored},
    {"long-branch",     &diagnostics.long_branch},
    {"altmode",         &diagnostics.altmode},
    {"align",           &diagnostics.align},
    {"page",            &diagnostics.page},
    {"macro-prefix",    &diagnostics.macro_prefix},
    {"float-round",     &diagnostics.float_round},
    {"size-larger",     &diagnostics.size_larger},
    {"priority",        &diagnostics.priority},
    {NULL,              NULL}
};

static bool woption(const char *s) {
    bool no = (s[0] == 'n') && (s[1] == 'o') && (s[2] == '-'), *b;
    const struct w_options_s *w = w_options;
    const char *s2 = no ? s + 3 : s;
    size_t m;

    if (strcmp(s2, "all") == 0) {
        memcpy(&diagnostics, no ? &diagnostic_no_all : &diagnostic_all, sizeof diagnostics);
        return false;
    }

    if (strcmp(s2, "error") == 0) {
        memcpy(&diagnostic_errors, no ? &diagnostic_no_error_all : &diagnostic_error_all, sizeof diagnostic_errors);
        return false;
    }

    m = strcmp(s2, "unused") == 0 ? strlen(s2) : SIZE_MAX;

    if (strncmp(s2, "error=", 6) == 0) {
        s2 += 6;
        while (w->name != NULL) {
            if (strncmp(w->name, s2, m) == 0) {
                if (!no) *w->opt = true;
                b = w->opt - &diagnostics.shadow + &diagnostic_errors.shadow;
                *b = !no;
                b = w->opt - &diagnostics.shadow + &diagnostic_error_all.shadow;
                *b = !no;
                b = w->opt - &diagnostics.shadow + &diagnostic_no_error_all.shadow;
                *b = !no;
                if (m == SIZE_MAX) return false;
            }
            w++;
        }
    } else {
        while (w->name != NULL) {
            if (strncmp(w->name, s2, m) == 0) {
                *w->opt = !no;
                b = w->opt - &diagnostics.shadow + &diagnostic_all.shadow;
                *b = !no;
                b = w->opt - &diagnostics.shadow + &diagnostic_no_all.shadow;
                *b = !no;
                if (m == SIZE_MAX) return false;
            }
            w++;
        }
    }
    if (m != SIZE_MAX) return false;
    fatal_error("unrecognized option '-W");
    printable_print((const uint8_t *)s, stderr);
    putc('\'', stderr);
    fatal_error(NULL);
    return true;
}

static const char short_options[] = "wqnbfXaTCBicxtel:L:I:M:msV?o:D:E:W:";

enum {
    HELP = 256, USAGE, TAB_SIZE, CARET_DIAG, MACRO_CARET_DIAG, NO_CARET_DIAG,
    LINE_NUMBERS, NO_LINE_NUMBERS, SOURCE, MONITOR, VERBOSE_LIST,
    NO_VERBOSE_LIST, MAKE_PHONY, NO_MAKE_PHONY, LABELS_ROOT, DUMP_LABELS,
    VICE_LABELS_NUMERIC, VICE_LABELS, EXPORT_LABELS, NORMAL_LABELS,
    OUTPUT_SECTION, M4510, MW65C02, MR65C02, M65CE02, M65XX, NO_LONG_BRANCH,
    NO_CASE_SENSITIVE, NO_TASM_COMPATIBLE, NO_ASCII, CBM_PRG, S_RECORD,
    INTEL_HEX, APPLE_II, ATARI_XEX, MOS_HEX, NO_LONG_ADDRESS, NO_QUIET, WARN,
    OUTPUT_APPEND, NO_OUTPUT, ERROR_APPEND, NO_ERROR, LABELS_APPEND, MAP,
    NO_MAP, MAP_APPEND, LIST_APPEND, SIMPLE_LABELS, LABELS_SECTION,
    MESEN_LABELS, LABELS_ADD_PREFIX, MAKE_APPEND, C256_PGX, C256_PGZ,
    OUTPUT_EXEC
};

static const struct my_option long_options[] = {
    {"no-warn"          , my_no_argument      , NULL, 'w'},
    {"warn"             , my_no_argument      , NULL,  WARN},
    {"no-quiet"         , my_no_argument      , NULL,  NO_QUIET},
    {"quiet"            , my_no_argument      , NULL, 'q'},
    {"nonlinear"        , my_no_argument      , NULL, 'n'},
    {"nostart"          , my_no_argument      , NULL, 'b'},
    {"flat"             , my_no_argument      , NULL, 'f'},
    {"no-long-address"  , my_no_argument      , NULL,  NO_LONG_ADDRESS},
    {"long-address"     , my_no_argument      , NULL, 'X'},
    {"atari-xex"        , my_no_argument      , NULL,  ATARI_XEX},
    {"apple-ii"         , my_no_argument      , NULL,  APPLE_II},
    {"intel-hex"        , my_no_argument      , NULL,  INTEL_HEX},
    {"mos-hex"          , my_no_argument      , NULL,  MOS_HEX},
    {"s-record"         , my_no_argument      , NULL,  S_RECORD},
    {"cbm-prg"          , my_no_argument      , NULL,  CBM_PRG},
    {"c256-pgx"         , my_no_argument      , NULL,  C256_PGX},
    {"c256-pgz"         , my_no_argument      , NULL,  C256_PGZ},
    {"no-ascii"         , my_no_argument      , NULL,  NO_ASCII},
    {"ascii"            , my_no_argument      , NULL, 'a'},
    {"no-tasm-compatible",my_no_argument      , NULL,  NO_TASM_COMPATIBLE},
    {"tasm-compatible"  , my_no_argument      , NULL, 'T'},
    {"no-case-sensitive", my_no_argument      , NULL,  NO_CASE_SENSITIVE},
    {"case-sensitive"   , my_no_argument      , NULL, 'C'},
    {"no-long-branch"   , my_no_argument      , NULL,  NO_LONG_BRANCH},
    {"long-branch"      , my_no_argument      , NULL, 'B'},
    {"m65xx"            , my_no_argument      , NULL,  M65XX},
    {"m6502"            , my_no_argument      , NULL, 'i'},
    {"m65c02"           , my_no_argument      , NULL, 'c'},
    {"m65ce02"          , my_no_argument      , NULL,  M65CE02},
    {"m65816"           , my_no_argument      , NULL, 'x'},
    {"m65dtv02"         , my_no_argument      , NULL, 't'},
    {"m65el02"          , my_no_argument      , NULL, 'e'},
    {"mr65c02"          , my_no_argument      , NULL,  MR65C02},
    {"mw65c02"          , my_no_argument      , NULL,  MW65C02},
    {"m4510"            , my_no_argument      , NULL,  M4510},
    {"labels"           , my_required_argument, NULL, 'l'},
    {"labels-append"    , my_required_argument, NULL,  LABELS_APPEND},
    {"output"           , my_required_argument, NULL, 'o'},
    {"no-output"        , my_no_argument      , NULL,  NO_OUTPUT},
    {"output-append"    , my_required_argument, NULL,  OUTPUT_APPEND},
    {"output-section"   , my_required_argument, NULL,  OUTPUT_SECTION},
    {"output-exec"      , my_required_argument, NULL,  OUTPUT_EXEC},
    {"map"              , my_required_argument, NULL,  MAP},
    {"no-map"           , my_no_argument      , NULL,  NO_MAP},
    {"map-append"       , my_required_argument, NULL,  MAP_APPEND},
    {"error"            , my_required_argument, NULL, 'E'},
    {"no-error"         , my_no_argument      , NULL,  NO_ERROR},
    {"error-append"     , my_required_argument, NULL,  ERROR_APPEND},
    {"normal-labels"    , my_no_argument      , NULL,  NORMAL_LABELS},
    {"export-labels"    , my_no_argument      , NULL,  EXPORT_LABELS},
    {"vice-labels"      , my_no_argument      , NULL,  VICE_LABELS},
    {"vice-labels-numeric",my_no_argument     , NULL,  VICE_LABELS_NUMERIC},
    {"dump-labels"      , my_no_argument      , NULL,  DUMP_LABELS},
    {"simple-labels"    , my_no_argument      , NULL,  SIMPLE_LABELS},
    {"mesen-labels"     , my_no_argument      , NULL,  MESEN_LABELS},
    {"labels-add-prefix", my_required_argument, NULL,  LABELS_ADD_PREFIX},
    {"labels-root"      , my_required_argument, NULL,  LABELS_ROOT},
    {"labels-section"   , my_required_argument, NULL,  LABELS_SECTION},
    {"list"             , my_required_argument, NULL, 'L'},
    {"list-append"      , my_required_argument, NULL,  LIST_APPEND},
    {"dependencies"     , my_required_argument, NULL, 'M'},
    {"dependencies-append",my_required_argument,NULL,  MAKE_APPEND},
    {"no-make-phony"    , my_no_argument      , NULL,  NO_MAKE_PHONY},
    {"make-phony"       , my_no_argument      , NULL,  MAKE_PHONY},
    {"no-verbose-list"  , my_no_argument      , NULL,  NO_VERBOSE_LIST},
    {"verbose-list"     , my_no_argument      , NULL,  VERBOSE_LIST},
    {"no-monitor"       , my_no_argument      , NULL, 'm'},
    {"monitor"          , my_no_argument      , NULL,  MONITOR},
    {"no-source"        , my_no_argument      , NULL, 's'},
    {"source"           , my_no_argument      , NULL,  SOURCE},
    {"no-line-numbers"  , my_no_argument      , NULL,  NO_LINE_NUMBERS},
    {"line-numbers"     , my_no_argument      , NULL,  LINE_NUMBERS},
    {"no-caret-diag"    , my_no_argument      , NULL,  NO_CARET_DIAG},
    {"macro-caret-diag" , my_no_argument      , NULL,  MACRO_CARET_DIAG},
    {"caret-diag"       , my_no_argument      , NULL,  CARET_DIAG},
    {"tab-size"         , my_required_argument, NULL,  TAB_SIZE},
    {"version"          , my_no_argument      , NULL, 'V'},
    {"usage"            , my_no_argument      , NULL,  USAGE},
    {"help"             , my_no_argument      , NULL,  HELP},
    {NULL               , my_no_argument      , NULL,  0}
};

static MUST_CHECK char *read_one(FILE *f) {
    bool q, q2, q3;
    char *line;
    size_t i, ln;
    int c;
    uint8_t *data;

    do {
        c = getc(f);
        if (c == EOF) break;
    } while (c == 0 || isspace(c) != 0);
    if (c == EOF) return NULL;
    line = NULL;
    i = ln = 0;
    q = q2 = q3 = false;
    do {
        if (!q3 && c == '\\') q3 = true;
        else if (!q3 && !q2 && c == '"') q = !q;
        else if (!q3 && !q && c == '\'') q2 = !q2;
        else {
            q3 = false;
            if (i >= ln) extend_array(&line, &ln, 16);
            line[i++] = (char)c;
        }
        c = getc(f);
        if (c == EOF || c == 0) break;
    } while (q || q2 || q3 || isspace(c) == 0);
    if (i >= ln) {
        extend_array(&line, &ln, 1);
    }
    line[i] = 0;

    data = char_to_utf8(line);
    if (data == NULL) err_msg_out_of_memory();
    if ((char *)data != line) free(line);
    return (char *)data;
}

static address_t check_outputs(void) {
    size_t i;
    bool tostdout = false;
    address_t min = 0xffffffff;
    for (i = 0; i < arguments.output_len; i++) {
        const struct output_s *output = &arguments.output[i];
        switch (output->mode) {
        case OUTPUT_RAW:
        case OUTPUT_NONLINEAR:
        case OUTPUT_CBM: min &= output->longaddr ? 0xffffff : 0xffff; break;
        case OUTPUT_IHEX:
        case OUTPUT_SREC:
        case OUTPUT_FLAT: min &= 0xffffffff; break;
        case OUTPUT_PGX:
        case OUTPUT_PGZ: min &= 0xffffff; break;
        case OUTPUT_MHEX:
        case OUTPUT_APPLE:
        case OUTPUT_XEX: min &= 0xffff; break;
        }
        if (tostdout) continue;
        if (output->name != NULL && dash_name(output->name)) tostdout = true;
        if (output->mapname != NULL && dash_name(output->mapname)) tostdout = true;
    }
    if (arguments.list.name != NULL && dash_name(arguments.list.name)) tostdout = true;
    if (arguments.make.name != NULL && dash_name(arguments.make.name)) tostdout = true;
    if (!tostdout) {
        for (i = 0; i < arguments.symbol_output_len; i++) {
            struct symbol_output_s *symbol_output = &arguments.symbol_output[i];
            if (dash_name(symbol_output->name)) {
                tostdout = true;
                break;
            }
        }
    }
    if (tostdout) arguments.quiet = false;
    else setvbuf(stdout, NULL, _IOLBF, 1024);
    return min;
}

static struct include_list_s **include_list_add(struct include_list_s **lastil, const char *path)
{
    struct include_list_s *include;
    size_t i, j, len;
    j = i = strlen(path);
    if (i == 0) return lastil;
#if defined _WIN32 || defined __MSDOS__ || defined __DOS__
    if (path[i - 1] != '/' && path[i-1] != '\\') j++;
#else
    if (path[i - 1] != '/') j++;
#endif
    len = j + 1;
    if (inc_overflow(&len, sizeof(struct include_list_s))) err_msg_out_of_memory();
    include = (struct include_list_s *)allocate_array(uint8_t, len);
    if (include == NULL) err_msg_out_of_memory();
    include->next = NULL;
    memcpy(include->path, path, i + 1);
    if (i != j) memcpy(include->path + i, "/", 2);
    *lastil = include;
    return &include->next;
}

struct get_arg_s {
    char **argv;
    int optind;
    uint32_t pos;
};

static void get_arg(struct get_arg_s *param, struct argpos_s *argpos) {
    int i;
    uint8_t *data;
    uint32_t len = 0;
    for (i = param->optind; i < my_optind; i++) {
        if (inc_overflow(&len, strlen(param->argv[i]) + 1)) err_msg_out_of_memory();
    }
    argpos->start = param->pos;
    argpos->line = (linenum_t)my_optind - 1;
    argpos->pos = (linecpos_t)(len - strlen(my_optarg) - 1);
    if (inc_overflow(&param->pos, len)) err_msg_out_of_memory();
    if (param->pos > arguments.commandline.len) {
        if (add_overflow(param->pos, 1024, &arguments.commandline.len)) err_msg_out_of_memory();
        resize_array(&arguments.commandline.data, arguments.commandline.len);
    }
    data = arguments.commandline.data + (param->pos - len);
    for (i = param->optind; i < my_optind; i++) {
        size_t l = strlen(param->argv[i]);
        memcpy(data, param->argv[i], l);
        data += l;
        *data++ = ' ';
    }
    data[-1] = 0;
}

int init_arguments(int *argc2, char **argv2[]) {
    int argc = *argc2;
    char **argv = *argv2;
    int opt;
    size_t defines_p = 0;
    struct get_arg_s get_args;
    int max = 10;
    bool again;
    struct include_list_s **lastil = &arguments.include;
    struct symbol_output_s symbol_output = { {0, 0, 0}, NULL, {0, 0, 0}, NULL, NULL, NULL, LABEL_64TASS, false };
    struct output_s output = { {0, 0, 0}, "a.out", NULL, {0, 0, 0}, NULL, {0, 0, 0}, 0, OUTPUT_CBM, false, false, false, false };
    memcpy(&arguments, &arguments_default, sizeof arguments);
    memcpy(&diagnostics, &diagnostics_default, sizeof diagnostics);
    memcpy(&diagnostic_errors, &diagnostic_errors_default, sizeof diagnostic_errors);
    memset(&diagnostic_no_all, 0, sizeof diagnostic_no_all);
    memcpy(&diagnostic_all, &diagnostic_all_default, sizeof diagnostic_all);
    memset(&diagnostic_no_error_all, 0, sizeof diagnostic_no_error_all);
    memcpy(&diagnostic_error_all, &diagnostic_error_all_default, sizeof diagnostic_error_all);

    get_args.pos = 0;
    my_optind = 1;
    do {
        int i;
        get_args.argv = argv;
        again = false;
        for (;;) {
            get_args.optind = my_optind;
            opt = my_getopt_long(argc, argv, short_options, long_options, NULL);
            if (opt == -1) break;
            switch (opt) {
            case 'W':
                if (woption(my_optarg)) goto exit;
                break;
            case 'w':arguments.error.warning = false;break;
            case WARN:arguments.error.warning = true;break;
            case 'q':arguments.quiet = false;break;
            case NO_QUIET:arguments.quiet = true;break;
            case 'X':output.longaddr = true;break;
            case NO_LONG_ADDRESS:output.longaddr = false;break;
            case 'n':output.mode = OUTPUT_NONLINEAR;break;
            case ATARI_XEX:output.mode = OUTPUT_XEX;break;
            case APPLE_II:output.mode = OUTPUT_APPLE;break;
            case INTEL_HEX:output.mode = OUTPUT_IHEX;break;
            case MOS_HEX:output.mode = OUTPUT_MHEX;break;
            case S_RECORD:output.mode = OUTPUT_SREC;break;
            case CBM_PRG: output.mode = OUTPUT_CBM;break;
            case C256_PGX: output.mode = OUTPUT_PGX;break;
            case C256_PGZ: output.mode = OUTPUT_PGZ;break;
            case 'b':output.mode = OUTPUT_RAW;break;
            case 'f':output.mode = OUTPUT_FLAT;break;
            case 'a':arguments.to_ascii = true;break;
            case NO_ASCII:arguments.to_ascii = false;break;
            case 'T':arguments.tasmcomp = true;break;
            case NO_TASM_COMPATIBLE:arguments.tasmcomp = false;break;
            case NO_OUTPUT: output.name = NULL; output.name_pos.start = 0; output.name_pos.line = 0; output.name_pos.pos = 0; output.append = false; break;
            case OUTPUT_APPEND:
            case 'o': output.name = my_optarg; get_arg(&get_args, &output.name_pos);
                      output.append = (opt == OUTPUT_APPEND) || dash_name(output.name);
                      extend_array(&arguments.output, &arguments.output_len, 1);
                      arguments.output[arguments.output_len - 1] = output;
                      output.section = NULL;
                      output.mapname = NULL;
                      output.mapname_pos.start = 0;
                      output.mapname_pos.line = 0;
                      output.mapname_pos.pos = 0;
                      output.mapfile = false;
                      output.exec_pos.start = 0;
                      output.exec_pos.line = 0;
                      output.exec_pos.pos = 0;
                      break;
            case OUTPUT_SECTION:output.section = my_optarg; break;
            case OUTPUT_EXEC: get_arg(&get_args, &output.exec_pos); break;
            case MAP_APPEND:
            case MAP: output.mapname = my_optarg; get_arg(&get_args, &output.mapname_pos); output.mapappend = (opt == MAP_APPEND); output.mapfile = true; break;
            case NO_MAP:output.mapname = NULL; output.mapname_pos.start = 0; output.mapname_pos.line = 0; output.mapname_pos.pos = 0; output.mapfile = true; break;
            case CARET_DIAG:arguments.error.caret = CARET_ALWAYS;break;
            case MACRO_CARET_DIAG:arguments.error.caret = CARET_MACRO;break;
            case NO_CARET_DIAG:arguments.error.caret = CARET_NEVER;break;
            case 'D':
                {
                    size_t len = strlen(my_optarg) + 1;
                    if (inc_overflow(&defines_p, len)) err_msg_out_of_memory();
                    if (defines_p > arguments.defines.len) {
                        if (add_overflow(defines_p, 1024, &arguments.defines.len)) err_msg_out_of_memory();
                        resize_array(&arguments.defines.data, arguments.defines.len);
                    }
                    memcpy(arguments.defines.data + defines_p - len, my_optarg, len - 1);
                    arguments.defines.data[defines_p - 1] = '\n';
                }
                break;
            case 'B': arguments.longbranch = true;break;
            case NO_LONG_BRANCH: arguments.longbranch = false;break;
            case M65XX: arguments.cpumode = &c6502;break;
            case 'i': arguments.cpumode = &c6502i;break;
            case 'c': arguments.cpumode = &c65c02;break;
            case M65CE02: arguments.cpumode = &c65ce02;break;
            case 'x': arguments.cpumode = &w65816;break;
            case 't': arguments.cpumode = &c65dtv02;break;
            case 'e': arguments.cpumode = &c65el02;break;
            case MR65C02: arguments.cpumode = &r65c02;break;
            case MW65C02: arguments.cpumode = &w65c02;break;
            case M4510: arguments.cpumode = &c4510;break;
            case LABELS_APPEND:
            case 'l': symbol_output.name = my_optarg;
                      symbol_output.append = (opt == LABELS_APPEND);
                      get_arg(&get_args, &symbol_output.name_pos);
                      extend_array(&arguments.symbol_output, &arguments.symbol_output_len, 1);
                      arguments.symbol_output[arguments.symbol_output_len - 1] = symbol_output;
                      symbol_output.space_pos.start = 0;
                      symbol_output.space_pos.line = 0;
                      symbol_output.space_pos.pos = 0;
                      symbol_output.section = NULL;
                      symbol_output.add_prefix = NULL;
                      break;
            case NORMAL_LABELS: symbol_output.mode = LABEL_64TASS; break;
            case EXPORT_LABELS: symbol_output.mode = LABEL_EXPORT; break;
            case VICE_LABELS: symbol_output.mode = LABEL_VICE; break;
            case VICE_LABELS_NUMERIC: symbol_output.mode = LABEL_VICE_NUMERIC; break;
            case DUMP_LABELS: symbol_output.mode = LABEL_DUMP; break;
            case SIMPLE_LABELS: symbol_output.mode = LABEL_SIMPLE; break;
            case MESEN_LABELS: symbol_output.mode = LABEL_MESEN; break;
            case LABELS_ROOT: get_arg(&get_args, &symbol_output.space_pos); break;
            case LABELS_SECTION: symbol_output.section = my_optarg; break;
            case LABELS_ADD_PREFIX: symbol_output.add_prefix = my_optarg; break;
            case NO_ERROR: arguments.error.name = NULL; arguments.error.name_pos.start = 0; arguments.error.name_pos.line = 0; arguments.error.name_pos.pos = 0; arguments.error.no_output = true; arguments.error.append = false; break;
            case ERROR_APPEND:
            case 'E': arguments.error.name = my_optarg; get_arg(&get_args, &arguments.error.name_pos); arguments.error.no_output = false; arguments.error.append = (opt == ERROR_APPEND); break;
            case LIST_APPEND:
            case 'L': arguments.list.name = my_optarg; get_arg(&get_args, &arguments.list.name_pos); arguments.list.append = (opt == LIST_APPEND); break;
            case MAKE_APPEND:
            case 'M': arguments.make.name = my_optarg; get_arg(&get_args, &arguments.make.name_pos); arguments.make.append = (opt == MAKE_APPEND); break;
            case 'I': lastil = include_list_add(lastil, my_optarg);break;
            case 'm': arguments.list.monitor = false;break;
            case MONITOR: arguments.list.monitor = true;break;
            case 's': arguments.list.source = false;break;
            case SOURCE: arguments.list.source = true;break;
            case LINE_NUMBERS: arguments.list.linenum = true;break;
            case NO_LINE_NUMBERS: arguments.list.linenum = false;break;
            case 'C': arguments.caseinsensitive = 0;break;
            case NO_CASE_SENSITIVE: arguments.caseinsensitive = 0x20;break;
            case VERBOSE_LIST: arguments.list.verbose = true;break;
            case NO_VERBOSE_LIST: arguments.list.verbose = false;break;
            case MAKE_PHONY: arguments.make.phony = true;break;
            case NO_MAKE_PHONY: arguments.make.phony = false;break;
            case TAB_SIZE:
                {
                    char *s;
                    long int tab = strtol(my_optarg, &s, 10);
                    if (tab > 0 && tab <= 64 && *s == 0) arguments.tab_size = (unsigned int)tab;
                    break;
                }
            case USAGE:puts(
             /* 12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
               "Usage: 64tass [-abBCfnTqwWcitxmse?V] [-D <label>=<value>] [-o <file>]\n"
               "        [-E <file>] [-I <path>] [-l <file>] [-L <file>] [-M <file>] [--ascii]\n"
               "        [--nostart] [--long-branch] [--case-sensitive] [--cbm-prg] [--flat]\n"
               "        [--atari-xex] [--apple-ii] [--intel-hex] [--mos-hex] [--s-record]\n"
               "        [--nonlinear] [--c256-pgx] [--c256-pgz] [--tasm-compatible]\n"
               "        [--long-address] [--output-section=<name>] [--m65c02] [--m6502]\n"
               "        [--m65xx] [--m65dtv02] [--m65816] [--m65el02] [--mr65c02] [--mw65c02]\n"
               "        [--m65ce02] [--m4510] [--labels=<file>] [--labels-append=<file>]\n"
               "        [--labels-add-prefix=<txt>] [--labels-section=<name>]\n"
               "        [--labels-root=<expr>] [--export-labels] [--vice-labels-numeric]\n"
               "        [--vice-labels] [--dump-labels] [--simple-labels] [--mesen-labels]\n"
               "        [--list=<file>] [--list-append=<file>] [--no-monitor] [--no-source]\n"
               "        [--line-numbers] [--tab-size=<value>] [--verbose-list] [-W<option>]\n"
               "        [--dependencies=<file>] [--dependencies-append=<file>] [--make-phony]\n"
               "        [--output=<file>] [--output-append=<file>] [--output-exec=<expr>]\n"
               "        [--no-output] [--map=<file>] [--map-append=<file>] [--no-map]\n"
               "        [--error=<file>] [--error-append=<file>] [--quiet] [--no-warn]\n"
               "        [--no-caret-diag] [--macro-caret-diag] [--help] [--usage] [--version]\n"
               "        SOURCES\n");
                   return 0;

            case 'V':puts("64tass Turbo Assembler Macro V" VERSION);
                     return 0;
            case HELP:
            case '?':if (my_optopt == '?' || opt == HELP) { puts(
               "Usage: 64tass [OPTIONS...] SOURCES\n"
               "64tass Turbo Assembler Macro V" VERSION "\n"
               "\n"
               "  -a, --ascii            Source is not in PETASCII\n"
               "  -B, --long-branch      Automatic bxx *+5 jmp $xxxx\n"
               "  -C, --case-sensitive   Case sensitive labels\n"
               "  -D <label>=<value>     Define <label> to <value>\n"
               "  -E, --error=<file>     Place errors into <file>\n"
               "      --error-append=<f> Append errors to <file>\n"
               "      --no-error         Do not output any errors\n"
               "  -I <path>              Include search path\n"
               "  -M, --dependencies=<f> Makefile dependencies to <file>\n"
               "      --dependencies-append=<f> Append dependencies to <file>\n"
               "  -q, --quiet            Do not output summary and header\n"
               "  -T, --tasm-compatible  Enable TASM compatible mode\n"
               "  -w, --no-warn          Suppress warnings\n"
               "      --make-phony       Add phony target to dependencies\n"
               "      --no-caret-diag    Suppress source line display\n"
               "      --macro-caret-diag Source lines in macros only\n"
               "\n"
               " Diagnostic options:\n"
               "  -Wall                  Enable most diagnostic warnings\n"
               "  -Werror                Diagnostic warnings to errors\n"
               "  -Werror=<name>         Make a diagnostic to an error\n"
               "  -Wno-error=<name>      Make a diagnostic to a warning\n"
               "  -Walias                Warn about instruction aliases\n"
               "  -Walign                Warn when alignment is done\n"
               "  -Waltmode              Warn about alternative addressing\n"
               "  -Wbranch-page          Warn if a branch crosses a page\n"
               "  -Wcase-symbol          Warn on mismatch of symbol case\n"
               "  -Wimmediate            Suggest immediate addressing\n"
               "  -Wimplied-reg          No implied register aliases\n"
               "  -Wleading-zeros        Warn for ignored leading zeros\n"
               "  -Wlong-branch          Warn when a long branch is used\n"
               "  -Wmacro-prefix         Warn about unprefixed macro calls\n"
               "  -Wno-deprecated        No deprecated feature warnings\n"
               "  -Wno-float-compare     No approximate compare warnings\n"
               "  -Wno-float-round       No implicit rounding warnings\n"
               "  -Wno-ignored           No directive ignored warnings\n"
               "  -Wno-jmp-bug           No jmp ($xxff) bug warning\n"
               "  -Wno-label-left        No warning about strange labels\n"
               "  -Wno-page              No page crossing error\n"
               "  -Wno-wrap-addr         No memory address overflow warning\n"
               "  -Wno-wrap-bank0        No bank0 overflow warning\n"
               "  -Wno-wrap-dpage        No direct page overflow warning\n"
               "  -Wno-wrap-mem          No offset overflow warning\n"
               "  -Wno-wrap-pbank        No program bank overflow warning\n"
               "  -Wno-wrap-pc           No PC bank cross warning\n"
               "  -Wno-pitfalls          No common pitfall notes\n"
               "  -Wno-portable          No portability warnings\n"
               "  -Wno-priority          No operator priority warnings\n"
               "  -Wno-size-larger       No size larger than original warnings\n"
               "  -Wno-star-assign       No label multiply warnings\n"
               "  -Wold-equal            Warn about old equal operator\n"
               "  -Woptimize             Optimization warnings\n"
               "  -Wshadow               Check symbol shadowing\n"
               "  -Wstrict-bool          No implicit bool conversions\n"
               "  -Wunused               Warn about unused symbols\n"
               "  -Wunused-macro         Warn about unused macros\n"
               "  -Wunused-const         Warn about unused consts\n"
               "  -Wunused-label         Warn about unused labels\n"
               "  -Wunused-variable      Warn about unused variables\n");
               puts(" Output selection:\n"
               "  -o, --output=<file>    Place output into <file>\n"
               "      --output-append=<f> Append output to <file>\n"
               "      --no-output        Do not create an output file\n"
               "      --output-section=<n> Output this section only\n"
               "      --output-exec=<e>  Output execution address\n"
               "      --map=<f>          Place output map into <file>\n"
               "      --map-append=<f>   Append output map to <file>\n"
               "      --no-map           Do not create a map file\n"
               "  -b, --nostart          Strip starting address\n"
               "  -f, --flat             Generate flat output file\n"
               "  -n, --nonlinear        Generate nonlinear output file\n"
               "  -X, --long-address     Use 3 byte start/len address\n"
               "      --cbm-prg          Output CBM program file\n"
               "      --atari-xex        Output Atari XEX file\n"
               "      --apple-ii         Output Apple II file\n"
               "      --intel-hex        Output Intel HEX file\n"
               "      --mos-hex          Output MOS Technology file\n"
               "      --s-record         Output Motorola S-record file\n"
               "      --c256-pgx         Output C256 PGX file\n"
               "      --c256-pgz         Output C256 PGZ file\n"
               "\n"
               " Target CPU selection:\n"
               "      --m65xx            Standard 65xx (default)\n"
               "  -c, --m65c02           CMOS 65C02\n"
               "      --m65ce02          CSG 65CE02\n"
               "  -e, --m65el02          65EL02\n"
               "  -i, --m6502            NMOS 65xx\n"
               "  -t, --m65dtv02         65DTV02\n"
               "  -x, --m65816           W65C816\n"
               "      --mr65c02          R65C02\n"
               "      --mw65c02          W65C02\n"
               "      --m4510            CSG 4510\n"
               "\n"
               " Source listing and labels:\n"
               "  -l, --labels=<file>    List labels into <file>\n"
               "      --labels-append=<f> Append labels to <file>\n"
               "      --normal-labels    Labels in native format\n"
               "      --export-labels    Export for other source\n"
               "      --vice-labels      Labels in VICE format\n"
               "      --vice-labels-numeric Labels for VICE with numeric constants\n"
               "      --dump-labels      Dump for debugging\n"
               "      --simple-labels    Simple hexadecimal labels\n"
               "      --labels-root=<l>  List from scope <l> only\n"
               "      --labels-section=<n> List from section <n> only\n"
               "      --labels-add-prefix=<p> Set label prefix\n"
               "  -L, --list=<file>      List into <file>\n"
               "      --list-append=<f>  Append list to <file>\n"
               "  -m, --no-monitor       Don't put monitor code into listing\n"
               "  -s, --no-source        Don't put source code into listing\n"
               "      --line-numbers     Put line numbers into listing\n"
               "      --tab-size=<n>     Override the default tab size (8)\n"
               "      --verbose-list     List unused lines as well\n"
               "\n"
               " Misc:\n"
               "  -?, --help             Give this help list\n"
               "      --usage            Give a short usage message\n"
               "  -V, --version          Print program version\n"
               "\n"
               "Mandatory or optional arguments to long options are also mandatory or optional\n"
               "for any corresponding short options.\n"
               "\n"
               "Report bugs to <soci" "\x40" "c64.rulez.org>.");
               return 0;
            }
                FALL_THROUGH; /* fall through */
            default:
            exit:
                fputs("Try '64tass --help' or '64tass --usage' for more information.\n", stderr);
                return -1;
            }
        }

        if (my_optind > 1 && strcmp(argv[my_optind - 1], "--") == 0) break;
        for (i = my_optind; i < argc; i++) {
            char *arg = argv[i];
            if (arg[0] == '@' && arg[1] != 0) {
                FILE *f = fopen_utf8(arg + 1, "rb");
                if (f == NULL) continue;
                while (feof(f) == 0) {
                    size_t l;
                    char *onepar = read_one(f);
                    if (onepar == NULL) break;
                    l = (size_t)argc + 1;
                    resize_array(&argv, l);
                    *argv2 = argv;
                    if (arg != NULL) {
                        free(arg);
                        arg = NULL;
                    } else {
                        int j;
                        for (j = argc; j > i; j--) {
                            argv[j] = argv[j - 1];
                        }
                        argc++;
                    }
                    argv[i++] = onepar;
                    again = true;
                }
                fclose(f);
                *argc2 = argc;
                break;
            }
        }
        max--;
    } while (again && max > 0);

    if (again && max <= 0) {
        fatal_error("too many @-files encountered");
        fatal_error(NULL);
        return -1;
    }

    if (arguments.symbol_output_len != 0) {
        arguments.symbol_output[arguments.symbol_output_len - 1].mode = symbol_output.mode;
        if (symbol_output.space_pos.pos != 0) arguments.symbol_output[arguments.symbol_output_len - 1].space = symbol_output.space;
        if (symbol_output.section != NULL) arguments.symbol_output[arguments.symbol_output_len - 1].section = symbol_output.section;
        if (symbol_output.add_prefix != NULL) arguments.symbol_output[arguments.symbol_output_len - 1].add_prefix = symbol_output.add_prefix;
    }

    if (arguments.output == NULL) {
        if (output.name != NULL) {
            new_instance(&arguments.output);
            arguments.output[0] = output;
            arguments.output_len = 1;
        }
    } else {
        struct output_s *lastoutput = &arguments.output[arguments.output_len - 1];
        lastoutput->mode = output.mode;
        if (output.section != NULL) lastoutput->section = output.section;
        lastoutput->longaddr = output.longaddr;
        if (!lastoutput->mapfile) {
            lastoutput->mapname = output.mapname;
            lastoutput->mapfile = output.mapfile;
        }
        lastoutput->mapappend = output.mapappend;
    }

    all_mem2 = check_outputs();
    if (arguments.caseinsensitive == 0) {
        diagnostics.case_symbol = false;
    }
    if (defines_p != arguments.defines.len) {
        arguments.defines.len = defines_p;
        if (defines_p != 0) {
            uint8_t *d = reallocate_array(arguments.defines.data, defines_p);
            if (d != NULL) arguments.defines.data = d;
        }
    }
    if (argc <= my_optind) {
        fputs("Usage: 64tass [OPTIONS...] SOURCES\n"
              "Try '64tass --help' or '64tass --usage' for more information.\n", stderr);
        return -1;
    }
    return my_optind;
}

void destroy_arguments(void) {
    struct include_list_s *include;
    free(arguments.output);
    free(arguments.symbol_output);
    free(arguments.defines.data);
    free(arguments.commandline.data);
    include = arguments.include;
    while (include != NULL) {
        struct include_list_s *tmp = include;
        include = tmp->next;
        free(tmp);
    }
}
