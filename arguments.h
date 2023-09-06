/*
    $Id: arguments.h 3112 2023-09-06 06:34:22Z soci $

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
#ifndef ARGUMENTS_H
#define ARGUMENTS_H
#include "stdbool.h"
#include "inttypes.h"

struct Namespace;

typedef enum Output_types {
    OUTPUT_CBM, OUTPUT_RAW, OUTPUT_NONLINEAR, OUTPUT_FLAT, OUTPUT_XEX,
    OUTPUT_APPLE, OUTPUT_IHEX, OUTPUT_SREC, OUTPUT_MHEX, OUTPUT_PGX, OUTPUT_PGZ
} Output_types;

typedef enum Symbollist_types {
    LABEL_64TASS, LABEL_VICE, LABEL_VICE_NUMERIC, LABEL_DUMP, LABEL_EXPORT,
    LABEL_SIMPLE, LABEL_MESEN
} Symbollist_types;

typedef enum Caret_types {
    CARET_ALWAYS, CARET_MACRO, CARET_NEVER
} Caret_types;

struct argpos_s {
    uint32_t start;
    linenum_t line;
    linecpos_t pos;
};

struct output_s {
    struct argpos_s name_pos;
    const char *name;
    const char *section;
    struct argpos_s mapname_pos;
    const char *mapname;
    struct argpos_s exec_pos;
    uval_t exec;
    Output_types mode;
    bool append;
    bool longaddr;
    bool mapappend;
    bool mapfile;
};

struct error_output_s {
    struct argpos_s name_pos;
    const char *name;
    Caret_types caret;
    bool warning;
    bool no_output;
    bool append;
};

struct symbol_output_s {
    struct argpos_s name_pos;
    const char *name;
    struct argpos_s space_pos;
    struct Namespace *space;
    const char *section;
    const char *add_prefix;
    Symbollist_types mode;
    bool append;
};

struct list_output_s {
    struct argpos_s name_pos;
    const char *name;
    bool monitor;
    bool source;
    bool linenum;
    bool verbose;
    bool append;
};

struct make_output_s {
    struct argpos_s name_pos;
    const char *name;
    bool phony;
    bool append;
};

struct arguments_data_s {
    uint8_t *data;
    size_t len;
};

struct include_list_s {
    struct include_list_s *next;
#if __STDC_VERSION__ >= 199901L
    char path[];
#elif __GNUC__ >= 3
    char path[];
#else
    char path[1];
#endif
};

struct arguments_s {
    bool quiet;
    bool to_ascii;
    bool longbranch;
    bool tasmcomp;
    uint8_t caseinsensitive;
    struct output_s *output;
    size_t output_len;
    const struct cpu_s *cpumode;
    struct symbol_output_s *symbol_output;
    size_t symbol_output_len;
    struct include_list_s *include;
    struct list_output_s list;
    struct make_output_s make;
    struct arguments_data_s defines;
    struct arguments_data_s commandline;
    struct error_output_s error;
    unsigned int tab_size;
};

struct diagnostics_s {
    bool shadow;
    bool strict_bool;
    bool optimize;
    bool implied_reg;
    bool jmp_bug;
    struct {
        bool pc;
        bool mem;
        bool addr;
        bool dpage;
        bool bank0;
        bool pbank;
    } wrap;
    bool label_left;
    bool branch_page;
    bool deprecated;
    bool old_equal;
    bool portable;
    struct {
        bool macro;
        bool consts;
        bool label;
        bool variable;
    } unused;
    bool case_symbol;
    bool immediate;
    bool float_compare;
    bool leading_zeros;
    bool alias;
    bool pitfalls;
    bool star_assign;
    bool ignored;
    bool long_branch;
    bool altmode;
    bool align;
    bool page;
    bool macro_prefix;
    bool float_round;
    bool size_larger;
    bool priority;
};

extern int init_arguments(int *, char ***);
extern void destroy_arguments(void);
extern struct arguments_s arguments;
extern struct diagnostics_s diagnostics, diagnostic_errors;

#endif
