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
#ifndef ARGUMENTS_H
#define ARGUMENTS_H
#include "stdbool.h"
#include "inttypes.h"

enum output_mode_e {
    OUTPUT_CBM, OUTPUT_RAW, OUTPUT_NONLINEAR, OUTPUT_FLAT, OUTPUT_XEX,
    OUTPUT_APPLE, OUTPUT_IHEX, OUTPUT_SREC
};

enum label_mode_e {
    LABEL_64TASS, LABEL_VICE, LABEL_DUMP
};

struct file_s;

struct arguments_s {
    bool warning;
    bool caret;
    bool quiet;
    bool toascii;
    bool monitor;
    bool source;
    bool linenum;
    bool longbranch;
    bool longaddr;
    bool tasmcomp;
    bool shadow_check;
    bool verbose;
    uint8_t caseinsensitive;
    const char *output;
    const struct cpu_s *cpumode;
    const char *label;
    const char *list;
    const char *make;
    const char *error;
    enum output_mode_e output_mode;
    uint8_t tab_size;
    enum label_mode_e label_mode;
};

extern int testarg(int, char **, struct file_s *);
extern struct arguments_s arguments;

#endif
