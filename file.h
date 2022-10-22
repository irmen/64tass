/*
    $Id: file.h 2833 2022-10-22 10:01:11Z soci $

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
#ifndef FILE_H
#define FILE_H
#include <stdio.h>
#include "stdbool.h"
#include "inttypes.h"

typedef enum Encoding_types {
    E_UNKNOWN, E_UTF8, E_UTF16LE, E_UTF16BE, E_ISO, E_RAW
} Encoding_types;

typedef enum File_open_type {
    FILE_OPEN_STDIN, FILE_OPEN_BINARY, FILE_OPEN_DEFINES, FILE_OPEN_SOURCE
} File_open_type;

typedef uint32_t filesize_t;

struct file_data_s {
    uint8_t *data;
    filesize_t len;
    bool read;
};

struct file_s {
    const char *name;
    int hash;
    uint8_t *nomacro;
    filesize_t *line;
    linenum_t lines;
    struct file_data_s source;
    struct file_data_s binary;
    int err_no;
    bool open;
    bool read_error;
    bool portable;
    bool cmdline;
    uint8_t pass;
    uint8_t entercount;
    uint16_t uid;
    Encoding_types encoding;
};

#define not_in_file(a, b) ((size_t)((a) - (1 ? (b) : (struct file_s *)(void *)(b))->source.data) >= (b)->source.len)

struct star_s {
    linenum_t line, vline;
    address_t addr;
    uint8_t pass;
};

struct str_t;
struct file_list_s;

extern struct file_s *file_open(const struct str_t *, const struct file_list_s *, File_open_type, linepos_t);
extern struct star_s *new_star(linenum_t);
extern struct star_s *init_star(linenum_t);
extern void destroy_file(void);
extern void init_file(void);
extern void makefile(int, char *[], bool);

#endif
