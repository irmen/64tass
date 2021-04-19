/*
    $Id: file.h 2578 2021-04-17 13:33:35Z soci $

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
#include "attributes.h"
#include "stdbool.h"
#include "inttypes.h"
#include "str.h"

typedef enum Encoding_types {
    E_UNKNOWN, E_UTF8, E_UTF16LE, E_UTF16BE, E_ISO
} Encoding_types;

typedef uint32_t filesize_t;

struct file_s {
    const char *name;
    const char *realname;
    str_t base;
    int hash;
    uint8_t *nomacro;
    filesize_t *line;
    linenum_t lines;
    uint8_t *data;    /* data */
    filesize_t len;   /* length */
    uint16_t open;    /* open/not open */
    uint16_t uid;     /* uid */
    unsigned int type;
    int err_no;
    bool read_error;
    bool portable;
    uint8_t pass;
    uint8_t entercount;
    Encoding_types encoding;
};

#define not_in_file(a, b) ((size_t)((a) - (1 ? (b) : (struct file_s *)(void *)(b))->data) >= (b)->len)

struct star_s {
    linenum_t line, vline;
    address_t addr;
    uint8_t pass;
};

static inline bool dash_name(const char *name) {
    return (name[0] == '-' && name[1] == 0);
}

extern struct file_s *openfile(const char *, const char *, unsigned int, const struct str_t *, linepos_t);
extern void closefile(struct file_s*);
extern struct star_s *new_star(linenum_t, bool *);
extern struct star_s *init_star(linenum_t);
extern void destroy_file(void);
extern void init_file(void);
extern FILE *file_open(const char *, const char *);
extern void include_list_add(const char *);
extern size_t get_base(const char *);
extern char *get_path(const struct str_t *, const char *);
extern void makefile(int, char *[], bool);

#endif
