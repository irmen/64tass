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
#ifndef _CODEOBJ_H
#define _CODEOBJ_H

enum dtype_e {
    D_DINT = -4,
    D_LINT = -3,
    D_INT = -2,
    D_CHAR = -1,
    D_NONE = 0,
    D_BYTE = 1,
    D_WORD = 2,
    D_LONG = 3,
    D_DWORD = 4
};

extern obj_t CODE_OBJ;

typedef struct {
    size_t size;
    uint8_t pass;
    uint8_t apass;
    signed char dtype;
    struct value_s *addr;
    const struct memblocks_s *mem;
    size_t memp;
    size_t membp;
    struct label_s *label;
} code_t;

extern void codeobj_init(void);
#endif
