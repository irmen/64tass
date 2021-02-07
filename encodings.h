/*
    $Id: encodings.h 2292 2021-01-24 21:43:14Z soci $

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
#ifndef ENCODINGS_H
#define ENCODINGS_H
#include "stdbool.h"

struct encoding_s;
struct str_t;

extern const char *petscii_esc;
#define identmap (const uint8_t *)petscii_esc

extern void enctables(struct encoding_s *, struct str_t *, bool);
#endif
