/*
    $Id: isnprintf.h 2425 2021-02-28 07:40:41Z soci $

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
#ifndef ISNPRINTF_H
#define ISNPRINTF_H
#include "attributes.h"
#include "inttypes.h"

struct Funcargs;
struct oper_s;

extern MUST_CHECK struct Obj *isnprintf(struct oper_s *);
#endif
