/*
    $Id: unicodedata.h 2946 2022-12-28 22:59:29Z soci $

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
#ifndef UNICODEDATA_H
#define UNICODEDATA_H
#include "inttypes.h"
#include "attributes.h"

#define pr_compat 1
#define id_Start 2
#define id_Continue 4
#define qc_N 8
#define qc_M 16

struct properties_s {
    int16_t decompose;
    int16_t casefold;
    int16_t diarbase;
    uint8_t combclass;
    uint8_t property;
};

extern const int16_t usequences[];
extern const int32_t usequences2[];
extern const uint8_t upropshash1[788];
extern const uint16_t upropshash2[][256];
extern const struct properties_s uproperties[];
extern const uint32_t ucomposed[];
extern const int16_t ucomposing[];

static inline const struct properties_s *uget_property(unichar_t c) {
    return (c < 256 * sizeof upropshash1) ? &uproperties[upropshash2[upropshash1[c >> 8]][c & 0xff]] : uproperties;
}
#endif
