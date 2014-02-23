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
#ifndef _UNICODEDATA_H
#define _UNICODEDATA_H
#include <stdint.h>

#define bp_Other 1
#define bp_Control 2
#define bp_Extend 4
#define bp_SpacingMark 8
#define bp_L 16
#define bp_T 32
#define bp_V 64
#define bp_LV 128
#define bp_LVT 256
#define bp_Regional_Indicator 512
#define bp_MASK 1023
#define pr_compat 1024
#define id_Start 2048
#define id_Continue 4096
#define qc_N 8192
#define qc_M 16384

struct properties_s {
    int16_t decompose;
    int16_t casefold;
    uint8_t combclass;
    int8_t diar;
    int16_t base;
    uint16_t property;
};

extern const int16_t usequences[];
extern const int32_t usequences2[];
extern const uint8_t upropshash1[3600];
extern const uint16_t upropshash2[][256];
extern const struct properties_s uproperties[];
extern const uint32_t ucomposed[];
extern const int16_t ucomposing[];

static inline const struct properties_s *uget_property(uint32_t c) {
    return c < sizeof(upropshash1)*256 ? &uproperties[upropshash2[upropshash1[c >> 8]][c & 0xff]] : uproperties;
}
#endif
