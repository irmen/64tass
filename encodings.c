/*
    $Id: encodings.c 2465 2021-03-06 19:58:00Z soci $

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
#include "encodings.h"
#include <string.h>
#include "inttypes.h"
#include "64tass.h"
#include "encoding.h"
#include "str.h"

struct translate_table_s {
    uint16_t start;
    uint8_t length;
    uint8_t offset;
};

static const struct translate_table_s no_trans[] = {
    {0x0000, 255, 0x00},
};

static const struct translate_table_s petscii_trans[] = {
    {0x0020,  32, 0x20}, /*  -@ */
    {0x0041,  25, 0xc1}, /* A-Z */
    {0x005b,   0, 0x5b}, /* [ */
    {0x005d,   0, 0x5d}, /* ] */
    {0x0061,  25, 0x41}, /* a-z */
    {0x00a3,   0, 0x5c}, /* £ */
    {0x03c0,   0, 0xff}, /* π */
    {0x2190,   0, 0x5f}, /* ← */
    {0x2191,   0, 0x5e}, /* ↑ */
    {0x2500,   0, 0xc0}, /* ─ */
    {0x2502,   0, 0xdd}, /* │ */
    {0x250c,   0, 0xb0}, /* ┌ */
    {0x2510,   0, 0xae}, /* ┐ */
    {0x2514,   0, 0xad}, /* └ */
    {0x2518,   0, 0xbd}, /* ┘ */
    {0x251c,   0, 0xab}, /* ├ */
    {0x2524,   0, 0xb3}, /* ┤ */
    {0x252c,   0, 0xb2}, /* ┬ */
    {0x2534,   0, 0xb1}, /* ┴ */
    {0x253c,   0, 0xdb}, /* ┼ */
    {0x256d,   0, 0xd5}, /* ╭ */
    {0x256e,   0, 0xc9}, /* ╮ */
    {0x256f,   0, 0xcb}, /* ╯ */
    {0x2570,   0, 0xca}, /* ╰ */
    {0x2571,   0, 0xce}, /* ╱ */
    {0x2572,   0, 0xcd}, /* ╲ */
    {0x2573,   0, 0xd6}, /* ╳ */
    {0x2581,   0, 0xa4}, /* ▁ */
    {0x2582,   0, 0xaf}, /* ▂ */
    {0x2583,   0, 0xb9}, /* ▃ */
    {0x2584,   0, 0xa2}, /* ▄ */
    {0x258c,   0, 0xa1}, /* ▌ */
    {0x258d,   0, 0xb5}, /* ▍ */
    {0x258e,   0, 0xb4}, /* ▎ */
    {0x258f,   0, 0xa5}, /* ▏ */
    {0x2592,   0, 0xa6}, /* ▒ */
    {0x2594,   0, 0xa3}, /* ▔ */
    {0x2595,   0, 0xa7}, /* ▕ */
    {0x2596,   0, 0xbb}, /* ▖ */
    {0x2597,   0, 0xac}, /* ▗ */
    {0x2598,   0, 0xbe}, /* ▘ */
    {0x259a,   0, 0xbf}, /* ▚ */
    {0x259d,   0, 0xbc}, /* ▝ */
    {0x25cb,   0, 0xd7}, /* ○ */
    {0x25cf,   0, 0xd1}, /* ● */
    {0x25e4,   0, 0xa9}, /* ◤ */
    {0x25e5,   0, 0xdf}, /* ◥ */
    {0x2660,   0, 0xc1}, /* ♠ */
    {0x2663,   0, 0xd8}, /* ♣ */
    {0x2665,   0, 0xd3}, /* ♥ */
    {0x2666,   0, 0xda}, /* ♦ */
    {0x2713,   0, 0xba}, /* ✓ */
    {0xfb70,   0, 0xd4}, /* 🭰 */
    {0xfb71,   0, 0xc7}, /* 🭱 */
    {0xfb72,   0, 0xc2}, /* 🭲 */
    {0xfb73,   0, 0xdd}, /* 🭳 */
    {0xfb74,   0, 0xc8}, /* 🭴 */
    {0xfb75,   0, 0xd9}, /* 🭵 */
    {0xfb76,   0, 0xc5}, /* 🭶 */
    {0xfb77,   0, 0xc4}, /* 🭷 */
    {0xfb78,   0, 0xc3}, /* 🭸 */
    {0xfb79,   0, 0xc0}, /* 🭹 */
    {0xfb7a,   0, 0xc6}, /* 🭺 */
    {0xfb7b,   0, 0xd2}, /* 🭻 */
    {0xfb7c,   0, 0xcc}, /* 🭼 */
    {0xfb7d,   0, 0xcf}, /* 🭽 */
    {0xfb7e,   0, 0xd0}, /* 🭾 */
    {0xfb7f,   0, 0xba}, /* 🭿 */
    {0xfb82,   0, 0xb7}, /* 🮂 */
    {0xfb83,   0, 0xb8}, /* 🮃 */
    {0xfb87,   0, 0xaa}, /* 🮇 */
    {0xfb88,   0, 0xb6}, /* 🮈 */
    {0xfb8c,   0, 0xdc}, /* 🮌 */
    {0xfb8f,   0, 0xa8}, /* 🮏 */
    {0xfb95,   0, 0xff}, /* 🮕 */
    {0xfb98,   0, 0xdf}, /* 🮘 */
    {0xfb99,   0, 0xa9}, /* 🮙 */
};

/* PETSCII codes, must be sorted */
const char *petscii_esc =
    "\x07" "{bell}\0"
    "\x90" "{black}\0"
    "\x90" "{blk}\0"
    "\x1f" "{blue}\0"
    "\x1f" "{blu}\0"
    "\x95" "{brn}\0"
    "\x95" "{brown}\0"
    "\xdf" "{cbm-*}\0"
    "\xa6" "{cbm-+}\0"
    "\xdc" "{cbm--}\0"
    "\x30" "{cbm-0}\0"
    "\x81" "{cbm-1}\0"
    "\x95" "{cbm-2}\0"
    "\x96" "{cbm-3}\0"
    "\x97" "{cbm-4}\0"
    "\x98" "{cbm-5}\0"
    "\x99" "{cbm-6}\0"
    "\x9a" "{cbm-7}\0"
    "\x9b" "{cbm-8}\0"
    "\x29" "{cbm-9}\0"
    "\xa4" "{cbm-@}\0"
    "\xde" "{cbm-^}\0"
    "\xb0" "{cbm-a}\0"
    "\xbf" "{cbm-b}\0"
    "\xbc" "{cbm-c}\0"
    "\xac" "{cbm-d}\0"
    "\xb1" "{cbm-e}\0"
    "\xbb" "{cbm-f}\0"
    "\xa5" "{cbm-g}\0"
    "\xb4" "{cbm-h}\0"
    "\xa2" "{cbm-i}\0"
    "\xb5" "{cbm-j}\0"
    "\xa1" "{cbm-k}\0"
    "\xb6" "{cbm-l}\0"
    "\xa7" "{cbm-m}\0"
    "\xaa" "{cbm-n}\0"
    "\xb9" "{cbm-o}\0"
    "\xa8" "{cbm-pound}\0"
    "\xaf" "{cbm-p}\0"
    "\xab" "{cbm-q}\0"
    "\xb2" "{cbm-r}\0"
    "\xae" "{cbm-s}\0"
    "\xa3" "{cbm-t}\0"
    "\xde" "{cbm-up arrow}\0"
    "\xb8" "{cbm-u}\0"
    "\xbe" "{cbm-v}\0"
    "\xb3" "{cbm-w}\0"
    "\xbd" "{cbm-x}\0"
    "\xb7" "{cbm-y}\0"
    "\xad" "{cbm-z}\0"
    "\x93" "{clear}\0"
    "\x93" "{clr}\0"
    "\x92" "{control-0}\0"
    "\x90" "{control-1}\0"
    "\x05" "{control-2}\0"
    "\x1c" "{control-3}\0"
    "\x9f" "{control-4}\0"
    "\x9c" "{control-5}\0"
    "\x1e" "{control-6}\0"
    "\x1f" "{control-7}\0"
    "\x9e" "{control-8}\0"
    "\x12" "{control-9}\0"
    "\x1b" "{control-:}\0"
    "\x1d" "{control-;}\0"
    "\x1f" "{control-=}\0"
    "\x00" "{control-@}\0"
    "\x01" "{control-a}\0"
    "\x02" "{control-b}\0"
    "\x03" "{control-c}\0"
    "\x04" "{control-d}\0"
    "\x05" "{control-e}\0"
    "\x06" "{control-f}\0"
    "\x07" "{control-g}\0"
    "\x08" "{control-h}\0"
    "\x09" "{control-i}\0"
    "\x0a" "{control-j}\0"
    "\x0b" "{control-k}\0"
    "\x06" "{control-left arrow}\0"
    "\x0c" "{control-l}\0"
    "\x0d" "{control-m}\0"
    "\x0e" "{control-n}\0"
    "\x0f" "{control-o}\0"
    "\x1c" "{control-pound}\0"
    "\x10" "{control-p}\0"
    "\x11" "{control-q}\0"
    "\x12" "{control-r}\0"
    "\x13" "{control-s}\0"
    "\x14" "{control-t}\0"
    "\x1e" "{control-up arrow}\0"
    "\x15" "{control-u}\0"
    "\x16" "{control-v}\0"
    "\x17" "{control-w}\0"
    "\x18" "{control-x}\0"
    "\x19" "{control-y}\0"
    "\x1a" "{control-z}\0"
    "\x0d" "{cr}\0"
    "\x9f" "{cyan}\0"
    "\x9f" "{cyn}\0"
    "\x14" "{delete}\0"
    "\x14" "{del}\0"
    "\x08" "{dish}\0"
    "\x11" "{down}\0"
    "\x09" "{ensh}\0"
    "\x1b" "{esc}\0"
    "\x82" "{f10}\0"
    "\x84" "{f11}\0"
    "\x8f" "{f12}\0"
    "\x85" "{f1}\0"
    "\x89" "{f2}\0"
    "\x86" "{f3}\0"
    "\x8a" "{f4}\0"
    "\x87" "{f5}\0"
    "\x8b" "{f6}\0"
    "\x88" "{f7}\0"
    "\x8c" "{f8}\0"
    "\x80" "{f9}\0"
    "\x97" "{gray1}\0"
    "\x98" "{gray2}\0"
    "\x9b" "{gray3}\0"
    "\x1e" "{green}\0"
    "\x97" "{grey1}\0"
    "\x98" "{grey2}\0"
    "\x9b" "{grey3}\0"
    "\x1e" "{grn}\0"
    "\x97" "{gry1}\0"
    "\x98" "{gry2}\0"
    "\x9b" "{gry3}\0"
    "\x84" "{help}\0"
    "\x13" "{home}\0"
    "\x94" "{insert}\0"
    "\x94" "{inst}\0"
    "\x9a" "{lblu}\0"
    "\x5f" "{left arrow}\0"
    "\x9d" "{left}\0"
    "\x0a" "{lf}\0"
    "\x99" "{lgrn}\0"
    "\x0e" "{lower case}\0"
    "\x96" "{lred}\0"
    "\x9a" "{lt blue}\0"
    "\x99" "{lt green}\0"
    "\x96" "{lt red}\0"
    "\x81" "{orange}\0"
    "\x81" "{orng}\0"
    "\xff" "{pi}\0"
    "\x5c" "{pound}\0"
    "\x9c" "{purple}\0"
    "\x9c" "{pur}\0"
    "\x1c" "{red}\0"
    "\x0d" "{return}\0"
    "\x92" "{reverse off}\0"
    "\x12" "{reverse on}\0"
    "\x1d" "{rght}\0"
    "\x1d" "{right}\0"
    "\x83" "{run}\0"
    "\x92" "{rvof}\0"
    "\x12" "{rvon}\0"
    "\x92" "{rvs off}\0"
    "\x12" "{rvs on}\0"
    "\x8d" "{shift return}\0"
    "\xc0" "{shift-*}\0"
    "\xdb" "{shift-+}\0"
    "\x3c" "{shift-,}\0"
    "\xdd" "{shift--}\0"
    "\x3e" "{shift-.}\0"
    "\x3f" "{shift-/}\0"
    "\x30" "{shift-0}\0"
    "\x21" "{shift-1}\0"
    "\x22" "{shift-2}\0"
    "\x23" "{shift-3}\0"
    "\x24" "{shift-4}\0"
    "\x25" "{shift-5}\0"
    "\x26" "{shift-6}\0"
    "\x27" "{shift-7}\0"
    "\x28" "{shift-8}\0"
    "\x29" "{shift-9}\0"
    "\x5b" "{shift-:}\0"
    "\x5d" "{shift-;}\0"
    "\xba" "{shift-@}\0"
    "\xde" "{shift-^}\0"
    "\xc1" "{shift-a}\0"
    "\xc2" "{shift-b}\0"
    "\xc3" "{shift-c}\0"
    "\xc4" "{shift-d}\0"
    "\xc5" "{shift-e}\0"
    "\xc6" "{shift-f}\0"
    "\xc7" "{shift-g}\0"
    "\xc8" "{shift-h}\0"
    "\xc9" "{shift-i}\0"
    "\xca" "{shift-j}\0"
    "\xcb" "{shift-k}\0"
    "\xcc" "{shift-l}\0"
    "\xcd" "{shift-m}\0"
    "\xce" "{shift-n}\0"
    "\xcf" "{shift-o}\0"
    "\xa9" "{shift-pound}\0"
    "\xd0" "{shift-p}\0"
    "\xd1" "{shift-q}\0"
    "\xd2" "{shift-r}\0"
    "\xa0" "{shift-space}\0"
    "\xd3" "{shift-s}\0"
    "\xd4" "{shift-t}\0"
    "\xde" "{shift-up arrow}\0"
    "\xd5" "{shift-u}\0"
    "\xd6" "{shift-v}\0"
    "\xd7" "{shift-w}\0"
    "\xd8" "{shift-x}\0"
    "\xd9" "{shift-y}\0"
    "\xda" "{shift-z}\0"
    "\x20" "{space}\0"
    "\x8d" "{sret}\0"
    "\x03" "{stop}\0"
    "\x0e" "{swlc}\0"
    "\x8e" "{swuc}\0"
    "\x09" "{tab}\0"
    "\x5e" "{up arrow}\0"
    "\x09" "{up/lo lock off}\0"
    "\x08" "{up/lo lock on}\0"
    "\x8e" "{upper case}\0"
    "\x91" "{up}\0"
    "\x05" "{white}\0"
    "\x05" "{wht}\0"
    "\x9e" "{yellow}\0"
    "\x9e" "{yel}\0"
    "\x00" "\0";

static const struct translate_table_s petscii_screen_trans[] = {
    {0x0020,  31, 0x20}, /*  -? */
    {0x0040,   0, 0x00}, /* @ */
    {0x0041,  25, 0x41}, /* A-Z */
    {0x005b,   0, 0x1b}, /* [ */
    {0x005d,   0, 0x1d}, /* ] */
    {0x0061,  25, 0x01}, /* a-z */
    {0x00a3,   0, 0x1c}, /* £ */
    {0x03c0,   0, 0x5e}, /* π */
    {0x2190,   0, 0x1f}, /* ← */
    {0x2191,   0, 0x1e}, /* ↑ */
    {0x2500,   0, 0x40}, /* ─ */
    {0x2502,   0, 0x5d}, /* │ */
    {0x250c,   0, 0x70}, /* ┌ */
    {0x2510,   0, 0x6e}, /* ┐ */
    {0x2514,   0, 0x6d}, /* └ */
    {0x2518,   0, 0x7d}, /* ┘ */
    {0x251c,   0, 0x6b}, /* ├ */
    {0x2524,   0, 0x73}, /* ┤ */
    {0x252c,   0, 0x72}, /* ┬ */
    {0x2534,   0, 0x71}, /* ┴ */
    {0x253c,   0, 0x5b}, /* ┼ */
    {0x256d,   0, 0x55}, /* ╭ */
    {0x256e,   0, 0x49}, /* ╮ */
    {0x256f,   0, 0x4b}, /* ╯ */
    {0x2570,   0, 0x4a}, /* ╰ */
    {0x2571,   0, 0x4e}, /* ╱ */
    {0x2572,   0, 0x4d}, /* ╲ */
    {0x2573,   0, 0x56}, /* ╳ */
    {0x2581,   0, 0x64}, /* ▁ */
    {0x2582,   0, 0x6f}, /* ▂ */
    {0x2583,   0, 0x79}, /* ▃ */
    {0x2584,   0, 0x62}, /* ▄ */
    {0x258c,   0, 0x61}, /* ▌ */
    {0x258d,   0, 0x75}, /* ▍ */
    {0x258e,   0, 0x74}, /* ▎ */
    {0x258f,   0, 0x65}, /* ▏ */
    {0x2592,   0, 0x66}, /* ▒ */
    {0x2594,   0, 0x63}, /* ▔ */
    {0x2595,   0, 0x67}, /* ▕ */
    {0x2596,   0, 0x7b}, /* ▖ */
    {0x2597,   0, 0x6c}, /* ▗ */
    {0x2598,   0, 0x7e}, /* ▘ */
    {0x259a,   0, 0x7f}, /* ▚ */
    {0x259d,   0, 0x7c}, /* ▝ */
    {0x25cb,   0, 0x57}, /* ○ */
    {0x25cf,   0, 0x51}, /* ● */
    {0x25e4,   0, 0x69}, /* ◤ */
    {0x25e5,   0, 0x5f}, /* ◥ */
    {0x2660,   0, 0x41}, /* ♠ */
    {0x2663,   0, 0x58}, /* ♣ */
    {0x2665,   0, 0x53}, /* ♥ */
    {0x2666,   0, 0x5a}, /* ♦ */
    {0x2713,   0, 0x7a}, /* ✓ */
    {0xfb70,   0, 0x54}, /* 🭰 */
    {0xfb71,   0, 0x47}, /* 🭱 */
    {0xfb72,   0, 0x42}, /* 🭲 */
    {0xfb73,   0, 0x5d}, /* 🭳 */
    {0xfb74,   0, 0x48}, /* 🭴 */
    {0xfb75,   0, 0x59}, /* 🭵 */
    {0xfb76,   0, 0x45}, /* 🭶 */
    {0xfb77,   0, 0x44}, /* 🭷 */
    {0xfb78,   0, 0x43}, /* 🭸 */
    {0xfb79,   0, 0x40}, /* 🭹 */
    {0xfb7a,   0, 0x46}, /* 🭺 */
    {0xfb7b,   0, 0x52}, /* 🭻 */
    {0xfb7c,   0, 0x4c}, /* 🭼 */
    {0xfb7d,   0, 0x4f}, /* 🭽 */
    {0xfb7e,   0, 0x50}, /* 🭾 */
    {0xfb7f,   0, 0x7a}, /* 🭿 */
    {0xfb82,   0, 0x77}, /* 🮂 */
    {0xfb83,   0, 0x78}, /* 🮃 */
    {0xfb87,   0, 0x6a}, /* 🮇 */
    {0xfb88,   0, 0x76}, /* 🮈 */
    {0xfb8c,   0, 0x5c}, /* 🮌 */
    {0xfb8f,   0, 0x68}, /* 🮏 */
    {0xfb95,   0, 0x5e}, /* 🮕 */
    {0xfb98,   0, 0x5f}, /* 🮘 */
    {0xfb99,   0, 0x69}, /* 🮙 */
};

/* petscii screen codes, must be sorted */
static const char *petscii_screen_esc =
    "\x5f" "{cbm-*}\0"
    "\x66" "{cbm-+}\0"
    "\x5c" "{cbm--}\0"
    "\x30" "{cbm-0}\0"
    "\x29" "{cbm-9}\0"
    "\x64" "{cbm-@}\0"
    "\x5e" "{cbm-^}\0"
    "\x70" "{cbm-a}\0"
    "\x7f" "{cbm-b}\0"
    "\x7c" "{cbm-c}\0"
    "\x6c" "{cbm-d}\0"
    "\x71" "{cbm-e}\0"
    "\x7b" "{cbm-f}\0"
    "\x65" "{cbm-g}\0"
    "\x74" "{cbm-h}\0"
    "\x62" "{cbm-i}\0"
    "\x75" "{cbm-j}\0"
    "\x61" "{cbm-k}\0"
    "\x76" "{cbm-l}\0"
    "\x67" "{cbm-m}\0"
    "\x6a" "{cbm-n}\0"
    "\x79" "{cbm-o}\0"
    "\x68" "{cbm-pound}\0"
    "\x6f" "{cbm-p}\0"
    "\x6b" "{cbm-q}\0"
    "\x72" "{cbm-r}\0"
    "\x6e" "{cbm-s}\0"
    "\x63" "{cbm-t}\0"
    "\x5e" "{cbm-up arrow}\0"
    "\x78" "{cbm-u}\0"
    "\x7e" "{cbm-v}\0"
    "\x73" "{cbm-w}\0"
    "\x7d" "{cbm-x}\0"
    "\x77" "{cbm-y}\0"
    "\x6d" "{cbm-z}\0"
    "\x1f" "{left arrow}\0"
    "\x5e" "{pi}\0"
    "\x1c" "{pound}\0"
    "\x40" "{shift-*}\0"
    "\x5b" "{shift-+}\0"
    "\x3c" "{shift-,}\0"
    "\x5d" "{shift--}\0"
    "\x3e" "{shift-.}\0"
    "\x3f" "{shift-/}\0"
    "\x30" "{shift-0}\0"
    "\x21" "{shift-1}\0"
    "\x22" "{shift-2}\0"
    "\x23" "{shift-3}\0"
    "\x24" "{shift-4}\0"
    "\x25" "{shift-5}\0"
    "\x26" "{shift-6}\0"
    "\x27" "{shift-7}\0"
    "\x28" "{shift-8}\0"
    "\x29" "{shift-9}\0"
    "\x1b" "{shift-:}\0"
    "\x1d" "{shift-;}\0"
    "\x7a" "{shift-@}\0"
    "\x5e" "{shift-^}\0"
    "\x41" "{shift-a}\0"
    "\x42" "{shift-b}\0"
    "\x43" "{shift-c}\0"
    "\x44" "{shift-d}\0"
    "\x45" "{shift-e}\0"
    "\x46" "{shift-f}\0"
    "\x47" "{shift-g}\0"
    "\x48" "{shift-h}\0"
    "\x49" "{shift-i}\0"
    "\x4a" "{shift-j}\0"
    "\x4b" "{shift-k}\0"
    "\x4c" "{shift-l}\0"
    "\x4d" "{shift-m}\0"
    "\x4e" "{shift-n}\0"
    "\x4f" "{shift-o}\0"
    "\x69" "{shift-pound}\0"
    "\x50" "{shift-p}\0"
    "\x51" "{shift-q}\0"
    "\x52" "{shift-r}\0"
    "\x60" "{shift-space}\0"
    "\x53" "{shift-s}\0"
    "\x54" "{shift-t}\0"
    "\x5e" "{shift-up arrow}\0"
    "\x55" "{shift-u}\0"
    "\x56" "{shift-v}\0"
    "\x57" "{shift-w}\0"
    "\x58" "{shift-x}\0"
    "\x59" "{shift-y}\0"
    "\x5a" "{shift-z}\0"
    "\x20" "{space}\0"
    "\x1e" "{up arrow}\0"
    "\x00" "\0";

static const struct translate_table_s no_screen_trans[] = {
    {0x0000,  31, 0x80},
    {0x0020,  31, 0x20},
    {0x0040,  31, 0x00},
    {0x0060,  31, 0x40},
    {0x0080,  31, 0x80},
    {0x00A0,  31, 0x60},
    {0x00C0,  62, 0x40},
    {0x00FF,   0, 0x5E},
};

static void add_trans(struct encoding_s *enc, const struct translate_table_s *table, size_t ln) {
    size_t i;
    struct character_range_s range;
    struct linepos_s nopoint = {0, 0};
    uint8_t oldpass = pass;
    pass = 255;
    for (i = 0; i < ln; i++) {
        uint32_t start = table[i].start;
        if (start >= 0x8000) start += 0x10000;
        range.start = start;
        range.end = (start + table[i].length) & 0xffffff;
        range.offset = table[i].offset;
        new_trans(enc, &range, &nopoint);
    }
    pass = oldpass;
}

void enctables(struct encoding_s *enc, str_t *name, bool toascii) {
    bool oldfixeddig = fixeddig;
    fixeddig = false;
    if (name->len == 4 && memcmp(name->data, "none", 4) == 0) {
        if (!toascii) {
            add_trans(enc, no_trans, lenof(no_trans));
        } else {
            add_esc(enc, petscii_esc);
            add_trans(enc, petscii_trans, lenof(petscii_trans));
        }
    } else if (name->len == 6 && memcmp(name->data, "screen", 6) == 0) {
        if (!toascii) {
            add_trans(enc, no_screen_trans, lenof(no_screen_trans));
        } else {
            add_esc(enc, petscii_screen_esc);
            add_trans(enc, petscii_screen_trans, lenof(petscii_screen_trans));
        }
    }
    fixeddig = oldfixeddig;
}
