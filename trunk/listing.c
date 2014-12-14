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
#include "listing.h"
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "file.h"
#include "error.h"
#include "64tass.h"
#include "unicode.h"
#include "misc.h"
#include "section.h"
#include "instruction.h"
#include "obj.h"
#include "values.h"
#include "mem.h"

#define VERBOSE 0
#define HEX_COLUMN 9
#define MONITOR_COLUMN 24
#define SOURCE_COLUMN 40

unsigned int nolisting;   /* listing */
const uint8_t *llist = NULL;
static FILE* flist = NULL;      /* listfile */
static const char *hex = "0123456789abcdef";

static int padding(int l, int t) {
    if (l >= t) {putc('\n', flist); l = 0;}
    if (arguments.tab_size > 1) {
        int l2 = l - l % arguments.tab_size;
        while (l2 + arguments.tab_size <= t) { l2 += arguments.tab_size; l = l2; putc('\t', flist);} 
    }
    while (l < t) { l++; putc(' ', flist);} 
    return l;
}

static inline char *out_hex(char *s, unsigned int c) {
    *s++ = hex[(c >> 4) & 15];
    *s++ = hex[c & 15];
    return s;
}

static char *out_byte(char *s, unsigned int adr) {
    *s++ = '$';
    return out_hex(s, adr);
}

static char *out_word(char *s, unsigned int adr) {
    s = out_byte(s, adr >> 8);
    return out_hex(s, adr);
}

static char *out_long(char *s, unsigned int adr) {
    s = out_word(s, adr >> 8);
    return out_hex(s, adr);
}

static char *out_zp(char *s, unsigned int c) {
    unsigned int adr = (uint16_t)(((uint8_t)c) + dpage);
    return (adr >= 0x100) ? out_word(s, adr) : out_byte(s, adr);
}

static char *out_db(char *s, unsigned int adr) {
    *s++ = '$';
    if (databank) s = out_hex(s, databank);
    s = out_hex(s, adr >> 8);
    return out_hex(s, adr);
}

static char *out_pb(char *s, unsigned int adr) {
    *s++ = '$';
    if (current_section->l_address & 0xff0000) s = out_hex(s, current_section->l_address >> 16);
    s = out_hex(s, adr >> 8);
    return out_hex(s, adr);
}

static char *out_bit(char *s, unsigned int cod, unsigned int c) {
    *s++ = 0x30 + ((cod >> 4) & 7); 
    *s++ = ',';
    return out_zp(s, c);
}

void listing_open(const char *filename, int argc, char *argv[]) {
    struct linepos_s nopoint = {0, 0};
    time_t t;
    const char *prgname;
    int i;
    if (filename[0] == '-' && !filename[1]) {
        flist = stdout;
    } else {
        if (!(flist=file_open(filename,"wt"))) err_msg_file(ERROR_CANT_DUMP_LST, filename, &nopoint);
    }
    fputs("\n; 64tass Turbo Assembler Macro V" VERSION " listing file\n;", flist);
    prgname = *argv;
    if (prgname) {
        const char *newp = strrchr(prgname, '/');
        if (newp) prgname = newp + 1;
#if defined _WIN32 || defined __WIN32__ || defined __EMX__ || defined __DJGPP__
        newp = strrchr(prgname, '\\');
        if (newp) prgname = newp + 1;
#endif
    }
    for (i = 0; i < argc; i++) {
        putc(' ', flist);
        argv_print(i ? argv[i] : prgname, flist);
    }
    fputs("\n; ", flist);
    time(&t); fputs(ctime(&t), flist);
}

void listing_close(void) {
    fputs("\n;******  End of listing\n", flist);
    if (flist != stdout) fclose(flist);
    else fflush(flist);
    flist = NULL;
}

static void printllist(int l) {
    if (!nolisting && flist && arguments.source && !temporary_label_branch) {
        if (llist) {
            const uint8_t *c = llist;
            while (*c == 0x20 || *c == 0x09) c++;
            if (*c) {
                padding(l, SOURCE_COLUMN);
                printable_print(llist, flist);
            }
            llist = NULL;
        }
        putc('\n', flist);
    }
}

void listing_equal(const value_t val) {
    if (!nolisting && flist && arguments.source && !temporary_label_branch) {
        int l;
        putc('=', flist);
        l = val_print(val, flist) + 1;
        printllist(l);
    }
}

static int printaddr(char pre, address_t addr) {
    char str[8], *s = str;
    *s++ = pre;
    if (all_mem != 0xffff) s = out_hex(s, addr >> 16);
    s = out_hex(s, addr >> 8);
    s = out_hex(s, addr);
    *s = 0;
    fputs(str, flist);
    return s - str;
}

void listing_line(linecpos_t pos) {
    if (!nolisting && flist && arguments.source && !temporary_label_branch) {
        if (llist) {
            size_t i = 0;
            int l;
            while (i < pos && (llist[i] == 0x20 || llist[i] == 0x09)) i++;
            l = (i < pos) ? printaddr('.', current_section->address) : 0;
            if (VERBOSE) {
                if (llist[i]) {
                    padding(l, SOURCE_COLUMN);
                    printable_print(llist, flist);
                }
                putc('\n', flist);
            } else {
                if (l) {
                    while (llist[pos-1] == 0x20 || llist[pos-1] == 0x09) pos--;
                    padding(l, SOURCE_COLUMN);
                    printable_print2(llist, flist, pos);
                    putc('\n', flist);
                }
            }
            llist = NULL;
        }
    }
}

void listing_line_cut(linecpos_t pos) {
    if (!nolisting && flist && arguments.source && !temporary_label_branch) {
        if (llist) {
            size_t i = 0;
            while (i < pos && (llist[i] == 0x20 || llist[i] == 0x09)) i++;
            if (i < pos) {
                int l = printaddr('.', current_section->address);
                while (llist[pos-1] == 0x20 || llist[pos-1] == 0x09) pos--;
                padding(l, SOURCE_COLUMN);
                printable_print2(llist, flist, pos);
                putc('\n', flist);
            }
            llist = NULL;
        }
    }
}

void listing_line_cut2(linecpos_t pos) {
    if (VERBOSE) {
        if (!nolisting && flist && arguments.source && !temporary_label_branch) {
            if (llist) {
                padding(0, SOURCE_COLUMN);
                caret_print(llist, flist, pos);
                printable_print(llist + pos, flist);
                putc('\n', flist);
                llist = NULL;
            }
        }
    }
}

static const uint16_t *disasm;
static const uint32_t *mnemonic;    /* mnemonics */

void listing_set_cpumode(const struct cpu_s *cpumode) {
    disasm = cpumode->disasm;
    mnemonic = cpumode->mnemonic; 
}

void listing_instr(uint8_t cod, uint32_t adr, int ln) {
    if (!nolisting && flist && !temporary_label_branch) {
        int i, l;

        l = printaddr('.', current_section->address - ln - 1);
        if (current_section->dooutput) {
            char str[32], *s;
            if (ln >= 0) {
                uint32_t temp = adr;
                l = padding(l, HEX_COLUMN);
                s = out_hex(str, cod ^ outputeor);
                for (i = 0; i < ln; i++) {*s++ = ' '; s = out_hex(s, temp); temp >>= 8;}
                *s = 0;
                fputs(str, flist);
                l += s - str;
            }
            if (arguments.monitor) {
                if (ln >= 0) {
                    const char *post = "";
                    uint32_t mnem = mnemonic[disasm[cod] & 0xff];
                    l = padding(l, MONITOR_COLUMN);
                    s = str;
                    for (i = 16; i >= 0; i -= 8) *s++ = mnem >> i;
                    *s++ = ' ';

                    switch (disasm[cod] >> 8) {
                    case ADR_IMPLIED: s--; break;
                    case ADR_REG: post = "a"; break;
                    case ADR_IMMEDIATE:
                        switch (ln) {
                        default: s--; break;
                        case 1: *s++ = '#'; s = out_byte(s, adr); break;
                        case 2: *s++ = '#'; s = out_word(s, adr); break;
                        }
                        break;
                    case ADR_LONG: s = out_long(s, adr); break;
                    case ADR_ADDR: s = (cod == 0x20 || cod == 0x4c) ? out_pb(s, adr) : out_db(s, adr); break;
                    case ADR_ZP: s = out_zp(s, adr); break;
                    case ADR_BIT_ZP: s = out_bit(s, cod, adr); break;
                    case ADR_LONG_X: s = out_long(s, adr); *s++ = ','; *s++ = 'x'; break;
                    case ADR_ADDR_X: s = out_db(s, adr); post = ",x"; break;
                    case ADR_ZP_X: s = out_zp(s, adr); post = ",x"; break;
                    case ADR_ADDR_X_I: *s++ = '('; s = out_pb(s, adr); *s++ = ','; *s++ = 'x'; *s++ = ')'; break;
                    case ADR_ZP_X_I: *s++ = '('; s = out_zp(s, adr); post = ",x)"; break;
                    case ADR_ZP_S: s = out_byte(s, adr); post = ",s"; break;
                    case ADR_ZP_S_I_Y: *s++ = '('; s = out_byte(s, adr); post = ",s),y"; break;
                    case ADR_ZP_R: s = out_byte(s, adr); post = ",r"; break;
                    case ADR_ZP_R_I_Y: *s++ = '('; s = out_byte(s, adr); post = ",r),y"; break;
                    case ADR_ADDR_Y: s = out_db(s, adr); post = ",y"; break;
                    case ADR_ZP_Y: s = out_zp(s, adr); post = ",y"; break;
                    case ADR_ZP_LI_Y: *s++ = '['; s = out_zp(s, adr); post = "],y"; break;
                    case ADR_ZP_I_Y: *s++ = '('; s = out_zp(s, adr); post = "),y"; break;
                    case ADR_ZP_I_Z: *s++ = '('; s = out_zp(s, adr); post = "),z"; break;
                    case ADR_ADDR_LI: *s++ = '['; s = out_word(s, adr); post = "]"; break;
                    case ADR_ZP_LI: *s++ = '['; s = out_zp(s, adr); post = "]"; break;
                    case ADR_ADDR_I: *s++ = '('; s = out_word(s, adr); post = ")"; break;
                    case ADR_ZP_I: *s++ = '('; s = out_zp(s, adr); post = ")"; break;
                    case ADR_REL: if (ln > 0) s = out_pb(s, ((int8_t)adr) + current_section->l_address); else s--; break;
                    case ADR_BIT_ZP_REL: 
                        s = out_bit(s, cod, adr);
                        s = out_pb(s, ((int8_t)(adr >> 8)) + current_section->l_address);
                        break;
                    case ADR_REL_L: s = out_pb(s, ((int16_t)adr) + current_section->l_address); break;
                    case ADR_MOVE: s = out_byte(s, adr >> 8); *s++ = ','; s = out_byte(s, adr);
                    case ADR_LEN: break;/* not an addressing mode */
                    }
                    while (*post) *s++ = *post++;
                    *s = 0;
                    fputs(str, flist);
                    l += s - str;
                }
            }
        }
        if (arguments.source) {
            printllist(l);
        } else putc('\n', flist);
    }
}

void listing_mem(const uint8_t *data, size_t len, address_t myaddr) { 
    int print;
    int l;
    int lcol;
    char str[3*16+1], *s;
    if (nolisting || !flist || temporary_label_branch) return;

    print = 1;
    l = printaddr('>', myaddr);
    if (len) {
        size_t p = 0;
        lcol = arguments.source ? 8 : 16;
        s = str;
        l = padding(l, HEX_COLUMN);
        while (len) {
            if (!lcol--) {
                *s = 0;
                fputs(str + 1, flist);
                l += s - str - 1;
                s = str;
                if (arguments.source && print) {
                    printllist(l);
                    print = 0;
                } else putc('\n',flist);
                l = printaddr('>', myaddr);
                l = padding(l, HEX_COLUMN);
                lcol = 15;
            }
            *s++ = ' ';
            s = out_hex(s, data[p++]);
            myaddr = (myaddr + 1) & all_mem;
            len--;
        }
        *s = 0;
        fputs(str + 1, flist);
        l += s - str - 1;
    }

    if (arguments.source && print) {
        printllist(l);
    } else putc('\n',flist);
}

void listing_file(const char *txt, const char *name) {
    if (flist) {
        fputs(txt, flist);
        argv_print(name, flist);
        fputs("\n\n", flist);
    }
}
