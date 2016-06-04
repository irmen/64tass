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
#include <errno.h>
#include "file.h"
#include "error.h"
#include "64tass.h"
#include "opcodes.h"
#include "unicode.h"
#include "section.h"
#include "instruction.h"
#include "obj.h"
#include "mem.h"
#include "values.h"
#include "arguments.h"

#define LINE_WIDTH 8
#define ADDR_WIDTH 8
#define LADDR_WIDTH 8
#define HEX_WIDTH 16
#define MONITOR_WIDTH 16

static struct {
    int addr, laddr, hex, monitor, source;
} columns;

bool listing_pccolumn;
unsigned int nolisting;   /* listing */
const uint8_t *llist = NULL;
static FILE* flist = NULL;      /* listfile */
static const char *hex = "0123456789abcdef";
static uint16_t lastfile;

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
    if (databank != 0) s = out_hex(s, databank);
    s = out_hex(s, adr >> 8);
    return out_hex(s, adr);
}

static char *out_pb(char *s, unsigned int adr) {
    *s++ = '$';
    if (current_section->l_address.bank != 0) s = out_hex(s, current_section->l_address.bank >> 16);
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
    size_t l;

    columns.addr = arguments.linenum ? LINE_WIDTH : 0;
    columns.laddr = columns.addr + ADDR_WIDTH;
    columns.hex = columns.laddr + (listing_pccolumn ? LADDR_WIDTH : 0);
    columns.monitor = columns.hex + HEX_WIDTH;
    columns.source = columns.monitor + (arguments.monitor ? MONITOR_WIDTH : 0);

    flist = dash_name(filename) ? stdout : file_open(filename, "wt");
    if (flist == NULL) {
        err_msg_file(ERROR_CANT_WRTE_LST, filename, &nopoint);
        return;
    }
    fputs("\n; 64tass Turbo Assembler Macro V" VERSION " listing file\n;", flist);
    prgname = *argv;
    if (prgname != NULL) {
        const char *newp = strrchr(prgname, '/');
        if (newp != NULL) prgname = newp + 1;
#if defined _WIN32 || defined __WIN32__ || defined __EMX__ || defined __MSDOS__ || defined __DOS__
        newp = strrchr(prgname, '\\');
        if (newp != NULL) prgname = newp + 1;
#endif
    }
    for (i = 0; i < argc; i++) {
        putc(' ', flist);
        argv_print((i != 0) ? argv[i] : prgname, flist);
    }
    fputs("\n; ", flist);
    time(&t); fputs(ctime(&t), flist);
    lastfile = 0;
    putc('\n', flist);
    l = 0;
    if (arguments.linenum) {
        fputs(";Line", flist); l += 5;
        l = padding(l, columns.addr);
    }
    fputs(";Offset", flist); l += 7;
    if (listing_pccolumn) {
        l = padding(l, columns.laddr);
        fputs(";PC", flist); l += 3;
    }
    l = padding(l, columns.hex);
    fputs(";Hex", flist); l += 4;
    if (arguments.monitor) {
        l = padding(l, columns.monitor);
        fputs(";Monitor", flist); l += 8;
    }
    if (arguments.source) {
        padding(l, columns.source);
        fputs(";Source", flist);
    }
    putc('\n', flist);
}

void listing_close(const char *filename) {
    struct linepos_s nopoint = {0, 0};
    if (flist != NULL) {
        int err;
        fputs("\n;******  End of listing\n", flist);
        err = ferror(flist);
        err |= (flist != stdout) ? fclose(flist) : fflush(flist);
        if (err != 0 && errno) err_msg_file(ERROR_CANT_WRTE_LST, filename, &nopoint);
    }
    flist = NULL;
}

static void printllist(int l) {
    if (nolisting == 0 && flist != NULL && arguments.source && temporary_label_branch == 0) {
        if (llist != NULL) {
            const uint8_t *c = llist;
            while (*c == 0x20 || *c == 0x09) c++;
            if (*c != 0) {
                padding(l, columns.source);
                printable_print(llist, flist);
            }
            llist = NULL;
        }
        putc('\n', flist);
    }
}

static int printline(void) {
    int l;
    if (curfile < 2) return 0;
    l = fprintf(flist, "%" PRIuline, lpoint.line);
    if (l < 0) l = 0;
    if (lastfile != curfile) {
        int l2 = fprintf(flist, ":%u", curfile - 1);
        if (l2 >= 0) l += l2;
        lastfile = curfile;
    }
    return l;
}

void listing_equal(Obj *val) {
        int l;
    if (nolisting != 0 || flist == NULL || !arguments.source || temporary_label_branch != 0) return;
    if (arguments.linenum) {
        l = printline();
        l = padding(l, columns.addr);
    } else l = 0;
    putc('=', flist);
    l += val_print(val, flist) + 1;
    printllist(l);
}

static int printaddr(char pre, address_t addr) {
    char str[8], *s = str;
    if (pre != 0) *s++ = pre;
    if (addr > 0xffffff) s = out_hex(s, addr >> 24);
    if (addr > 0xffff) s = out_hex(s, addr >> 16);
    s = out_hex(s, addr >> 8);
    s = out_hex(s, addr);
    *s = 0;
    fputs(str, flist);
    return s - str;
}

void listing_line(linecpos_t pos) {
    size_t i;
    int l;
    if (nolisting != 0 || !arguments.source || temporary_label_branch != 0 || llist == NULL) return;
    if (flist == NULL) {
        if (!fixeddig || constcreated || listing_pccolumn) return;
        address_t addr = (current_section->l_address.address & 0xffff) | current_section->l_address.bank;
        i = 0;
        while (i < pos && (llist[i] == 0x20 || llist[i] == 0x09)) i++;
        if (i < pos && current_section->address != addr) listing_pccolumn = true;
        return;
    }
    i = 0;
    while (i < pos && (llist[i] == 0x20 || llist[i] == 0x09)) i++;
    if (i < pos) {
        address_t addr = (current_section->l_address.address & 0xffff) | current_section->l_address.bank;
        if (arguments.linenum) {
            l = printline();
            l = padding(l, columns.addr);
        } else l = 0;
        l += printaddr('.', current_section->address);
        if (current_section->address != addr && listing_pccolumn) {
            l = padding(l, columns.laddr);
            l += printaddr('\0', addr);
        }
    } else l = 0;
    if (arguments.verbose) {
        if (llist[i] != 0) {
            if (l == 0 && arguments.linenum) l = printline();
            padding(l, columns.source);
            printable_print(llist, flist);
        }
        putc('\n', flist);
    } else {
        if (l != 0) {
            while (llist[pos-1] == 0x20 || llist[pos-1] == 0x09) pos--;
            padding(l, columns.source);
            printable_print2(llist, flist, pos);
            putc('\n', flist);
        }
    }
    llist = NULL;
}

void listing_line_cut(linecpos_t pos) {
    size_t i;
    if (nolisting != 0 || !arguments.source || temporary_label_branch != 0 || llist == NULL) return;
    if (flist == NULL) {
        if (!fixeddig || constcreated || listing_pccolumn) return;
        address_t addr = (current_section->l_address.address & 0xffff) | current_section->l_address.bank;
        i = 0;
        while (i < pos && (llist[i] == 0x20 || llist[i] == 0x09)) i++;
        if (i < pos && current_section->address != addr) listing_pccolumn = true;
        return;
    }
    i = 0;
    while (i < pos && (llist[i] == 0x20 || llist[i] == 0x09)) i++;
    if (i < pos) {
        address_t addr = (current_section->l_address.address & 0xffff) | current_section->l_address.bank;
        int l;
        if (arguments.linenum) {
            l = printline();
            l = padding(l, columns.addr);
        } else l = 0;
        l += printaddr('.', current_section->address);
        if (current_section->address != addr && listing_pccolumn) {
            l = padding(l, columns.laddr);
            l += printaddr('\0', addr);
        }
        while (llist[pos-1] == 0x20 || llist[pos-1] == 0x09) pos--;
        padding(l, columns.source);
        printable_print2(llist, flist, pos);
        putc('\n', flist);
    }
    llist = NULL;
}

void listing_line_cut2(linecpos_t pos) {
    if (arguments.verbose) {
        if (nolisting == 0 && flist != NULL && arguments.source && temporary_label_branch == 0) {
            if (llist != NULL) {
                int l = arguments.linenum ? printline() : 0;
                padding(l, columns.source);
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
    int i, l;
    address_t addr, addr2;
    if (nolisting != 0 || temporary_label_branch != 0) return;
    if (flist == NULL) {
        if (!fixeddig || constcreated || listing_pccolumn) return;
        addr = ((current_section->l_address.address - ln - 1) & 0xffff) | current_section->l_address.bank;
        addr2 = (current_section->address - ln - 1) & all_mem2;
        if (addr2 != addr) listing_pccolumn = true;
        return;
    }
    addr = ((current_section->l_address.address - ln - 1) & 0xffff) | current_section->l_address.bank;
    addr2 = (current_section->address - ln - 1) & all_mem2;
    if (arguments.linenum) {
        l = (llist != NULL) ? printline() : 0;
        l = padding(l, columns.addr);
    } else l = 0;
    l += printaddr('.', addr2);
    if (addr2 != addr && listing_pccolumn) {
        l = padding(l, columns.laddr);
        l += printaddr('\0', addr);
    }
    if (current_section->dooutput) {
        char str[32], *s;
        if (ln >= 0) {
            uint32_t temp = adr;
            l = padding(l, columns.hex);
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
                l = padding(l, columns.monitor);
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
                case ADR_REL: if (ln > 0) s = out_pb(s, ((int8_t)adr) + current_section->l_address.address); else s--; break;
                case ADR_BIT_ZP_REL: 
                    s = out_bit(s, cod, adr);
                    *s++ = ',';
                    s = out_pb(s, ((int8_t)(adr >> 8)) + current_section->l_address.address);
                    break;
                case ADR_REL_L: s = out_pb(s, ((int16_t)(adr + (((cod & 0x0F) == 3) ? -1 : 0))) + current_section->l_address.address); break;
                case ADR_MOVE: s = out_byte(s, adr >> 8); *s++ = ','; s = out_byte(s, adr);
                case ADR_LEN: break;/* not an addressing mode */
                }
                while (*post != 0) *s++ = *post++;
                *s = 0;
                fputs(str, flist);
                l += s - str;
            }
        }
    }
    if (arguments.source) {
        printllist(l);
        return;
    }
    putc('\n', flist);
}

void listing_mem(const uint8_t *data, size_t len, address_t myaddr, address_t myaddr2) { 
    bool print;
    int l;
    int lcol;
    char str[3 * 16 + 1], *s;
    if (nolisting != 0 || temporary_label_branch != 0) return;
    if (flist == NULL) {
         if (myaddr != myaddr2) listing_pccolumn = true;
         return;
    }
    print = true;
    if (arguments.linenum) {
        l = printline();
        l = padding(l, columns.addr);
    } else l = 0;
    l += printaddr('>', myaddr);
    if (myaddr != myaddr2 && listing_pccolumn) {
        l = padding(l, columns.laddr);
        l += printaddr('\0', myaddr2);
    }
    if (len != 0) {
        size_t p = 0;
        lcol = arguments.source ? (arguments.monitor ? 8 : 4) : 16;
        s = str;
        l = padding(l, columns.hex);
        while (len != 0) {
            if ((lcol--) == 0) {
                *s = 0;
                fputs(str + 1, flist);
                l += s - str - 1;
                s = str;
                if (arguments.source && print) {
                    printllist(l);
                    print = false;
                } else putc('\n',flist);
                l = arguments.linenum ? padding(0, columns.addr) : 0;
                l += printaddr('>', myaddr);
                if (myaddr != myaddr2 && listing_pccolumn) {
                    l = padding(l, columns.laddr);
                    l += printaddr('\0', myaddr2);
                }
                l = padding(l, columns.hex);
                lcol = 15;
            }
            *s++ = ' ';
            s = out_hex(s, data[p++]);
            myaddr = (myaddr + 1) & all_mem2;
            myaddr2 = ((myaddr2 + 1) & 0xffff) | (myaddr2 & ~0xffff);
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
    if (flist != NULL) {
        putc('\n', flist);
        if (arguments.linenum) {
            int l = (name != NULL) ? fprintf(flist, ":%u", curfile - 1) : 0;
            padding((l >= 0) ? l : 0, columns.addr);
            lastfile = curfile;
        };
        fputs(txt, flist);
        if (name != NULL) argv_print(name, flist);
        fputs("\n\n", flist);
    }
}
