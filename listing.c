/*
    $Id: listing.c 2432 2021-02-28 13:18:37Z soci $

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
#include "values.h"
#include "arguments.h"

#define LINE_WIDTH 8
#define ADDR_WIDTH 8
#define LADDR_WIDTH 8
#define HEX_WIDTH 16
#define MONITOR_WIDTH 16

bool listing_pccolumn;
unsigned int nolisting;   /* listing */
const uint8_t *llist = NULL;

typedef struct Listing {
    size_t c;
    unsigned int i;
    char s[LINE_WIDTH + ADDR_WIDTH + LADDR_WIDTH + 3*16];
    char hex[16];
    const uint16_t *disasm;
    const uint32_t *mnemonic;
    struct {
        size_t addr, laddr, hex, monitor, source;
    } columns;
    FILE *flist;
    uint16_t lastfile;
    const char *filename;
    unsigned int tab_size;
    bool linenum, verbose, monitor, pccolumn, source;
} Listing;

static void flushbuf(Listing *ls) {
    ls->c += fwrite(ls->s, 1, ls->i, ls->flist);
    ls->i = 0;
}

static void newline(Listing *ls) {
    putc('\n', ls->flist);
    ls->c = 0;
}

static void padding2(Listing *ls, size_t t) {
    if (ls->c + ls->i >= t) {ls->s[ls->i++] = '\n'; flushbuf(ls); ls->c = 0;}
    ls->c += ls->i;
    if (ls->tab_size > 1) {
        ls->c -= ls->c % ls->tab_size;
        while (ls->c + ls->tab_size <= t) { ls->c += ls->tab_size; ls->s[ls->i++] = '\t'; }
    }
    while (ls->c < t) { ls->c++; ls->s[ls->i++] = ' '; }
    ls->c -= ls->i;
}

static inline void out_hex(Listing *ls, unsigned int c) {
    ls->s[ls->i++] = ls->hex[(c >> 4) & 15];
    ls->s[ls->i++] = ls->hex[c & 15];
}

static void out_byte(Listing *ls, unsigned int adr) {
    ls->s[ls->i++] = '$';
    out_hex(ls, adr);
}

static void out_word(Listing *ls, unsigned int adr) {
    out_byte(ls, adr >> 8);
    out_hex(ls, adr);
}

static void out_long(Listing *ls, unsigned int adr) {
    out_word(ls, adr >> 8);
    out_hex(ls, adr);
}

static void out_zp(Listing *ls, unsigned int adr) {
    adr = (uint16_t)(((uint8_t)adr) + dpage);
    if (adr > 0xff) out_word(ls, adr);
    else out_byte(ls, adr);
}

static void out_db(Listing *ls, unsigned int adr) {
    ls->s[ls->i++] = '$';
    if (databank != 0) out_hex(ls, databank);
    out_hex(ls, adr >> 8);
    out_hex(ls, adr);
}

static void out_pb(Listing *ls, unsigned int adr) {
    ls->s[ls->i++] = '$';
    if (current_address->l_address > 0xffff) out_hex(ls, current_address->l_address >> 16);
    out_hex(ls, adr >> 8);
    out_hex(ls, adr);
}

static void out_bit(Listing *ls, unsigned int cod, unsigned int c) {
    ls->s[ls->i++] = (char)('0' + ((cod >> 4) & 7));
    ls->s[ls->i++] = ',';
    out_zp(ls, c);
}

static void out_txt(Listing *ls, const char *s) {
    while (*s != '\0') {
        ls->s[ls->i++] = *s++;
    }
}

MUST_CHECK Listing *listing_open(const char *filename, int argc, char *argv[]) {
    Listing *ls;
    struct linepos_s nopoint = {0, 0};
    time_t t;
    int i;
    FILE *flist;

    flist = dash_name(filename) ? stdout : file_open(filename, "wt");
    if (flist == NULL) {
        err_msg_file(ERROR_CANT_WRTE_LST, filename, &nopoint);
        return NULL;
    }
    clearerr(flist); errno = 0;

    ls = (Listing *)mallocx(sizeof *ls);

    memcpy(ls->hex, "0123456789abcdef", 16);
    ls->filename = filename;
    ls->flist = flist;
    ls->linenum = arguments.list.linenum;
    ls->pccolumn = listing_pccolumn;
    ls->columns.addr = arguments.list.linenum ? LINE_WIDTH : 0;
    ls->columns.laddr = ls->columns.addr + ADDR_WIDTH;
    ls->columns.hex = ls->columns.laddr + (ls->pccolumn ? LADDR_WIDTH : 0);
    ls->columns.monitor = ls->columns.hex + HEX_WIDTH;
    ls->columns.source = ls->columns.monitor + (arguments.list.monitor ? MONITOR_WIDTH : 0);
    ls->tab_size = arguments.tab_size;
    ls->verbose = arguments.list.verbose;
    ls->monitor = arguments.list.monitor;
    ls->source = arguments.list.source;
    ls->lastfile = 0;
    ls->i = 0;

    fputs("\n; 64tass Turbo Assembler Macro V" VERSION " listing file\n;", flist);
    for (i = 0; i < argc; i++) {
        putc(' ', flist);
        argv_print(argv[i], flist);
    }
    fputs("\n; ", flist);
    time(&t); fputs(ctime(&t), flist);
    newline(ls);
    if (ls->linenum) {
        out_txt(ls, ";Line");
        padding2(ls, ls->columns.addr);
    }
    out_txt(ls, ";Offset");
    if (ls->pccolumn) {
        padding2(ls, ls->columns.laddr);
        out_txt(ls, ";PC");
    }
    padding2(ls, ls->columns.hex);
    out_txt(ls, ";Hex");
    if (ls->monitor) {
        padding2(ls, ls->columns.monitor);
        out_txt(ls, ";Monitor");
    }
    if (ls->source) {
        padding2(ls, ls->columns.source);
        out_txt(ls, ";Source");
    }
    flushbuf(ls);
    newline(ls);
    return ls;
}

void listing_close(Listing *ls) {
    struct linepos_s nopoint = {0, 0};
    int err;
    if (ls == NULL) return;

    fputs("\n;******  End of listing\n", ls->flist);
    err = ferror(ls->flist);
    err |= (ls->flist != stdout) ? fclose(ls->flist) : fflush(ls->flist);
    if (err != 0 && errno != 0) err_msg_file(ERROR_CANT_WRTE_LST, ls->filename, &nopoint);
    free(ls);
}

static bool printllist(Listing *ls) {
    const uint8_t *c;
    if (llist == NULL) return true;
    c = llist;
    while (*c == 0x20 || *c == 0x09) c++;
    if (*c != 0) {
        padding2(ls, ls->columns.source);
        flushbuf(ls);
        printable_print(llist, ls->flist);
    }
    llist = NULL;
    return *c == 0;
}

static void printdec(Listing *ls, uint32_t dec) {
    static const uint32_t d[9] = {1000000000, 100000000, 10000000, 1000000, 100000, 10000, 1000, 100, 10};
    unsigned int i;
    for (i = 9; i > 0; i--) {
        if (dec < d[i - 1]) break;
    }
    for (; i < 9; i++) {
        uint32_t a = dec / d[i];
        dec = dec % d[i];
        ls->s[ls->i++] = (char)('0' + a);
    }
    ls->s[ls->i++] = (char)('0' + dec);
}

static void printfile(Listing *ls) {
    ls->s[ls->i++] = ':';
    printdec(ls, ls->lastfile - 1U);
}

static void printline(Listing *ls) {
    uint16_t curfile = current_file_list->file->uid;
    if (curfile < 2) return;
    printdec(ls, lpoint.line);
    if (ls->lastfile == curfile) return;
    ls->lastfile = curfile;
    printfile(ls);
}

FAST_CALL void listing_equal(Listing *ls, Obj *val) {
    if (ls == NULL) return;
    if (nolisting != 0 || !ls->source || temporary_label_branch != 0) return;
    if (ls->linenum) {
        printline(ls);
        padding2(ls, ls->columns.addr);
        flushbuf(ls);
    }
    putc('=', ls->flist);
    ls->c += val_print(val, ls->flist, ls->verbose ? SIZE_MAX : ls->columns.source - 2) + 1;
    printllist(ls);
    newline(ls);
}

static void printaddr(Listing *ls, char pre, address_t addr, address_t addr2) {
    ls->s[ls->i++] = pre;
    for (;;) {
        if (addr > 0xffff) {
            if (addr >> 24 != 0) out_hex(ls, addr >> 24);
            out_hex(ls, addr >> 16);
        }
        out_hex(ls, addr >> 8);
        out_hex(ls, addr);
        if (addr2 == addr || !ls->pccolumn) return;
        addr = addr2;
        padding2(ls, ls->columns.laddr);
    }
}

static void printhex(Listing *ls, unsigned int cod, uint32_t adr, int ln) {
    padding2(ls, ls->columns.hex);
    out_hex(ls, cod);
    while (ln > 0) {
        ls->s[ls->i++] = ' ';
        out_hex(ls, adr);
        adr >>= 8;
        ln--;
    }
}

static void printhex2(Listing *ls, unsigned int ln, const uint8_t *data) {
    unsigned int i = 0;
    padding2(ls, ls->columns.hex);
    for (;;) {
        out_hex(ls, data[i++]);
        if (i >= ln) break;
        ls->s[ls->i++] = ' ';
    }
}

static void printmon(Listing *ls, unsigned int cod, int ln, uint32_t adr) {
    const char *mode;
    Adr_types type;
    uint32_t mnem;

    padding2(ls, ls->columns.monitor);
    mnem = ls->mnemonic[ls->disasm[cod] & 0xff];
    ls->s[ls->i++] = (char)(mnem >> 16);
    ls->s[ls->i++] = (char)(mnem >> 8);
    ls->s[ls->i++] = (char)(mnem);
    ls->s[ls->i++] = ' ';

    type = (Adr_types)(ls->disasm[cod] >> 8);
    mode = addr_modes[type];
    if (*mode != ' ') ls->s[ls->i++] = *mode;
    mode++;
    switch (type) {
    case ADR_IMPLIED: ls->i--; return;
    case ADR_REG: return;
    case ADR_IMMEDIATE:
        switch (ln) {
        default: ls->i -= 2; return;
        case 0: return;
        case 1: out_byte(ls, adr); return;
        case 2: out_word(ls, adr); return;
        }
    case ADR_ADDR: if (cod == 0x20 || cod == 0x4c) out_pb(ls, adr); else out_db(ls, adr); return;
    case ADR_BIT_ZP: out_bit(ls, cod, adr); return;
    case ADR_LONG:
    case ADR_LONG_X: out_long(ls, adr); break;
    case ADR_ADDR_X_I: out_pb(ls, adr); break;
    case ADR_ZP_R:
    case ADR_ZP_R_I_Y:
    case ADR_ZP_S:
    case ADR_ZP_S_I_Y: out_byte(ls, adr); break;
    case ADR_ADDR_X:
    case ADR_ADDR_Y: out_db(ls, adr); break;
    case ADR_ZP:
    case ADR_ZP_I:
    case ADR_ZP_I_Y:
    case ADR_ZP_I_Z:
    case ADR_ZP_LI:
    case ADR_ZP_LI_Y:
    case ADR_ZP_X:
    case ADR_ZP_X_I:
    case ADR_ZP_Y: out_zp(ls, adr); break;
    case ADR_ADDR_I:
    case ADR_ADDR_LI: out_word(ls, adr); break;
    case ADR_REL: if (ln > 0) out_pb(ls, (address_t)((int8_t)adr + (int)current_address->l_address)); else ls->i--; return;
    case ADR_BIT_ZP_REL:
        out_bit(ls, cod, adr);
        ls->s[ls->i++] = ',';
        out_pb(ls, (address_t)((int8_t)(adr >> 8) + (int)current_address->l_address));
        return;
    case ADR_REL_L: if (ln > 0) out_pb(ls, adr + (((cod & 0x0F) == 3) ? -1U : 0) + current_address->l_address); else ls->i--; return;
    case ADR_MOVE: out_byte(ls, adr >> 8); ls->s[ls->i++] = ','; ls->s[ls->i++] = '#'; out_byte(ls, adr); return;
    }
    while (*mode != 0) ls->s[ls->i++] = *mode++;
}

static void printsource(Listing *ls, linecpos_t pos) {
    while (pos > 0 && (llist[pos-1] == 0x20 || llist[pos-1] == 0x09)) pos--;
    padding2(ls, ls->columns.source);
    flushbuf(ls);
    printable_print2(llist, ls->flist, pos);
    newline(ls);
}

FAST_CALL void listing_equal2(Listing *ls, Obj *val, linecpos_t pos) {
    if (ls == NULL) return;
    if (nolisting != 0 || !ls->source || temporary_label_branch != 0) return;
    if (ls->linenum) {
        printline(ls);
        padding2(ls, ls->columns.addr);
    }
    putc('=', ls->flist);
    ls->c += val_print(val, ls->flist, ls->verbose ? SIZE_MAX : ls->columns.source - 2) + 1;
    if (ls->verbose) {
        printllist(ls);
        newline(ls);
    } else {
        printsource(ls, pos);
    }
    llist = NULL;
}


FAST_CALL void listing_line(Listing *ls, linecpos_t pos) {
    size_t i;
    if (nolisting != 0  || temporary_label_branch != 0 || llist == NULL) return;
    if (ls == NULL) {
        address_t addr;
        if (!fixeddig || constcreated || listing_pccolumn || !arguments.list.source) return;
        addr = current_address->l_address;
        i = 0;
        while (i < pos && (llist[i] == 0x20 || llist[i] == 0x09)) i++;
        if (i < pos && current_address->address != addr) listing_pccolumn = true;
        return;
    }
    if (!ls->source) return;
    i = 0;
    while (i < pos && (llist[i] == 0x20 || llist[i] == 0x09)) i++;
    if (i < pos) {
        if (ls->linenum) {
            printline(ls);
            padding2(ls, ls->columns.addr);
        }
        printaddr(ls, '.', current_address->address, current_address->l_address);
    }
    if (ls->verbose) {
        if (ls->i != 0) flushbuf(ls);
        if (llist[i] != 0) {
            if (ls->c == 0 && ls->linenum) printline(ls);
            padding2(ls, ls->columns.source);
            flushbuf(ls);
            printable_print(llist, ls->flist);
        }
        newline(ls);
    } else {
        if (ls->c + ls->i != 0) printsource(ls, pos);
        else if (ls->i != 0) flushbuf(ls);
    }
    llist = NULL;
}

FAST_CALL void listing_line_cut(Listing *ls, linecpos_t pos) {
    size_t i;
    if (nolisting != 0 || temporary_label_branch != 0 || llist == NULL) return;
    if (ls == NULL) {
        if (!fixeddig || constcreated || listing_pccolumn || !arguments.list.source) return;
        i = 0;
        while (i < pos && (llist[i] == 0x20 || llist[i] == 0x09)) i++;
        if (i < pos && current_address->address != current_address->l_address) listing_pccolumn = true;
        return;
    }
    if (!ls->source) return;
    i = 0;
    while (i < pos && (llist[i] == 0x20 || llist[i] == 0x09)) i++;
    if (i < pos) {
        if (ls->linenum) {
            printline(ls);
            padding2(ls, ls->columns.addr);
        }
        printaddr(ls, '.', current_address->address, current_address->l_address);
        printsource(ls, pos);
    }
    llist = NULL;
}

FAST_CALL void listing_line_cut2(Listing *ls, linecpos_t pos) {
    if (ls == NULL || !ls->verbose || llist == NULL) return;
    if (nolisting == 0 && ls->source && temporary_label_branch == 0) {
        if (ls->linenum) printline(ls);
        padding2(ls, ls->columns.source);
        flushbuf(ls);
        caret_print(llist, ls->flist, pos);
        printable_print(llist + pos, ls->flist);
        newline(ls);
        llist = NULL;
    }
}

FAST_CALL void listing_set_cpumode(Listing *ls, const struct cpu_s *cpumode) {
    if (ls == NULL) return;
    ls->disasm = cpumode->disasm;
    ls->mnemonic = cpumode->mnemonic;
}

void listing_instr(Listing *ls, unsigned int cod, uint32_t adr, int ln) {
    address_t addr, addr2;
    if (nolisting != 0 || temporary_label_branch != 0) return;
    if (ls == NULL) {
        if (!fixeddig || constcreated || listing_pccolumn) return;
        ln++;
        addr = (current_address->l_address - (unsigned int)ln) & all_mem;
        addr2 = (current_address->address - (unsigned int)ln) & all_mem2;
        if (addr2 != addr) listing_pccolumn = true;
        return;
    }
    if (ln < 0 && !ls->source) return;
    if (ls->linenum) {
        if (llist != NULL) printline(ls);
        padding2(ls, ls->columns.addr);
    }
    addr = (current_address->l_address - (unsigned int)(ln + 1)) & all_mem;
    addr2 = (current_address->address - (unsigned int)(ln + 1)) & all_mem2;
    printaddr(ls, '.', addr2, addr);
    if (ln >= 0) {
        printhex(ls, cod ^ outputeor, adr ^ outputeor, ln);
        if (ls->monitor) {
            printmon(ls, cod, ln, adr);
        }
    }
    if (!ls->source || printllist(ls)) flushbuf(ls);
    newline(ls);
}

void listing_mem(Listing *ls, const uint8_t *data, size_t len, address_t myaddr, address_t myaddr2) {
    bool print, exitnow;
    int lcol;
    unsigned int repeat;
    struct {
        uint8_t data[16];
        unsigned int len;
        address_t addr, addr2;
    } prev, current;
    size_t p;

    if (nolisting != 0 || temporary_label_branch != 0) return;
    if (ls == NULL) {
         if (myaddr != myaddr2) listing_pccolumn = true;
         return;
    }
    print = true; exitnow = false;
    prev.addr = current.addr = myaddr;
    prev.addr2 = current.addr2 = myaddr2;
    repeat = 0; p = 0; prev.len = current.len = 0;
    if (len != 0) {
        lcol = ls->source ? (ls->monitor ? 8 : 4) : 16;
        while (len != 0) {
            if ((lcol--) == 0) {
                if (print || prev.len != current.len || memcmp(prev.data, current.data, current.len) != 0 || ls->verbose) {
                flush:
                    if (repeat != 0) {
                        if (ls->linenum) padding2(ls, ls->columns.addr);
                        if (repeat == 1) {
                            printaddr(ls, '>', prev.addr, prev.addr2);
                            printhex2(ls, prev.len, prev.data);
                            flushbuf(ls);
                        } else {
                            ls->s[ls->i++] = ';';
                            padding2(ls, ls->columns.hex);
                            flushbuf(ls);
                            fprintf(ls->flist, "...repeated %u times (%u bytes)...", repeat, repeat * 16);
                        }
                        newline(ls);
                        repeat = 0;
                    }
                    if (current.len != 0 || ls->source) {
                        if (ls->linenum) {
                            if (print) printline(ls);
                            padding2(ls, ls->columns.addr);
                        }
                        printaddr(ls, '>', current.addr, current.addr2);
                        if (current.len != 0) {
                            printhex2(ls, current.len, current.data);
                        }
                        if (!ls->source || !print || printllist(ls)) flushbuf(ls);
                        newline(ls);
                    }
                    if (exitnow) return;
                    memcpy(&prev, &current, sizeof prev);
                    print = false;
                } else {
                    repeat++;
                    prev.addr = current.addr;
                    prev.addr2 = current.addr2;
                }
                current.len = 0;
                current.addr = myaddr;
                current.addr2 = myaddr2;
                lcol = 15;
            }
            current.data[current.len++] = data[p++];
            myaddr = (myaddr + 1) & all_mem2;
            myaddr2 = (myaddr2 + 1) & all_mem;
            len--;
        }
    }
    exitnow = true;
    goto flush;
}

void listing_file(Listing *ls, const char *txt, const struct file_s *file) {
    if (ls == NULL) return;
    newline(ls);
    if (ls->linenum) {
        if (file != NULL) {
            uint16_t curfile = file->uid;
            if (ls->lastfile != curfile) {
                ls->lastfile = file->uid;
                printfile(ls);
            }
        }
        padding2(ls, ls->columns.addr);
        flushbuf(ls);
    };
    fputs(txt, ls->flist);
    if (file != NULL) argv_print(file->realname, ls->flist);
    newline(ls);
    newline(ls);
}
