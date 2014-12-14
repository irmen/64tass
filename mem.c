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
#include "mem.h"
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include "error.h"
#include "file.h"
#include "misc.h"
#include "64tass.h"
#include "listing.h"

struct memblock_s { /* starts and sizes */
    size_t p, len;
    address_t addr;
    struct memblocks_s *ref;
};

static int memblockcomp(const void *a, const void *b) {
    const struct memblock_s *aa=(const struct memblock_s *)a;
    const struct memblock_s *bb=(const struct memblock_s *)b;
    return aa->addr - bb->addr;
}

static void memcomp(struct memblocks_s *memblocks) {
    unsigned int i, j, k;
    if (memblocks->compressed) return;
    memblocks->compressed = 1;
    memjmp(memblocks, 0);

    for (j = 0; j < memblocks->p; j++) { /* replace references with real copies */
        struct memblocks_s *b = memblocks->data[j].ref;
        if (b) {
            size_t rest;
            memcomp(b);
            rest = memblocks->p - j - 1;
            memblocks->p += b->p - 1;
            if (memblocks->p >= memblocks->len) {
                memblocks->len = memblocks->p + 64;
                memblocks->data = (struct memblock_s *)realloc(memblocks->data, memblocks->len*sizeof(*memblocks->data));
                if (!memblocks->data || memblocks->len < 64 || memblocks->len > SIZE_MAX / sizeof(*memblocks->data)) err_msg_out_of_memory(); /* overflow */
            }
            memmove(&memblocks->data[j + b->p], &memblocks->data[j + 1], rest * sizeof(*memblocks->data));
            for (k = 0; k < b->p; k++) {
                struct memblock_s *b2 = &memblocks->data[j + k];
                b2->p = memblocks->mem.p;
                b2->len = b->data[k].len;
                b2->ref = NULL;
                b2->addr = b->data[k].addr;
                memblocks->mem.p += b2->len;
                if (memblocks->mem.p >= memblocks->mem.len) {
                    memblocks->mem.len = memblocks->mem.p + 0x1000;
                    memblocks->mem.data = (uint8_t *)realloc(memblocks->mem.data, memblocks->mem.len);
                    if (!memblocks->mem.data || memblocks->mem.len < 0x1000) err_msg_out_of_memory(); /* overflow */
                }
                memcpy(&memblocks->mem.data[b2->p], &b->mem.data[b->data[k].p], b2->len);
            }
            j--;
        }
    }
    if (memblocks->p < 2) return;
    if (arguments.output_mode == OUTPUT_XEX) return;

    for (k = j = 0; j < memblocks->p; j++) {
        struct memblock_s *bj = &memblocks->data[j];
        if (bj->len) {
            for (i = j + 1; i < memblocks->p; i++) if (memblocks->data[i].len) {
                struct memblock_s *bi = &memblocks->data[i];
                if (bj->addr <= bi->addr && (bj->addr + bj->len) > bi->addr) {
                    size_t overlap = (bj->addr + bj->len) - bi->addr;
                    if (overlap > bi->len) overlap = bi->len;
                    memcpy(memblocks->mem.data + bj->p + (unsigned)(bi->addr - bj->addr), memblocks->mem.data + bi->p, overlap);
                    bi->len -= overlap;
                    bi->p += overlap;
                    bi->addr += overlap;
                    continue;
                }
                if (bi->addr <= bj->addr && (bi->addr + bi->len) > bj->addr) {
                    size_t overlap = bi->addr + bi->len - bj->addr;
                    if (overlap > bj->len) overlap = bj->len;
                    bj->addr += overlap;
                    bj->p += overlap;
                    bj->len -= overlap;
                    if (!bj->len) break;
                }
            }
            if (bj->len) {
                if (j != k) memblocks->data[k] = *bj;
                k++;
            }
        }
    }
    memblocks->p = k;
    qsort(memblocks->data, memblocks->p, sizeof(*memblocks->data), memblockcomp);
}

void memjmp(struct memblocks_s *memblocks, address_t adr) {
    if (memblocks->mem.p == memblocks->lastp) {
        memblocks->lastaddr = adr;
        return;
    }
    if (memblocks->p>=memblocks->len) {
        memblocks->len += 64;
        memblocks->data = (struct memblock_s *)realloc(memblocks->data, memblocks->len*sizeof(*memblocks->data));
        if (!memblocks->data || memblocks->len < 64 || memblocks->len > SIZE_MAX / sizeof(*memblocks->data)) err_msg_out_of_memory(); /* overflow */
    }
    memblocks->data[memblocks->p].len = memblocks->mem.p - memblocks->lastp;
    memblocks->data[memblocks->p].p = memblocks->lastp;
    memblocks->data[memblocks->p].ref = NULL;
    memblocks->data[memblocks->p++].addr = memblocks->lastaddr;
    memblocks->lastp = memblocks->mem.p;
    memblocks->lastaddr = adr;
}

void memref(struct memblocks_s *memblocks, struct memblocks_s *ref) {
    if (memblocks->p >= memblocks->len) {
        memblocks->len += 64;
        memblocks->data = (struct memblock_s *)realloc(memblocks->data, memblocks->len*sizeof(*memblocks->data));
        if (!memblocks->data || memblocks->len < 64 || memblocks->len > SIZE_MAX / sizeof(*memblocks->data)) err_msg_out_of_memory(); /* overflow */
    }
    memblocks->data[memblocks->p].len = 0;
    memblocks->data[memblocks->p].p = memblocks->lastp;
    memblocks->data[memblocks->p].ref = ref;
    memblocks->data[memblocks->p++].addr = memblocks->lastaddr;
}

void memprint(struct memblocks_s *memblocks) {
    char temp[10];
    unsigned int i;
    address_t start, end;

    memcomp(memblocks);

    if (memblocks->p) {
        i = 0;
        start = memblocks->data[i].addr;
        end = memblocks->data[i].addr + memblocks->data[i].len;
        for (i++; i < memblocks->p; i++) {
            if (memblocks->data[i].addr != end) {
                sprintf(temp, "$%04" PRIaddress, start);
                printf("Memory range:    %7s-$%04" PRIaddress "\n", temp, end-1);
                start = memblocks->data[i].addr;
            }
            end = memblocks->data[i].addr + memblocks->data[i].len;
        }
        sprintf(temp, "$%04" PRIaddress, start);
        printf("Memory range:    %7s-$%04" PRIaddress "\n", temp, end-1);
    }
}

static void putlw(int w, FILE *f) {
    putc(w, f);
    putc(w >> 8, f);
}

static void output_mem_c64(FILE *fout, const struct memblocks_s *memblocks) {
    address_t pos, end;
    size_t size;
    unsigned int i;

    if (memblocks->p) {
        pos = memblocks->data[0].addr;
        if (arguments.output_mode == OUTPUT_CBM || arguments.output_mode == OUTPUT_APPLE) {
            putlw(pos, fout);
            if (arguments.longaddr) putc(pos >> 16,fout);
        }
        if (arguments.output_mode == OUTPUT_APPLE) {
            end = memblocks->data[memblocks->p - 1].addr + memblocks->data[memblocks->p - 1].len;
            end -= pos;
            putlw(end, fout);
            if (arguments.longaddr) putc(end >> 16,fout);
        }
        for (i = 0; i < memblocks->p; i++) {
            size = memblocks->data[i].addr - pos;
            while (size--) putc(0, fout);
            fwrite(memblocks->mem.data + memblocks->data[i].p, memblocks->data[i].len, 1, fout);
            pos = memblocks->data[i].addr + memblocks->data[i].len;
        }
    }
}

static void output_mem_nonlinear(FILE *fout, const struct memblocks_s *memblocks) {
    address_t start;
    size_t size;
    unsigned int i, last;

    if (memblocks->p) {
        i = 0;
        start = memblocks->data[i].addr;
        size = memblocks->data[i].len;
        last = i;
        for (i++; i < memblocks->p; i++) {
            if (memblocks->data[i].addr != start + size) {
                putlw(size,fout);
                if (arguments.longaddr) putc(size >> 16,fout);
                putlw(start,fout);
                if (arguments.longaddr) putc(start >> 16,fout);
                while (last < i) {
                    fwrite(memblocks->mem.data + memblocks->data[last].p, memblocks->data[last].len, 1, fout);
                    last++;
                }
                start = memblocks->data[i].addr;
                size = 0;
            }
            size += memblocks->data[i].len;
        }
        putlw(size,fout);
        if (arguments.longaddr) putc(size >> 16, fout);
        putlw(start, fout);
        if (arguments.longaddr) putc(start >> 16,fout);
        while (last<i) {
            fwrite(memblocks->mem.data + memblocks->data[last].p, memblocks->data[last].len, 1, fout);
            last++;
        }
    }
    putlw(0, fout);
    if (arguments.longaddr) putc(0, fout);
}

static void output_mem_flat(FILE *fout, const struct memblocks_s *memblocks) {
    address_t pos;
    size_t size;
    unsigned int i;

    for (pos = i = 0; i < memblocks->p; i++) {
        size = memblocks->data[i].addr - pos;
        while (size--) putc(0, fout);
        fwrite(memblocks->mem.data + memblocks->data[i].p, memblocks->data[i].len, 1, fout);
        pos = memblocks->data[i].addr + memblocks->data[i].len;
    }
}

static void output_mem_atari_xex(FILE *fout, const struct memblocks_s *memblocks) {
    address_t start;
    size_t size;
    unsigned int i, last;

    if (memblocks->p) {
        i = 0;
        start = memblocks->data[i].addr;
        size = memblocks->data[i].len;
        last = i;
        for (i++; i < memblocks->p; i++) {
            if (memblocks->data[i].addr != start + size) {
                if (last == 0 || start == 0xffff) putlw(0xffff, fout);
                putlw(start, fout);
                putlw(start + size - 1, fout);
                while (last < i) {
                    fwrite(memblocks->mem.data + memblocks->data[last].p, memblocks->data[last].len, 1, fout);
                    last++;
                }
                start = memblocks->data[i].addr;
                size = 0;
            }
            size += memblocks->data[i].len;
        }
        if (last == 0 || start == 0xffff) putlw(0xffff, fout);
        putlw(start, fout);
        putlw(start + size - 1, fout);
        while (last<i) {
            fwrite(memblocks->mem.data + memblocks->data[last].p, memblocks->data[last].len, 1, fout);
            last++;
        }
    }
}

void output_mem(struct memblocks_s *memblocks) {
    FILE* fout;
    struct linepos_s nopoint = {0, 0};

    memcomp(memblocks);

    if (memblocks->mem.p) {
        if (arguments.output[0] == '-' && !arguments.output[1]) {
#ifdef _WIN32
            setmode(fileno(stdout), O_BINARY);
#endif
            fout = stdout;
        } else {
            if ((fout=file_open(arguments.output,"wb"))==NULL) err_msg_file(ERROR_CANT_WRTE_OBJ, arguments.output, &nopoint);
        }
        clearerr(fout);
        switch (arguments.output_mode) {
        case OUTPUT_FLAT: output_mem_flat(fout, memblocks); break;
        case OUTPUT_NONLINEAR: output_mem_nonlinear(fout, memblocks); break;
        case OUTPUT_XEX: output_mem_atari_xex(fout, memblocks); break;
        case OUTPUT_RAW: 
        case OUTPUT_APPLE: 
        case OUTPUT_CBM: output_mem_c64(fout, memblocks); break;
        }
        if (fout == stdout) fflush(fout);
        if (ferror(fout) && errno) err_msg_file(ERROR_CANT_WRTE_OBJ, arguments.output, &nopoint);
        if (fout != stdout) fclose(fout);
#ifdef _WIN32
        setmode(fileno(stdout), O_TEXT);
#endif
    }
}

void write_mem(struct memblocks_s * memblocks, uint8_t c) {
    if (memblocks->mem.p >= memblocks->mem.len) {
        memblocks->mem.len += 0x1000;
        memblocks->mem.data = (uint8_t *)realloc(memblocks->mem.data, memblocks->mem.len);
        if (!memblocks->mem.data || memblocks->mem.len < 0x1000) err_msg_out_of_memory(); /* overflow */
    }
    memblocks->mem.data[memblocks->mem.p++] = c;
}

static unsigned int omemp;
static size_t ptextaddr;
static address_t oaddr;

void mark_mem(const struct memblocks_s *memblocks, address_t adr) {
    ptextaddr = memblocks->mem.p;
    omemp = memblocks->p;
    oaddr = adr;
}

void get_mem(const struct memblocks_s *memblocks, size_t *memp, size_t *membp) {
    *memp = memblocks->mem.p;
    *membp = memblocks->p;
}

int16_t read_mem(const struct memblocks_s *memblocks, size_t memp, size_t membp, size_t offs) {
    size_t len;
    if (memp >= memblocks->mem.p) return -1;
    for (;;) {
        if (membp < memblocks->p) {
            len = memblocks->data[membp].len - (memp - memblocks->data[membp].p);
        } else {
            len = memblocks->mem.p - memp;
        }
        if (offs < len) return memblocks->mem.data[memp + offs];
        offs -= len;
        memp += len;
        if (membp + 1 < memblocks->p) {
            len = memblocks->data[membp + 1].addr - memblocks->data[membp].addr - memblocks->data[membp].len;
        } else if (membp < memblocks->p) {
            len = memblocks->lastaddr - memblocks->data[membp].addr - memblocks->data[membp].len;
        } else return -1;
        if (offs < len) return -1;
        offs -= len;
        membp++;
    }
}

void write_mark_mem(struct memblocks_s *memblocks, uint8_t c) {
    memblocks->mem.data[ptextaddr] = c;
}

void list_mem(const struct memblocks_s *memblocks, int dooutput) { 
    address_t myaddr;
    size_t len;
    int first = 1, print = 1;

    for (;omemp <= memblocks->p;omemp++, first = 0) {
        if (omemp < memblocks->p) {
            if (first && oaddr < memblocks->data[omemp].addr) {
                len = 0; myaddr = oaddr; omemp--;
            } else {
                len = memblocks->data[omemp].len - (ptextaddr - memblocks->data[omemp].p);
                myaddr = (memblocks->data[omemp].addr + memblocks->data[omemp].len - len) & all_mem;
            }
        } else {
            if (first && oaddr < memblocks->lastaddr) {
                len = 0; myaddr = oaddr; omemp--;
            } else {
                myaddr = memblocks->lastaddr + (ptextaddr - memblocks->lastp);
                len = memblocks->mem.p - ptextaddr;
                if (!len) {
                    if (!print) continue;
                    if (omemp) myaddr = (memblocks->data[omemp-1].addr + memblocks->data[omemp-1].len) & all_mem;
                    else myaddr = oaddr;
                }
            }
        }
        listing_mem(memblocks->mem.data + ptextaddr, dooutput ? len : 0, myaddr);
        print = 0;
        ptextaddr += len;
    }
}

void restart_memblocks(struct memblocks_s *memblocks, address_t address) {
    memblocks->mem.p = 0;
    memblocks->lastp = 0;
    memblocks->p = 0;
    memblocks->lastaddr = address;
    memblocks->compressed = 0;
}

void init_memblocks(struct memblocks_s *memblocks) {
    memblocks->mem.p = 0;
    memblocks->mem.len = 0;
    memblocks->mem.data = NULL;
    memblocks->p = 0;
    memblocks->len = 0;
    memblocks->lastp = 0;
    memblocks->lastaddr = 0;
    memblocks->data = NULL;
    memblocks->compressed = 0;
}

void destroy_memblocks(struct memblocks_s *memblocks) {
    free(memblocks->mem.data);
    free(memblocks->data);
}
