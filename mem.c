/*
    $Id: mem.c 3112 2023-09-06 06:34:22Z soci $

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
#if defined _WIN32 || defined __MSDOS__ || defined __DOS__
#include <io.h>
#include <fcntl.h>
#define SETMODE_AVAILABLE
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#endif
#include <errno.h>
#include "error.h"
#include "unicode.h"
#include "64tass.h"
#include "listing.h"
#include "arguments.h"
#include "values.h"
#include "memblocksobj.h"

static int memblockcomp(const void *a, const void *b) {
    address_t aa = ((const struct memblock_s *)a)->addr;
    address_t bb = ((const struct memblock_s *)b)->addr;
    if (aa < bb) return -1;
    return (aa > bb) ? 1 : 0;
}

static void memcomp(Memblocks *memblocks, bool nomerge) {
    size_t i, j, k;
    if (!memblocks->flattened) {
        memblocks->flattened = true;

        for (j = 0; j < memblocks->p; j++) { /* replace references with real copies */
            Memblocks *b = memblocks->data[j].ref;
            if (b != NULL) {
                size_t rest;
                memcomp(b, nomerge);
                rest = memblocks->p - j - 1;
                memblocks->p += b->p - 1;
                if (memblocks->p >= memblocks->len) {
                    if (add_overflow(memblocks->p, 64, &memblocks->len)) err_msg_out_of_memory();
                    resize_array(&memblocks->data, memblocks->len);
                }
                memmove(&memblocks->data[j + b->p], &memblocks->data[j + 1], rest * sizeof *memblocks->data);
                for (k = 0; k < b->p; k++) {
                    struct memblock_s *b2 = &memblocks->data[j + k];
                    b2->p = memblocks->mem.p;
                    b2->len = b->data[k].len;
                    b2->ref = NULL;
                    b2->addr = b->data[k].addr;
                    memblocks->mem.p += b2->len;
                    if (memblocks->mem.p >= memblocks->mem.len) {
                        if (add_overflow(memblocks->mem.p, 0x1000, &memblocks->mem.len)) err_msg_out_of_memory();
                        resize_array(&memblocks->mem.data, memblocks->mem.len);
                    }
                    memcpy(&memblocks->mem.data[b2->p], &b->mem.data[b->data[k].p], b2->len);
                }
                j--;
                val_destroy(Obj(b));
            }
        }
    }
    if (memblocks->p < 2 || nomerge || memblocks->merged) return;
    memblocks->merged = true;

    for (k = j = 0; j < memblocks->p; j++) {
        struct memblock_s *bj = &memblocks->data[j];
        if (bj->len != 0) {
            for (i = j + 1; i < memblocks->p; i++) if (memblocks->data[i].len != 0) {
                struct memblock_s *bi = &memblocks->data[i];
                if (bj->addr <= bi->addr && (bj->addr + bj->len) > bi->addr) {
                    address_t overlap = (bj->addr + bj->len) - bi->addr;
                    if (overlap > bi->len) overlap = bi->len;
                    memcpy(memblocks->mem.data + bj->p + (bi->addr - bj->addr), memblocks->mem.data + bi->p, overlap);
                    bi->len -= overlap;
                    bi->p += overlap;
                    bi->addr += overlap;
                    continue;
                }
                if (bi->addr <= bj->addr && (bi->addr + bi->len) > bj->addr) {
                    address_t overlap = bi->addr + bi->len - bj->addr;
                    if (overlap > bj->len) overlap = bj->len;
                    bj->addr += overlap;
                    bj->p += overlap;
                    bj->len -= overlap;
                    if (bj->len == 0) break;
                }
            }
            if (bj->len != 0) {
                if (j != k) memblocks->data[k] = *bj;
                k++;
            }
        }
    }
    memblocks->p = k;
    qsort(memblocks->data, memblocks->p, sizeof *memblocks->data, memblockcomp);
}

void memjmp(Memblocks *memblocks, address_t adr) {
    struct memblock_s *block;
    if (memblocks->mem.p == memblocks->lastp) {
        memblocks->lastaddr = adr;
        return;
    }
    if (memblocks->p >= memblocks->len) extend_array(&memblocks->data, &memblocks->len, 64);
    block = &memblocks->data[memblocks->p++];
    block->len = memblocks->mem.p - memblocks->lastp;
    block->p = memblocks->lastp;
    block->ref = NULL;
    block->addr = memblocks->lastaddr;
    memblocks->lastp = memblocks->mem.p;
    memblocks->lastaddr = adr;
}

void memclose(Memblocks *memblocks) {
    size_t j;
    for (j = 0; j < memblocks->p; j++) {
        Memblocks *b = memblocks->data[j].ref;
        if (b != NULL) memclose(b);
    }
    memjmp(memblocks, 0);
}

void memref(Memblocks *memblocks, Memblocks *ref, address_t addr, address_t ln) {
    struct memblock_s *block;
    if (memblocks->p >= memblocks->len) extend_array(&memblocks->data, &memblocks->len, 64);
    block = &memblocks->data[memblocks->p++];
    block->len = ln;
    block->p = memblocks->lastp;
    block->ref = Memblocks(val_reference(Obj(ref)));
    block->addr = addr;
}

#if defined _POSIX_C_SOURCE || defined __unix__
#define USE_HOLES
static MUST_CHECK bool padding(FILE *f, address_t size, bool append)
#else
static MUST_CHECK bool padding(FILE *f, address_t size, bool UNUSED(append))
#endif
{
    uint8_t nuls[256];
#ifdef USE_HOLES
    if (!append) {
        while (size >= 0x80000000) {
            if (fseek(f, 0x40000000, SEEK_CUR) != 0) goto err;
            size -= 0x40000000;
        }
        if ((long)size > 256 && fseek(f, (long)size, SEEK_CUR) == 0) {
            return false;
        }
    }
err:
#endif
    nuls[0] = 1;
    while (size != 0) {
        address_t db = size < sizeof nuls ? size : sizeof nuls;
        size -= db;
        if (nuls[0] != 0) memset(nuls, 0, db);
        if (fwrite(nuls, db, 1, f) == 0) return true;
    }
    return false;
}

static void output_mem_raw(FILE *fout, const Memblocks *memblocks, const struct output_s *output) {
    address_t pos, end;
    size_t i;
    uint8_t header[8];

    if (memblocks->p == 0) return;
    pos = memblocks->data[0].addr;
    switch (output->mode) {
    case OUTPUT_PGX:
        memcpy(header, "PGX\x01", 4);
        header[4] = (uint8_t)pos;
        header[5] = (uint8_t)(pos >> 8);
        header[6] = (uint8_t)(pos >> 16);
        header[7] = (uint8_t)(pos >> 24);
        i = 8;
        break;
    case OUTPUT_CBM:
        header[0] = (uint8_t)pos;
        header[1] = (uint8_t)(pos >> 8);
        if (output->longaddr) {
            header[2] = (uint8_t)(pos >> 16);
            i = 3;
        } else i = 2;
        break;
    case OUTPUT_APPLE:
        header[0] = (uint8_t)pos;
        header[1] = (uint8_t)(pos >> 8);
        end = memblocks->data[memblocks->p - 1].addr + memblocks->data[memblocks->p - 1].len;
        end -= pos;
        header[2] = (uint8_t)end;
        header[3] = (uint8_t)(end >> 8);
        i = 4;
        break;
    default:
        i = 0;
        break;
    }
    if (i != 0 && fwrite(header, i, 1, fout) == 0) return;
    for (i = 0; i < memblocks->p; i++) {
        const struct memblock_s *block = &memblocks->data[i];
        if (padding(fout, block->addr - pos, output->append)) return;
        if (fwrite(memblocks->mem.data + block->p, block->len, 1, fout) == 0) return;
        pos = block->addr + block->len;
    }
}

static void output_mem_nonlinear(FILE *fout, const Memblocks *memblocks, bool longaddr) {
    size_t i, j;
    uint8_t header[6];
    for (i = 0; i < memblocks->p;) {
        const struct memblock_s *block = &memblocks->data[i];
        address_t start = block->addr;
        address_t size = block->len;
        for (j = i + 1; j < memblocks->p; j++) {
            const struct memblock_s *b = &memblocks->data[j];
            address_t addr = start + size;
            if (b->addr != addr || addr < start) break;
            size += b->len;
        }
        header[0] = (uint8_t)size;
        header[1] = (uint8_t)(size >> 8);
        if (longaddr) {
            header[2] = (uint8_t)(size >> 16);
            header[3] = (uint8_t)start;
            header[4] = (uint8_t)(start >> 8);
            header[5] = (uint8_t)(start >> 16);
        } else {
            header[2] = (uint8_t)start;
            header[3] = (uint8_t)(start >> 8);
        }
        if (fwrite(header, longaddr ? 6 : 4, 1, fout) == 0) return;
        for (;i < j; i++) {
            const struct memblock_s *b = &memblocks->data[i];
            if (fwrite(memblocks->mem.data + b->p, b->len, 1, fout) == 0) return;
        }
    }
    memset(header, 0, 4);
    fwrite(header, longaddr ? 3 : 2, 1, fout);
}

static void output_mem_c256_pgz(FILE *fout, const Memblocks *memblocks, const struct output_s *output) {
    size_t i, j;
    uint8_t header[7];
    unsigned int p = 0;
    header[0] = 'Z';
    for (i = 0; i < memblocks->p; p = 1) {
        const struct memblock_s *block = &memblocks->data[i];
        address_t start = block->addr;
        address_t size = block->len;
        for (j = i + 1; j < memblocks->p; j++) {
            const struct memblock_s *b = &memblocks->data[j];
            address_t addr = start + size;
            if (b->addr != addr || addr < start) break;
            size += b->len;
        }
        header[1] = (uint8_t)start;
        header[2] = (uint8_t)(start >> 8);
        header[3] = (uint8_t)(start >> 16);
        header[4] = (uint8_t)size;
        header[5] = (uint8_t)(size >> 8);
        header[6] = (uint8_t)(size >> 16);
        if (fwrite(header + p, 7 - p, 1, fout) == 0) return;
        for (;i < j; i++) {
            const struct memblock_s *b = &memblocks->data[i];
            if (fwrite(memblocks->mem.data + b->p, b->len, 1, fout) == 0) return;
        }
    }
    if (output->exec_pos.pos != 0) {
        header[1] = (uint8_t)output->exec;
        header[2] = (uint8_t)(output->exec >> 8);
        header[3] = (uint8_t)(output->exec >> 16);
        header[6] = header[5] = header[4] = 0;
        fwrite(header + p, 7 - p, 1, fout);
    }
}

static void output_mem_flat(FILE *fout, const Memblocks *memblocks, bool append) {
    address_t pos = 0;
    size_t i;

    for (i = 0; i < memblocks->p; i++) {
        const struct memblock_s *block = &memblocks->data[i];
        if (padding(fout, block->addr - pos, append)) return;
        if (fwrite(memblocks->mem.data + block->p, block->len, 1, fout) == 0) return;
        pos = block->addr + block->len;
    }
}

static void output_mem_atari_xex(FILE *fout, const Memblocks *memblocks, const struct output_s *output) {
    size_t i, j;
    uint8_t header[8];
    unsigned int p = 0;
    header[0] = 0xff;
    header[1] = 0xff;
    for (i = 0; i < memblocks->p; p = 2) {
        const struct memblock_s *block = &memblocks->data[i];
        address_t end, start = block->addr;
        address_t size = block->len;
        for (j = i + 1; j < memblocks->p; j++) {
            const struct memblock_s *b = &memblocks->data[j];
            address_t addr = start + size;
            if (b->addr != addr || addr < start) break;
            size += b->len;
        }
        if (start == 0xffff) p = 0;
        header[2] = (uint8_t)start;
        header[3] = (uint8_t)(start >> 8);
        end = start + size - 1;
        header[4] = (uint8_t)end;
        header[5] = (uint8_t)(end >> 8);
        if (fwrite(header + p, 6 - p, 1, fout) == 0) return;
        for (;i < j; i++) {
            const struct memblock_s *b = &memblocks->data[i];
            if (fwrite(memblocks->mem.data + b->p, b->len, 1, fout) == 0) return;
        }
    }
    if (output->exec_pos.pos != 0) {
        header[2] = 0xe0;
        header[3] = 0x02;
        header[4] = 0xe1;
        header[5] = 0x02;
        header[6] = (uint8_t)output->exec;
        header[7] = (uint8_t)(output->exec >> 8);
        fwrite(header + p, 8 - p, 1, fout);
    }
}

struct hexput_s {
    char *line;
    unsigned int sum;
};

static void hexput(struct hexput_s *h, unsigned int b) {
    const char *hex = "0123456789ABCDEF";
    h->sum += b;
    *h->line++ = hex[(b >> 4) & 0xf];
    *h->line++ = hex[b & 0xf];
}

enum { IHEX_LENGTH = 32U };

struct ihex_s {
    FILE *file;
    address_t address, segment;
    uint8_t data[IHEX_LENGTH];
    unsigned int length;
};

static MUST_CHECK bool output_mem_ihex_line(struct ihex_s *ihex, unsigned int length, address_t address, unsigned int type, const uint8_t *data) {
    unsigned int i;
    char line[1+(1+2+1+IHEX_LENGTH+1)*2+1];
    struct hexput_s h;
    h.line = line + 1;
    h.sum =  0;
    line[0] = ':';
    hexput(&h, length);
    hexput(&h, address >> 8);
    hexput(&h, address);
    hexput(&h, type);
    for (i = 0; i < length; i++) {
        hexput(&h, data[i]);
    }
    hexput(&h, -h.sum);
    *h.line++ = '\n';
    return fwrite(line, (size_t)(h.line - line), 1, ihex->file) == 0;
}

static MUST_CHECK bool output_mem_ihex_data(struct ihex_s *ihex) {
    const uint8_t *data = ihex->data;
    if ((ihex->address & 0xffff) + ihex->length > 0x10000) {
        uint16_t length = (uint16_t)-ihex->address;
        unsigned int remains = ihex->length - length;
        ihex->length = length;
        if (output_mem_ihex_data(ihex)) return true;
        data += length;
        ihex->length = remains;
    }
    if (((ihex->address ^ ihex->segment) & ~(address_t)0xffff) != 0) {
        uint8_t ez[2];
        ez[0] = (uint8_t)(ihex->address >> 24);
        ez[1] = (uint8_t)(ihex->address >> 16);
        if (output_mem_ihex_line(ihex, sizeof ez, 0, 4, ez)) return true;
        ihex->segment = ihex->address;
    }
    if (output_mem_ihex_line(ihex, ihex->length, ihex->address, 0, data)) return true;
    ihex->address += ihex->length;
    ihex->length = 0;
    return false;
}

static void output_mem_ihex(FILE *fout, const Memblocks *memblocks, const struct output_s *output) {
    struct ihex_s ihex;
    size_t i;

    ihex.file = fout;
    ihex.address = 0;
    ihex.segment = 0;
    ihex.length = 0;
    for (i = 0; i < memblocks->p; i++) {
        const struct memblock_s *b = &memblocks->data[i];
        address_t end = b->addr + b->len - 1;
        if (end >= 0x10000) {
            ihex.segment = ~memblocks->data[0].addr;
            break;
        }
    }
    for (i = 0; i < memblocks->p; i++) {
        const struct memblock_s *b = &memblocks->data[i];
        const uint8_t *d = memblocks->mem.data + b->p;
        address_t addr = b->addr;
        address_t blen = b->len;
        if (blen != 0 && ihex.address + ihex.length != addr) {
            if (ihex.length != 0) {
                if (output_mem_ihex_data(&ihex)) return;
            }
            ihex.address = addr;
        }
        while (blen != 0) {
            unsigned int left = IHEX_LENGTH - ihex.length;
            address_t copy = blen > left ? left : blen;
            memcpy(ihex.data + ihex.length, d, copy);
            ihex.length += copy;
            d += copy;
            blen -= copy;
            if (ihex.length == sizeof ihex.data) {
                if (output_mem_ihex_data(&ihex)) return;
            }
        }
    }
    if (ihex.length != 0) {
        if (output_mem_ihex_data(&ihex)) return;
    }
    if (output->exec_pos.pos != 0) {
        uint8_t ez[4];
        ez[0] = (uint8_t)(output->exec >> 24);
        ez[1] = (uint8_t)(output->exec >> 16);
        ez[2] = (uint8_t)(output->exec >> 8);
        ez[3] = (uint8_t)output->exec;
        if (output_mem_ihex_line(&ihex, sizeof ez, 0, 5, ez)) return;
    }
    if (output_mem_ihex_line(&ihex, 0, 0, 1, NULL)) return;
}

enum { MHEX_LENGTH = 24U };

struct mhex_s {
    FILE *file;
    address_t address;
    unsigned int lines;
    uint8_t data[MHEX_LENGTH];
    unsigned int length;
};

static MUST_CHECK bool output_mem_mhex_line(struct mhex_s *mhex, unsigned int length, address_t address, const uint8_t *data) {
    unsigned int i;
    char line[1+(1+2+MHEX_LENGTH+2)*2+2];
    struct hexput_s h;
    h.line = line + 1;
    h.sum =  0;
    line[0] = ';';
    hexput(&h, length);
    hexput(&h, address >> 8);
    hexput(&h, address & 0xff);
    for (i = 0; i < length; i++) {
        hexput(&h, data[i]);
    }
    i = h.sum;
    hexput(&h, i >> 8);
    hexput(&h, i);
    *h.line++ = '\r';
    *h.line++ = '\n';
    return fwrite(line, (size_t)(h.line - line), 1, mhex->file) == 0;
}

static MUST_CHECK bool output_mem_mhex_data(struct mhex_s *mhex) {
    const uint8_t *data = mhex->data;
    if ((mhex->address & 0xffff) + mhex->length > 0x10000) {
        uint16_t length = (uint16_t)-mhex->address;
        unsigned int remains = mhex->length - length;
        mhex->length = length;
        if (output_mem_mhex_data(mhex)) return true;
        data += length;
        mhex->length = remains;
    }
    if (output_mem_mhex_line(mhex, mhex->length, mhex->address, data)) return true;
    mhex->lines++;
    mhex->address += mhex->length;
    mhex->length = 0;
    return false;
}

static void output_mem_mhex(FILE *fout, const Memblocks *memblocks) {
    struct mhex_s mhex;
    size_t i;

    mhex.file = fout;
    mhex.address = 0;
    mhex.lines = 0;
    mhex.length = 0;
    for (i = 0; i < memblocks->p; i++) {
        const struct memblock_s *b = &memblocks->data[i];
        const uint8_t *d = memblocks->mem.data + b->p;
        address_t addr = b->addr;
        address_t blen = b->len;
        if (blen != 0 && mhex.address + mhex.length != addr) {
            if (mhex.length != 0) {
                if (output_mem_mhex_data(&mhex)) return;
            }
            mhex.address = addr;
        }
        while (blen != 0) {
            unsigned int left = MHEX_LENGTH - mhex.length;
            address_t copy = blen > left ? left : blen;
            memcpy(mhex.data + mhex.length, d, copy);
            mhex.length += copy;
            d += copy;
            blen -= copy;
            if (mhex.length == sizeof mhex.data) {
                if (output_mem_mhex_data(&mhex)) return;
            }
        }
    }
    if (mhex.length != 0) {
        if (output_mem_mhex_data(&mhex)) return;
    }
    if (output_mem_mhex_line(&mhex, 0, mhex.lines, NULL)) return;
}

enum { SRECORD_LENGTH = 32U };

struct srecord_s {
    FILE *file;
    unsigned int count;
    char rectype;
    unsigned int addrtype;
    unsigned int length;
    address_t address;
    uint8_t data[SRECORD_LENGTH];
};

static MUST_CHECK bool output_mem_srec_line(struct srecord_s *srec) {
    unsigned int i;
    char line[1+1+(1+4+SRECORD_LENGTH+1)*2+1];
    struct hexput_s h;
    h.line = line + 2;
    h.sum = 0;
    line[0] = 'S';
    line[1] = srec->rectype;
    hexput(&h, srec->length + srec->addrtype + 3);
    if (srec->addrtype > 1) hexput(&h, srec->address >> 24);
    if (srec->addrtype > 0) hexput(&h, srec->address >> 16);
    hexput(&h, srec->address >> 8);
    hexput(&h, srec->address);
    for (i = 0; i < srec->length; i++) {
        hexput(&h, srec->data[i]);
    }
    hexput(&h, ~h.sum);
    *h.line++ = '\n';
    srec->address += srec->length;
    srec->length = 0;
    srec->count++;
    return fwrite(line, (size_t)(h.line - line), 1, srec->file) == 0;
}

static void output_mem_srec(FILE *fout, const Memblocks *memblocks, const struct output_s *output) {
    struct srecord_s srec;
    size_t i;
    unsigned int addrtype;

    srec.file = fout;
    srec.count = 0;
    srec.addrtype = 0;
    srec.address = 0;
    srec.length = 3;
    srec.rectype = '0';
    srec.data[0] = 'H';
    srec.data[1] = 'D';
    srec.data[2] = 'R';
    if (output_mem_srec_line(&srec)) return;
    addrtype = 0;
    for (i = 0; i < memblocks->p; i++) {
        const struct memblock_s *b = &memblocks->data[i];
        address_t end = b->addr + b->len - 1;
        if (end >= 0x10000 && addrtype < 1) addrtype = 1;
        if (end >= 0x1000000 && addrtype < 2) {
            addrtype = 2;
            break;
        }
    }
    srec.addrtype = addrtype;
    srec.rectype = (char)('1' + addrtype);
    for (i = 0; i < memblocks->p; i++) {
        const struct memblock_s *b = &memblocks->data[i];
        const uint8_t *d = memblocks->mem.data + b->p;
        address_t addr = b->addr;
        address_t blen = b->len;
        if (blen != 0 && srec.address + srec.length != addr) {
            if (srec.length != 0) {
                if (output_mem_srec_line(&srec)) return;
            }
            srec.address = addr;
        }
        while (blen != 0) {
            unsigned int left = SRECORD_LENGTH - srec.length;
            address_t copy = blen > left ? left : blen;
            memcpy(srec.data + srec.length, d, copy);
            srec.length += copy;
            d += copy;
            blen -= copy;
            if (srec.length == sizeof srec.data) {
                if (output_mem_srec_line(&srec)) return;
            }
        }
    }
    if (srec.length != 0) {
        if (output_mem_srec_line(&srec)) return;
    }
    if (srec.count <= 0x1000000) {
        if (srec.count <= 0x10000) {
            srec.addrtype = 0;
            srec.rectype = '5';
        } else {
            srec.addrtype = 1;
            srec.rectype = '6';
        }
        srec.address = srec.count - 1;
        if (output_mem_srec_line(&srec)) return;
    }
    srec.addrtype = addrtype;
    srec.rectype = (char)('9' - addrtype);
    if (output->exec_pos.pos != 0) {
        srec.address = output->exec;
    } else {
        srec.address = (memblocks->p == 0) ? 0 : memblocks->data[0].addr;
    }
    if (output_mem_srec_line(&srec)) return;
}

void output_mem(Memblocks *memblocks, const struct output_s *output) {
    FILE* fout;
    bool binary = (output->mode != OUTPUT_IHEX) && (output->mode != OUTPUT_SREC);
    int err;
#ifdef SETMODE_AVAILABLE
    int oldmode = -1;
#endif

    if (dash_name(output->name)) {
        if (fflush(stdout) != 0 || binary) setvbuf(stdout, NULL, binary ? _IOFBF : _IOLBF, binary ? BUFSIZ : 1024);
#ifdef SETMODE_AVAILABLE
        if (binary) oldmode = setmode(STDOUT_FILENO, O_BINARY);
#endif
        fout = stdout;
    } else {
        fout = fopen_utf8(output->name, output->append ? (binary ? "ab" : "at") : (binary ? "wb" : "wt"));
    }
    if (fout == NULL) {
        err_msg_file2(ERROR_CANT_WRTE_OBJ, output->name, &output->name_pos);
        return;
    }
    memcomp(memblocks, output->mode == OUTPUT_XEX || output->mode == OUTPUT_IHEX || output->mode == OUTPUT_SREC || output->mode == OUTPUT_MHEX);

    clearerr(fout); errno = 0;
    switch (output->mode) {
    case OUTPUT_FLAT: output_mem_flat(fout, memblocks, output->append); break;
    case OUTPUT_NONLINEAR: output_mem_nonlinear(fout, memblocks, output->longaddr); break;
    case OUTPUT_PGZ: output_mem_c256_pgz(fout, memblocks, output); break;
    case OUTPUT_XEX: output_mem_atari_xex(fout, memblocks, output); break;
    case OUTPUT_PGX:
    case OUTPUT_RAW:
    case OUTPUT_APPLE:
    case OUTPUT_CBM: output_mem_raw(fout, memblocks, output); break;
    case OUTPUT_IHEX: output_mem_ihex(fout, memblocks, output); break;
    case OUTPUT_SREC: output_mem_srec(fout, memblocks, output); break;
    case OUTPUT_MHEX: output_mem_mhex(fout, memblocks); break;
    }
    err = ferror(fout);
    err |= (fout != stdout) ? fclose(fout) : fflush(fout);
    if (err != 0 && errno != 0) err_msg_file2(ERROR_CANT_WRTE_OBJ, output->name, &output->name_pos);
#ifdef SETMODE_AVAILABLE
    if (oldmode >= 0) setmode(STDOUT_FILENO, oldmode);
#endif
    if (fout == stdout && binary) setvbuf(stdout, NULL, _IOLBF, 1024);
}

FAST_CALL uint8_t *alloc_mem(Memblocks *memblocks, address_t len) {
    address_t p;
    uint8_t *d;
    if (add_overflow(memblocks->mem.p, len, &p)) err_msg_out_of_memory();
    if (p > memblocks->mem.len) {
        if (add_overflow(p, 0x1000, &memblocks->mem.len)) err_msg_out_of_memory();
        resize_array(&memblocks->mem.data, memblocks->mem.len);
    }
    d = memblocks->mem.data + memblocks->mem.p;
    do {
        address_t left = all_mem2 - (memblocks->lastaddr + memblocks->mem.p - memblocks->lastp);
        if (len <= left) break;
        memblocks->mem.p += left + 1;
        len -= left + 1;
        memjmp(memblocks, 0);
    } while (true);
    memblocks->mem.p = p;
    return d;
}

void mark_mem(struct mem_mark_s *mm, const Memblocks *memblocks, address_t adr, address_t adr2) {
    mm->ptextaddr = memblocks->mem.p;
    mm->omemp = memblocks->p;
    mm->olastaddr = memblocks->lastaddr;
    mm->oaddr = adr;
    mm->oaddr2 = adr2;
}

size_t get_mem(const Memblocks *memblocks) {
    return memblocks->p;
}

int read_mem(const Memblocks *memblocks, address_t raddr, size_t membp, address_t offs) {
    address_t addr, diff;
    address_t len;
    if (membp < memblocks->p) {
        addr = memblocks->data[membp].addr;
    } else {
        addr = memblocks->lastaddr;
    }
    if (raddr > addr) {
        offs += raddr - addr;
        raddr = addr;
    }
    for (; membp < memblocks->p; membp++) {
        addr = memblocks->data[membp].addr;
        diff = (addr - raddr) & all_mem2;
        if (diff > offs) return -1;
        offs -= diff;
        len = memblocks->data[membp].len;
        if (offs < len) return memblocks->mem.data[memblocks->data[membp].p + offs];
        offs -= len;
        raddr = (addr + len) & all_mem2;
    }
    addr = memblocks->lastaddr;
    diff = (addr - raddr) & all_mem2;
    if (diff > offs) return -1;
    offs -= diff;
    len = memblocks->mem.p - memblocks->lastp;
    if (offs < len) return memblocks->mem.data[memblocks->lastp + offs];
    return -1;
}

void write_mark_mem(const struct mem_mark_s *mm, Memblocks *memblocks, unsigned int c) {
    memblocks->mem.data[mm->ptextaddr] = (uint8_t)c;
}

void list_mem(const struct mem_mark_s *mm, const Memblocks *memblocks) {
    bool first = true;
    size_t o = mm->omemp;
    address_t addr2 = mm->oaddr;

    for (; o <= memblocks->p; o++) {
        address_t p;
        address_t addr, len;

        if (o < memblocks->p) {
            addr = memblocks->data[o].addr;
            p = memblocks->data[o].p;
            len = memblocks->data[o].len;
        } else {
            addr = memblocks->lastaddr;
            p = memblocks->lastp;
            len = memblocks->mem.p - p;
        }

        if (first) {
            first = false;
            if (addr != mm->olastaddr) {
                len = 0;
                o--;
            } else {
                address_t diff = (addr2 - addr) & all_mem2;
                if (diff > len) continue;
                p += diff;
                len -= diff;
            }
            addr = addr2;
        } else {
            if (len == 0) continue;
        }
        listing_mem(memblocks->mem.data + p, len, addr, (mm->oaddr2 + addr - addr2) & all_mem);
    }
}
