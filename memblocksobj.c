/*
    $Id: memblocksobj.c 3112 2023-09-06 06:34:22Z soci $

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
#include "memblocksobj.h"
#include <string.h>
#include <errno.h>
#include "values.h"
#include "error.h"
#include "section.h"
#include "unicode.h"
#include "arguments.h"
#include "version.h"

#include "typeobj.h"

static Type obj;

Type *const MEMBLOCKS_OBJ = &obj;

static FAST_CALL void destroy(Obj *o1) {
    size_t i;
    Memblocks *v1 = Memblocks(o1);
    free(v1->mem.data);
    for (i = 0; i < v1->p; i++) {
        const struct memblock_s *b = &v1->data[i];
        if (b->ref != NULL) val_destroy(Obj(b->ref));
    }
    free(v1->data);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    size_t i;
    const Memblocks *v1 = Memblocks(o1), *v2 = Memblocks(o2);
    if (o1->obj != o2->obj || v1->p != v2->p) return false;
    for (i = 0; i < v1->p; i++) {
        struct memblock_s *b1 = &v1->data[i];
        struct memblock_s *b2 = &v2->data[i];
        if (b1->p != b2->p || b1->addr != b2->addr || b1->len != b2->len) return false;
        if (b1->ref == b2->ref) continue;
        if (b1->ref == NULL || b2->ref == NULL) return false;
        if (!same(Obj(b1->ref), Obj(b2->ref))) return false;
    }
    return v1->mem.p == v2->mem.p && !memcmp(v1->mem.data, v2->mem.data, v1->mem.p);
}

MALLOC Memblocks *new_memblocks(address_t ln, size_t ln2) {
    Memblocks *val = Memblocks(val_alloc(MEMBLOCKS_OBJ));
    val->mem.p = 0;
    val->mem.len = ln;
    if (ln == 0) val->mem.data = NULL; else new_array(&val->mem.data, ln);
    val->p = 0;
    val->len = ln2;
    val->lastp = 0;
    val->lastaddr = 0;
    if (ln2 == 0) val->data = NULL; else new_array(&val->data, ln2);
    val->flattened = false;
    val->merged = false;
    val->enumeration = false;
    val->section = NULL;
    return val;
}

MALLOC Memblocks *copy_memblocks(Memblocks *m) {
    Memblocks *val = Memblocks(val_alloc(MEMBLOCKS_OBJ));
    size_t i;
    val->mem.p = m->mem.p;
    val->mem.len = m->mem.len;
    if (m->mem.len == 0) val->mem.data = NULL;
    else {
        new_array(&val->mem.data, m->mem.len);
        memcpy(val->mem.data, m->mem.data, m->mem.len);
    }
    val->p = m->p;
    val->len = m->p;
    val->lastp = m->lastp;
    val->lastaddr = m->lastaddr;
    if (m->p == 0) val->data = NULL; else new_array(&val->data, m->p);
    val->flattened = m->flattened;
    val->merged = m->merged;
    val->enumeration = m->enumeration;
    val->section = m->section;
    for (i = 0; i < m->p; i++) {
        const struct memblock_s *b = &m->data[i];
        val->data[i] = m->data[i];
        if (b->ref != NULL) val->data[i].ref = copy_memblocks(b->ref);
    }
    return val;
}

struct memblocks_print_s {
    FILE *f;
    unsigned int level;
    unsigned int max;
    const struct section_s *section;
};

static void sectionprint2(const struct section_s *section, FILE *f) {
    if (section->parent != NULL && section->parent->parent != NULL) {
        sectionprint2(section->parent, f);
        putc('.', f);
    }
    printable_print2(section->name.data, f, section->name.len);
}

static void rangeprint(const char *format, address_t start, address_t size, FILE *f) {
    char temp[10], temp2[10], temp3[10];

    sprintf(temp, "$%04" PRIxaddress, start);
    temp2[0] = 0;
    if (size != 0) {
        sprintf(temp2, "-$%04" PRIxaddress, start + size - 1U);
    }
    sprintf(temp3, "$%04" PRIxaddress, size);

    fprintf(f, format, size, temp, temp2, temp3);
}

static void sectionprint3(struct memblocks_print_s *state, bool next) {
    unsigned int i;
    for (i = (state->section != NULL) ? 1 : 0; i < state->level; i++) putc('|', state->f);
    if (state->section != NULL) {
        putc(next ? '.' : ' ', state->f);
        for (; i < state->max; i++) putc(next ? '-' : ' ', state->f);
        sectionprint2(state->section, state->f);
        state->section = NULL;
    }
    putc('\n', state->f);
}

static void sectionprint(struct memblocks_print_s *state, bool next) {
    if (state->section != NULL) {
        rangeprint("Section:%8" PRIuaddress " %7s%-8s %-7s ", state->section->address.start, state->section->size, state->f);
        sectionprint3(state, next);
    }
}

static int memblockprintcomp(const void *a, const void *b) {
    const struct memblock_s *aa = *(const struct memblock_s **)a;
    const struct memblock_s *bb = *(const struct memblock_s **)b;
    if (aa->addr != bb->addr) return (aa->addr > bb->addr) ? 1 : -1;
    if ((aa->ref == NULL) == (bb->ref == NULL) && aa->len != bb->len) return (aa->len > bb->len) ? 1 : -1;
    if (aa == bb) return 0;
    return aa > bb ? 1 : -1;
}

static bool memblockprint(const Memblocks *mem, struct memblocks_print_s *state) {
    size_t ln;
    address_t addr, end;
    bool root;
    bool data = false;

    root = (state->level == 0);
    if (root) {
        addr = end = 0;
    } else {
        addr = state->section->address.start;
        end = state->section->address.start + state->section->size;
    }
    ln = mem->p;
    if (ln != 0) {
        struct memblock_s **memblocks;
        size_t i;
        new_array(&memblocks, ln);
        for (i = 0; i < ln; i++) memblocks[i] = mem->data + i;
        qsort(memblocks, ln, sizeof *memblocks, memblockprintcomp);
        i = 0;
        while (i < ln) {
            const struct memblock_s *block = memblocks[i];
            address_t start = block->addr;
            if (start > addr) {
                if (data || !root) {
                    sectionprint(state, true);
                    rangeprint(root ? "Gap: %11" PRIuaddress " %7s%-8s %s" : "Gap: %11" PRIuaddress " %7s%-8s %-7s ", addr, start - addr, state->f);
                    sectionprint3(state, true);
                }
            }
            if (block->addr + block->len > addr) addr = block->addr + block->len;

            i++;
            if (block->ref != NULL) {
                sectionprint(state, true);
                state->level++;
                state->section = block->ref->section;
                if (memblockprint(block->ref, state)) data = true;
                state->level--;
                continue;
            }
            for (; i < ln; i++) {
                block = memblocks[i];
                if (block->ref != NULL) break;
                if (block->addr > addr) break;
                if (block->addr + block->len > addr) addr = block->addr + block->len;
            }
            if (i != ln || (end > addr && !root)) sectionprint(state, true);
            rangeprint(state->section == NULL && root ? "Data: %10" PRIuaddress " %7s%-8s %s" : "Data: %10" PRIuaddress " %7s%-8s %-7s ", start, addr - start, state->f);
            sectionprint3(state, i != ln || (end > addr && !root));
            data = true;
        }
        free(memblocks);
    }
    if (end > addr && !root) {
        rangeprint("Gap: %11" PRIuaddress " %7s%-8s %-7s ", addr, end - addr, state->f);
        sectionprint3(state, false);
    }
    sectionprint(state, false);
    return data;
}

static unsigned int memblocklevel(const Memblocks *mem, unsigned int level) {
    size_t i;
    unsigned int next, ret = level;
    for (i = 0; i < mem->p; i++) {
        const struct memblock_s *block = mem->data + i;
        if (block->ref == NULL) continue;
        next = memblocklevel(block->ref, level + 1);
        if (next > ret) ret = next;
    }
    return ret;
}

void printmemorymap(const Memblocks *mem) {
    struct memblocks_print_s state;
    state.f = stdout;
    state.level = 0;
    state.max = memblocklevel(mem, 0);
    state.section = NULL;
    memblockprint(mem, &state);
}

void memorymapfile(const Memblocks *mem, const struct output_s *output) {
    struct memblocks_print_s state;
    int err;

    state.f = dash_name(output->mapname) ? stdout : fopen_utf8(output->mapname, output->mapappend ? "at" : "wt");
    if (state.f == NULL) {
        err_msg_file2(ERROR_CANT_WRTE_MAP, output->mapname, &output->mapname_pos);
        return;
    }
    if (state.f == stdout && fflush(state.f) != 0) setvbuf(state.f, NULL, _IOLBF, 1024);
    clearerr(state.f); errno = 0;

    if (!output->mapappend) fputs("\n64tass Turbo Assembler Macro V" VERSION " memory map file\n", state.f);
    fputs("\nMemory map for output file: ", state.f);
    argv_print(output->name, state.f);
    fputs("\n\nType        Size      Range      Size    Name\n", state.f);

    state.level = 0;
    state.max = memblocklevel(mem, 0);
    state.section = NULL;
    memblockprint(mem, &state);

    err = ferror(state.f);
    err |= (state.f != stdout) ? fclose(state.f) : fflush(state.f);
    if (err != 0 && errno != 0) err_msg_file2(ERROR_CANT_WRTE_MAP, output->mapname, &output->mapname_pos);
}


void memblocksobj_init(void) {
    Type *type = new_type(&obj, T_MEMBLOCKS, "memblocks", sizeof(Memblocks));
    type->destroy = destroy;
    type->same = same;
}
