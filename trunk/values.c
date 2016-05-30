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
#include <string.h>
#include "values.h"
#include "obj.h"

#include "unicode.h"
#include "error.h"
#include "strobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "variables.h"

#define SLOTS 128
#define ALIGN sizeof(int *)

typedef struct Slot {
    Obj v;
    struct Slot *next;
} Slot;

static Slot *values_free[32];

typedef struct Slotcoll {
    struct Slotcoll *next;
} Slotcoll;

static Slotcoll *slotcoll[32];

static inline void value_free(Obj *val) {
    size_t p = (val->obj->length + (ALIGN - 1)) / ALIGN;
    Slot *slot = (Slot *)val, **c = &values_free[p];
    slot->next = *c;
    val->obj = NONE_OBJ;
    *c = slot;
}

Obj *val_alloc(Type *obj) {
    size_t p = (obj->length + (ALIGN - 1)) / ALIGN;
    Slot **c = &values_free[p];
    Obj *val = (Obj *)*c;
    if (val == NULL) {
        size_t i, size = p * ALIGN;
        Slot *slot;
        Slotcoll **s = &slotcoll[p];
        Slotcoll *n = (Slotcoll *)mallocx(size * SLOTS + sizeof *n);
        n->next = *s; *s = n;
        slot = (Slot *)(n + 1);
        val = (Obj *)slot;
        for (i = 0; i < (SLOTS - 1); i++, slot = (Slot *)(((const char *)slot) + size)) {
            slot->v.obj = NONE_OBJ;
            slot->v.refcount = 1;
            slot->next = (Slot *)(((const char *)slot) + size);
        }
        slot->v.obj = NONE_OBJ;
        slot->v.refcount = 1;
        slot->next = NULL;
    }
    *c = ((Slot *)val)->next;
    val->obj = obj;
    return val;
}

static inline void obj_destroy(Obj *v1) {
    v1->obj->destroy(v1);
}

void garbage_collect(void) {
    Slotcoll *vals;
    size_t i, j;
    destroy_lastlb();

    for (j = 0; j < lenof(slotcoll); j++) {
        size_t size = j * ALIGN;
        for (vals = slotcoll[j]; vals != NULL; vals = vals->next) {
            Obj *val = (Obj *)(vals + 1);
            for (i = 0; i < SLOTS; i++, val = (Obj *)(((const char *)val) + size)) {
                if (val->obj->garbage != NULL) {
                    val->obj->garbage(val, -1);
                    val->refcount |= SIZE_MSB;
                }
            }
        }
    }

    for (j = 0; j < lenof(slotcoll); j++) {
        size_t size = j * ALIGN;
        for (vals = slotcoll[j]; vals != NULL; vals = vals->next) {
            Obj *val = (Obj *)(vals + 1);
            for (i = 0; i < SLOTS; i++, val = (Obj *)(((const char *)val) + size)) {
                if (val->obj->garbage != NULL) {
                    if (val->refcount > SIZE_MSB) {
                        val->refcount -= SIZE_MSB;
                        val->obj->garbage(val, 1);
                    }
                }
            }
        }
    }

    for (j = 0; j < lenof(slotcoll); j++) {
        size_t size = j * ALIGN;
        for (vals = slotcoll[j]; vals != NULL; vals = vals->next) {
            Obj *val = (Obj *)(vals + 1);
            for (i = 0; i < SLOTS; i++, val = (Obj *)(((const char *)val) + size)) {
                if ((val->refcount & ~SIZE_MSB) == 0) {
                    val->refcount = 1;
                    if (val->obj->garbage != NULL) val->obj->garbage(val, 0);
                    else obj_destroy(val);
                    value_free(val);
                }
            }
        }
    }
}

void val_destroy(Obj *val) {
    if (val->refcount == 0) {
        obj_destroy(val);
        return;
    }
    if (val->refcount == 1) {
        obj_destroy(val);
        value_free(val);
    } else val->refcount--;
}

void val_replace(Obj **val, Obj *val2) {
    if (*val == val2) return;
    val_destroy(*val);
    *val = val_reference(val2);
}

int val_print(Obj *v1, FILE *f) {
    bool oldreferenceit = referenceit;
    Obj *err;
    struct linepos_s nopoint = {0, 0};
    int len;
    referenceit = false;
    err = v1->obj->repr(v1, &nopoint, SIZE_MAX);
    if (err != NULL) {
        if (err->obj == STR_OBJ) len = printable_print2(((Str *)err)->data, f, ((Str *)err)->len);
        else len = printable_print2((uint8_t *)err->obj->name, f, strlen(err->obj->name));
        val_destroy(err);
    } else len = 0;
    referenceit = oldreferenceit;
    return len;
}

void init_values(void)
{
}

void destroy_values(void)
{
    size_t j;
    garbage_collect();
    objects_destroy();

#ifdef DEBUG
    {
    Slotcoll *vals;
    size_t i;
    for (j = 0; j < lenof(slotcoll); j++) {
        size_t size = j * ALIGN;
        for (vals = slotcoll[j]; vals; vals = vals->next) {
            Obj *val = (Obj *)(vals + 1);
            for (i = 0; i < SLOTS; i++, val = ((void *)val) + size) {
                if (val->obj != NONE_OBJ) {
                    val_print(val, stderr);
                    fprintf(stderr, " %s %" PRIuSIZE " %" PRIxPTR "\n", val->obj->name, val->refcount, (uintptr_t)val);
                }
            }
        }
    }
    }
#endif

    for (j = 0; j < lenof(slotcoll); j++) {
        Slotcoll *s = slotcoll[j];
        while (s != NULL) {
            Slotcoll *old = s;
            s = s->next;
            free(old);
        }
    }
}
