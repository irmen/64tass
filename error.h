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
#ifndef ERROR_H
#define ERROR_H
#include "stdbool.h"
#include "inttypes.h"
#include "errors_e.h"
#include "avl.h"
#include "obj.h"

struct file_s;

struct file_list_s {
    struct linepos_s epoint;
    struct file_s *file;
    struct avltree_node node;
    struct file_list_s *parent;
    struct avltree members;
};

extern struct Type *ERROR_OBJ;

#if _POSIX_C_SOURCE >= 1 || _XOPEN_SOURCE || _POSIX_SOURCE || _POSIX_VERSION || _POSIX2_VERSION
#define COLOR_OUTPUT
extern bool print_use_color;
#else
#define print_use_color false
#endif

struct Namespace;
struct Register;
struct Oper;

typedef struct Error {
    Obj v;
    enum errors_e num;
    struct linepos_s epoint;
    union {
        struct {
            struct Oper *op;
            Obj *v1;
            Obj *v2;
        } invoper;
        struct {
            str_t ident;
            struct Namespace *names;
            bool down;
        } notdef;
        struct {
            unsigned int bits;
            Obj *val;
        } intconv;
        const char *objname;
        uint32_t addressing;
        struct Register *reg;
        size_t opers;
        struct {
            size_t v1;
            size_t v2;
        } broadcast;
        Obj *key;
    } u;
} Error;

extern void errorobj_init(void);

extern MUST_CHECK Error *new_error(enum errors_e, linepos_t);

struct Label;

extern void err_msg(enum errors_e, const void *);
extern void err_msg2(enum errors_e, const void *, linepos_t);
extern void err_msg_wrong_type(const Obj *, struct Type *, linepos_t);
extern void err_msg_cant_calculate(const str_t *, linepos_t);
extern void err_msg_still_none(const str_t *, linepos_t);
extern void err_msg_invalid_oper(const struct Oper *, const Obj *, const Obj *, linepos_t);
extern void err_msg_double_definedo(struct file_list_s *, linepos_t, const str_t *, linepos_t);
extern void err_msg_double_defined(struct Label *, const str_t *, linepos_t);
extern void err_msg_shadow_defined(struct Label *, struct Label *);
extern void err_msg_shadow_defined2(struct Label *);
extern void err_msg_not_defined(const str_t *, linepos_t);
extern void err_msg_not_definedx(const str_t *, linepos_t);
extern void err_msg_file(enum errors_e, const char *, linepos_t);
extern void err_msg_output(const Error *);
extern void err_msg_output_and_destroy(Error *);
extern void err_msg_argnum(unsigned int, unsigned int, unsigned int, linepos_t);
extern void err_msg_bool(enum errors_e, Obj *, linepos_t);
extern void err_msg_bool_oper(struct oper_s *);
extern void err_msg_bool_val(enum errors_e, unsigned int, Obj *, linepos_t);
extern void err_msg_implied_reg(linepos_t);
extern void err_msg_jmp_bug(linepos_t);
extern void err_msg_pc_wrap(void);
extern void err_msg_mem_wrap(void);
extern void err_msg_label_left(linepos_t);
extern void err_msg_branch_page(int, linepos_t);
extern void err_msg_deprecated(enum errors_e, linepos_t);
extern void error_reset(void);
extern bool error_print(void);
extern struct file_list_s *enterfile(struct file_s *, linepos_t);
extern void exitfile(void);
extern void err_init(const char *);
extern void err_destroy(void);
extern void NO_RETURN err_msg_out_of_memory2(void);
extern void NO_RETURN err_msg_out_of_memory(void);
extern void error_status(void);
extern bool error_serious(void);
extern linecpos_t interstring_position(linepos_t, const uint8_t *, size_t);

static inline void *mallocx(size_t l) {
    void *m = malloc(l);
    if (m == NULL) err_msg_out_of_memory();
    return m;
}

static inline void *reallocx(void *o, size_t l) {
    void *m = realloc(o, l);
    if (m == NULL) err_msg_out_of_memory();
    return m;
}

#endif
