/*
    $Id: error.h 2596 2021-04-18 18:52:11Z soci $

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
#include "attributes.h"
#include "stdbool.h"
#include "errors_e.h"
#include "inttypes.h"

struct file_s;

struct file_list_s {
    struct linepos_s epoint;
    struct file_s *file;
};

extern struct file_list_s *current_file_list;
extern const struct file_list_s *dummy_file_list;

struct Obj;
struct Str;
struct Type;
struct Label;
struct Symbol;
struct Error;
struct Namespace;
struct oper_s;
struct str_t;
struct error_output_s;

extern void err_msg(Error_types, const void *);
extern void err_msg2(Error_types, const void *, linepos_t);
extern void err_msg_wrong_type(const struct Obj *, struct Type *, linepos_t);
extern void err_msg_wrong_type2(const struct Obj *, struct Type *, linepos_t);
extern void err_msg_cant_unpack(size_t, size_t, linepos_t);
extern void err_msg_cant_calculate(const struct str_t *, linepos_t);
extern void err_msg_cant_calculate2(const struct str_t *, const struct file_list_s *, linepos_t);
extern void err_msg_still_none(const struct str_t *, linepos_t);
extern void err_msg_double_definedo(const struct file_list_s *, linepos_t, const struct str_t *, linepos_t);
extern void err_msg_not_variable(struct Label *, const struct str_t *, linepos_t);
extern void err_msg_double_defined(struct Label *, const struct str_t *, linepos_t);
extern void err_msg_shadow_defined(struct Label *, struct Label *);
extern void err_msg_shadow_defined2(struct Label *);
extern void err_msg_missing_argument(linepos_t, argcount_t);
extern void err_msg_unknown_argument(const struct str_t *, linepos_t);
extern void err_msg_unused_macro(struct Label *);
extern void err_msg_unused_label(struct Label *);
extern void err_msg_unused_const(struct Label *);
extern void err_msg_unused_variable(struct Label *);
extern void err_msg_not_defined(const struct str_t *, linepos_t);
extern unsigned int err_msg_unknown_formatchar(const struct Str *, size_t, linepos_t);
extern void err_msg_not_defined2(const struct str_t *, struct Namespace *, bool, linepos_t);
extern void err_msg_not_defined2a(ssize_t, struct Namespace *, bool, linepos_t);
extern void err_msg_symbol_case(const struct str_t *, const struct Label *, linepos_t);
extern void err_msg_symbol_case2(const struct Symbol *, const struct Symbol *);
extern void err_msg_macro_prefix(linepos_t);
extern void err_msg_address_mismatch(unsigned int, unsigned int, linepos_t);
extern void err_msg_file(Error_types, const char *, linepos_t);
extern void err_msg_output(const struct Error *);
extern void err_msg_output_and_destroy(struct Error *);
extern void err_msg_argnum(argcount_t, argcount_t, argcount_t, linepos_t);
extern void err_msg_bool(Error_types, struct Obj *, linepos_t);
extern void err_msg_bool_oper(struct oper_s *);
extern void err_msg_implied_reg(linepos_t, uint32_t);
extern void err_msg_size_larger(linepos_t);
extern void err_msg_jmp_bug(linepos_t);
extern void err_msg_pc_bank(linepos_t);
extern void err_msg_mem_wrap(linepos_t);
extern void err_msg_addr_wrap(linepos_t);
extern void err_msg_dpage_wrap(linepos_t);
extern void err_msg_bank0_wrap(linepos_t);
extern void err_msg_pbank_wrap(linepos_t);
extern void err_msg_label_left(linepos_t);
extern void err_msg_branch_page(int, linepos_t);
extern void err_msg_page(address_t, address_t, linepos_t);
extern void err_msg_alias(uint32_t, uint32_t, linepos_t);
extern void err_msg_deprecated(Error_types, linepos_t);
extern void err_msg_unknown_char(uchar_t, const struct str_t *, linepos_t);
extern void err_msg_star_assign(linepos_t);
extern void err_msg_compound_note(linepos_t);
extern void err_msg_byte_note(linepos_t);
extern void err_msg_char_note(const char *, linepos_t);
extern void err_msg_immediate_note(linepos_t);
extern void err_msg_big_address(linepos_t);
extern void error_reset(void);
extern void error_print(const struct error_output_s *);
extern const struct file_list_s *parent_file_list(const struct file_list_s *);
extern void enterfile(struct file_s *, linepos_t);
extern void exitfile(void);
extern void err_init(const char *);
extern void err_destroy(void);
extern void fatal_error(const char *);
extern void NO_RETURN err_msg_out_of_memory2(void);
extern void NO_RETURN err_msg_out_of_memory(void);
extern void err_msg_signal(void);
extern void error_status(void);
extern bool error_serious(void);
extern linecpos_t interstring_position(linepos_t, const uint8_t *, size_t);

static inline MALLOC void *mallocx(size_t l) {
    void *m = malloc(l);
    if (m == NULL) err_msg_out_of_memory();
    return m;
}

static inline MUST_CHECK void *reallocx(void *o, size_t l) {
    void *m = realloc(o, l);
    if (m == NULL) err_msg_out_of_memory();
    return m;
}

#endif
