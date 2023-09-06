/*
    $Id: error.h 3112 2023-09-06 06:34:22Z soci $

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
extern struct file_list_s *commandline_file_list;
extern const struct file_list_s *dummy_file_list;

struct Obj;
struct Str;
struct Type;
struct Label;
struct Symbol;
struct Error;
struct Namespace;
struct Oper;
struct oper_s;
struct str_t;
struct error_output_s;
struct values_s;
struct argpos_s;

extern void err_msg(Error_types, const void *);
extern void err_msg2(Error_types, const void *, linepos_t);
extern void err_msg_wrong_type2(const struct Obj *, struct Type *, linepos_t);
extern void err_msg_invalid_namespace_conv(const struct values_s *);
extern void err_msg_cant_unpack(size_t, size_t, linepos_t);
extern void err_msg_cant_calculate(const struct str_t *, linepos_t);
extern void err_msg_cant_calculate2(const struct str_t *, const struct file_list_s *, linepos_t);
extern void err_msg_still_none(const struct str_t *, linepos_t);
extern void err_msg_still_align(linepos_t);
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
extern void err_msg_unknown_formatchar(const struct Str *, size_t, linepos_t);
extern void err_msg_not_defined2(const struct str_t *, struct Namespace *, bool, linepos_t);
extern void err_msg_not_defined2a(ssize_t, struct Namespace *, bool, linepos_t);
extern void err_msg_symbol_case(const struct str_t *, const struct Label *, linepos_t);
extern void err_msg_symbol_case2(const struct Symbol *, const struct Symbol *);
extern void err_msg_macro_prefix(linepos_t);
extern void err_msg_address_mismatch(unsigned int, unsigned int, linepos_t);
extern void err_msg_file(Error_types, const char *, const struct file_list_s *, linepos_t);
extern void err_msg_file2(Error_types, const char *, const struct argpos_s *);
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
extern void err_msg_align(address_t, linepos_t);
extern void err_msg_alignblk(address_t, address_t, linepos_t);
extern void err_msg_page(address_t, address_t, uval_t, linepos_t);
extern void err_msg_page_cross(address_t, address_t, uval_t, linepos_t);
extern void err_msg_priority(const struct Oper *, linepos_t);
extern void err_msg_alias(uint32_t, uint32_t, linepos_t);
extern void err_msg_deprecated(Error_types, linepos_t);
extern void err_msg_unknown_char(unichar_t, linepos_t);
extern void err_msg_wrong_character(linepos_t);
extern void err_msg_encode_definition_note(const struct file_list_s *, linepos_t);
extern void err_msg_star_assign(linepos_t);
extern void err_msg_compound_note(linepos_t);
extern void err_msg_byte_note(linepos_t);
extern void err_msg_char_note(const char *, linepos_t);
extern void err_msg_immediate_note(linepos_t);
extern void err_msg_enc_large(uval_t, linepos_t);
extern void err_msg_big_address(linepos_t);
extern void error_reset(void);
extern void error_print(const struct error_output_s *);
extern const struct file_list_s *parent_file_list(const struct file_list_s *);
extern void enterfile(struct file_s *, linepos_t);
extern void exitfile(void);
extern void err_init(const char *);
extern void err_destroy(void);
extern void fatal_error(const char *);
extern void NO_RETURN err_msg_out_of_memory(void);
extern void err_msg_signal(void);
extern void error_status(void);
extern bool error_serious(void);
extern linecpos_t interstring_position(linepos_t, const uint8_t *, size_t);

#if __has_builtin(__builtin_mul_overflow)
static inline void *malloc_array(size_t size, size_t nmemb) {
    return __builtin_mul_overflow(size, nmemb, &size) ? NULL : malloc(size);
}
static inline void *realloc_array(void *old, size_t size, size_t nmemb) {
    return __builtin_mul_overflow(size, nmemb, &size) ? NULL : realloc(old, size);
}
#else
#define malloc_array(size, nmemb) ((size) != 1 && (nmemb) > SIZE_MAX / (size) ? NULL : malloc((size) * (nmemb)))
#define realloc_array(old, size, nmemb) ((size) != 1 && (nmemb) > SIZE_MAX / (size) ? NULL : realloc((old), (size) * (nmemb)))
#endif

#define allocate_instance(type) ((type *)malloc(sizeof(type)))
#define allocate_array(type, count) ((type *)malloc_array(sizeof(type), (count)))

#ifdef __cplusplus
#include <type_traits>
#define new_instance(old) if ((*old = (std::remove_pointer<decltype(old)>::type)malloc(sizeof(**old))) == NULL) err_msg_out_of_memory()
#define new_array(old, count) if ((*old = (std::remove_pointer<decltype(old)>::type)malloc_array(sizeof(**old), (count))) == NULL) err_msg_out_of_memory()
#define resize_array(old, count) if ((*old = (std::remove_pointer<decltype(old)>::type)realloc_array(*old, sizeof(**old), (count))) == NULL) err_msg_out_of_memory()
#define extend_array(old, len, count) if (inc_overflow(len, (count)) || (*old = (std::remove_pointer<decltype(old)>::type)realloc_array(*old, sizeof(**old), (*len))) == NULL) err_msg_out_of_memory()
#else
#define new_instance(old) if ((*old = malloc(sizeof(**old))) == NULL) err_msg_out_of_memory()
#define new_array(old, count) if ((*old = malloc_array(sizeof(**old), (count))) == NULL) err_msg_out_of_memory()
#define resize_array(old, count) if ((*old = realloc_array(*old, sizeof(**old), (count))) == NULL) err_msg_out_of_memory()
#define extend_array(old, len, count) if (inc_overflow(len, (count)) || (*old = realloc_array(*old, sizeof(**old), (*len))) == NULL) err_msg_out_of_memory()
#endif

#ifdef __cplusplus
#define reallocate_array(old, count) ((decltype(old))realloc_array(old, sizeof(*old), (count)))
#elif defined __GNUC__
#define reallocate_array(old, count) ((__typeof__(old))realloc_array(old, sizeof(*old), (count)))
#else
#define reallocate_array(old, count) (realloc_array(old, sizeof(*old), (count)))
#endif

#endif
