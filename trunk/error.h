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
#ifndef _ERROR_H_
#define _ERROR_H_
#include "inttypes.h"
#include "libtree.h"
#include "obj.h"

struct file_s;

struct file_list_s {
    struct linepos_s epoint;
    struct file_s *file;
    struct avltree_node node;
    struct file_list_s *parent;
    struct avltree members;
};

/* ---------------------------------------------------------------------------
   $00-$3f warning
   $40-$7f error
   $80-$bf fatal
*/

enum errors_e {
    ERROR_TOP_OF_MEMORY=0x00,
    ERROR___BANK_BORDER,
    ERROR______JUMP_BUG,
    ERROR___LONG_BRANCH,
    ERROR_DIRECTIVE_IGN,
    ERROR_LABEL_NOT_LEF,
    ERROR_WUSER_DEFINED,

    ERROR__DOUBLE_RANGE=0x40,
    ERROR_DOUBLE_ESCAPE,
    ERROR_EXTRA_CHAR_OL,
    ERROR_CONSTNT_LARGE,
    ERROR_NUMERIC_OVERF,
    ERROR_ADDRESS_LARGE,
    ERROR_GENERL_SYNTAX,
    ERROR_EXPRES_SYNTAX,
    ERROR_LABEL_REQUIRE,
    ERROR_MISSING_ARGUM,
    ERROR_DIVISION_BY_Z,
    ERROR_NO_ZERO_VALUE,
    ERROR___NO_HIGH_BIT,
    ERROR__BYTES_NEEDED,
    ERROR___NO_LAST_GAP,
    ERROR_CANT_CROSS_BA,
    ERROR_OUTOF_SECTION,
    ERROR_NEGFRAC_POWER,
    ERROR_SQUARE_ROOT_N,
    ERROR_LOG_NON_POSIT,
    ERROR___MATH_DOMAIN,
    ERROR__EMPTY_STRING,
    ERROR_BIG_STRING_CO,
    ERROR____NO_FORWARD,
    ERROR_REQUIREMENTS_,
    ERROR______CONFLICT,
    ERROR___INDEX_RANGE,
    ERROR_____KEY_ERROR,
    ERROR__NOT_HASHABLE,
    ERROR__NOT_KEYVALUE,
    ERROR_____CANT_IVAL,
    ERROR_____CANT_UVAL,
    ERROR_CANT_BROADCAS,
    ERROR_____CANT_SIGN,
    ERROR______CANT_ABS,
    ERROR______CANT_INT,
    ERROR______CANT_LEN,
    ERROR_____CANT_SIZE,
    ERROR_____CANT_BOOL,
    ERROR______NOT_ITER,
    ERROR__NO_BYTE_ADDR,
    ERROR__NO_WORD_ADDR,
    ERROR__NO_LONG_ADDR,
    ERROR____NOT_DIRECT,
    ERROR__NOT_DATABANK,
    ERROR_____NOT_BANK0,
    ERROR__USER_DEFINED,
    ERROR_NO_ADDRESSING,
    ERROR___NO_REGISTER,
    ERROR___NO_LOT_OPER,
    ERROR____PAGE_ERROR,
    ERROR__BRANCH_CROSS,
    ERROR_BRANCH_TOOFAR,
    ERROR____PTEXT_LONG,
    ERROR___UNKNOWN_CHR,
    ERROR______EXPECTED,
    ERROR___NOT_ALLOWED,
    ERROR_RESERVED_LABL,
    ERROR___UNKNOWN_CPU,

    ERROR___NOT_DEFINED,
    ERROR__INVALID_OPER,
    ERROR____STILL_NONE,

    ERROR_CANT_FINDFILE=0x80,
    ERROR__READING_FILE,
    ERROR_CANT_WRTE_OBJ,
    ERROR_CANT_DUMP_LST,
    ERROR_CANT_DUMP_LBL,
    ERROR_FILERECURSION,
    ERROR__MACRECURSION,
    ERROR__FUNRECURSION,
    ERROR_TOO_MANY_PASS,
    ERROR_UNKNOWN_OPTIO
};

extern obj_t ERROR_OBJ;

typedef uint32_t atype_t;
typedef struct Namespace Namespace;
typedef struct Register Register;
typedef struct Oper Oper;

typedef struct Error {
    Obj v;
    enum errors_e num;
    struct linepos_s epoint;
    union {
        struct {
            Oper *op;
            Obj *v1;
            Obj *v2;
        } invoper;
        struct {
            str_t ident;
            Namespace *names;
            int down;
        } notdef;
        struct {
            int bits;
            Obj *val;
        } intconv;
        const char *objname;
        atype_t addressing;
        Register *reg;
        size_t opers;
        struct {
            size_t v1;
            size_t v2;
        } broadcast;
    } u;
} Error;

extern void errorobj_init(void);

extern MUST_CHECK Error *new_error(enum errors_e, linepos_t);

typedef struct Label Label;

extern void err_msg(enum errors_e, const void *);
extern void err_msg2(enum errors_e, const void *, linepos_t);
extern void err_msg_wrong_type(const Obj *, obj_t, linepos_t);
extern void err_msg_cant_calculate(const str_t *, linepos_t);
extern void err_msg_still_none(const str_t *, linepos_t);
extern void err_msg_invalid_oper(const Oper *, const Obj *, const Obj *, linepos_t);
extern void err_msg_double_defined(Label *, const str_t *, linepos_t);
extern void err_msg_shadow_defined(Label *, Label *);
extern void err_msg_not_defined(const str_t *, linepos_t);
extern void err_msg_not_definedx(const str_t *, linepos_t);
extern void err_msg_file(enum errors_e, const char *, linepos_t);
extern void err_msg_output(const Error *);
extern void err_msg_output_and_destroy(Error *);
extern void err_msg_argnum(unsigned int, unsigned int, unsigned int, linepos_t);
extern void error_reset(void);
extern int error_print(int, int, int);
extern struct file_list_s *enterfile(struct file_s *, linepos_t);
extern void exitfile(void);
extern void err_init(void);
extern void err_destroy(void);
extern void NO_RETURN err_msg_out_of_memory(void);
extern void error_status(void);
extern int error_serious(int, int);
extern linecpos_t interstring_position(linepos_t, const uint8_t *, size_t);

#endif
