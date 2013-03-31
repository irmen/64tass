/*

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
#ifndef _ERROR_H_
#define _ERROR_H_
#include "inttypes.h"

struct value_s;
enum oper_e;

struct error_s {
    size_t p;
    size_t len;
    uint8_t *data;
};

extern unsigned int errors, conderrors, warnings;

/* ---------------------------------------------------------------------------
   $00-$3f warning
   $40-$7f error
   $80-$bf fatal
*/

enum errors_e {
    ERROR_TOP_OF_MEMORY=0x00,
    ERROR_A_USED_AS_LBL,
    ERROR___BANK_BORDER,
    ERROR______JUMP_BUG,
    ERROR___LONG_BRANCH,
    ERROR_DIRECTIVE_IGN,
    ERROR_LABEL_NOT_LEF,
    ERROR_WUSER_DEFINED,

    ERROR_DOUBLE_DEFINE=0x40,
    ERROR_EXTRA_CHAR_OL,
    ERROR_CONSTNT_LARGE,
    ERROR_GENERL_SYNTAX,
    ERROR______EXPECTED,
    ERROR_EXPRES_SYNTAX,
    ERROR_MISSING_ARGUM,
    ERROR_ILLEGAL_OPERA,
    ERROR_DIVISION_BY_Z,
    ERROR____WRONG_TYPE,
    ERROR___NOT_ALLOWED,
    ERROR_CANT_CROSS_BA,
    ERROR_OUTOF_SECTION,
    ERROR_NEGFRAC_POWER,
    ERROR_BIG_STRING_CO,
    ERROR___INDEX_RANGE,
    ERROR__USER_DEFINED,
    ERROR____PAGE_ERROR,
    ERROR__BRANCH_CROSS,
    ERROR_BRANCH_TOOFAR,
    ERROR___UNKNOWN_CHR,

    ERROR___NOT_DEFINED,
    ERROR_REQUIREMENTS_,
    ERROR______CONFLICT,

    ERROR_CANT_FINDFILE=0x80,
    ERROR__READING_FILE,
    ERROR_CANT_WRTE_OBJ,
    ERROR_LINE_TOO_LONG,
    ERROR_CANT_DUMP_LST,
    ERROR_CANT_DUMP_LBL,
    ERROR_FILERECURSION,
    ERROR__MACRECURSION,
    ERROR__REFRECURSION,
    ERROR___UNKNOWN_CPU,
    ERROR_UNKNOWN_OPTIO,
    ERROR_TOO_MANY_PASS,
    ERROR__TOO_MANY_ERR
};

extern void err_msg(enum errors_e, const void*);
extern void err_msg2(enum errors_e, const void*, linepos_t);
extern void err_msg_wrong_type(const struct value_s *, linepos_t);
extern void err_msg_cant_calculate(const str_t *name, linepos_t epoint);
extern void err_msg_invalid_oper(enum oper_e, const struct value_s *, const struct value_s *, linepos_t);
extern void err_msg_strange_oper(enum oper_e, const struct value_s *, const struct value_s *, linepos_t);
extern void err_msg_double_defined(const str_t *, const char *, line_t, linepos_t, const str_t *, linepos_t);
extern void err_msg_shadow_defined(const str_t *, const char *, line_t, linepos_t, const str_t *, linepos_t);
extern void err_msg_not_defined(const str_t *, linepos_t);
extern void err_msg_requires(const str_t *name, linepos_t epoint);
extern void err_msg_conflicts(const str_t *name, linepos_t epoint);
extern void err_msg_variable(struct error_s *, struct value_s *, int);
extern void err_msg_file(enum errors_e, const char*);
extern void freeerrorlist(int);
extern void enterfile(const char*, line_t);
extern void exitfile(void);
extern void err_destroy(void);
extern void err_msg_out_of_memory(void);
extern void error_destroy(struct error_s *);
extern void error_init(struct error_s *);

#endif
