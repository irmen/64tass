/*
    $Id: argvalues.c 3107 2023-09-05 20:46:55Z soci $

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

#include "argvalues.h"
#include "arguments.h"
#include "64tass.h"
#include "eval.h"
#include "error.h"
#include "values.h"
#include "instruction.h"

#include "namespaceobj.h"

static struct values_s *get_argument(const struct argpos_s *argpos) {
    struct linepos_s epoint;
    pline = arguments.commandline.data + argpos->start;
    lpoint.pos = argpos->pos; lpoint.line = argpos->line; vline++;
    epoint = lpoint;
    if (!get_exp(0, 1, 1, &epoint)) return NULL;
    return get_val();
}

void update_argvalues(void) {
    size_t j;
    for (j = 0; j < arguments.symbol_output_len; j++) {
        struct symbol_output_s *output = &arguments.symbol_output[j];
        Namespace *space = NULL;

        if (output->space_pos.pos != 0) {
            struct values_s *vs = get_argument(&output->space_pos);
            if (vs != NULL) {
                space = get_namespace(vs->val);
                if (space == NULL) err_msg_invalid_namespace_conv(vs);
            }
        }
        if (space != output->space) {
            if (output->space != NULL) val_destroy(Obj(output->space));
            output->space = space == NULL ? NULL : ref_namespace(space);
        }
    }
    for (j = 0; j < arguments.output_len; j++) {
        struct output_s *output = &arguments.output[j];
        struct values_s *vs;
        uval_t uval;
        if (output->exec_pos.pos == 0) continue;
        vs = get_argument(&output->exec_pos);
        if (touval(vs->val, &uval, 8 * sizeof uval, &vs->epoint)) continue;
        output->exec = uval;
    }
}

void destroy_argvalues(void) {
    size_t i;
    for (i = 0; i < arguments.symbol_output_len; i++) {
        if (arguments.symbol_output[i].space == NULL) continue;
        val_destroy(Obj(arguments.symbol_output[i].space));
    }
}
