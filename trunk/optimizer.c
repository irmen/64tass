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

#include "optimizer.h"
#include "stdbool.h"
#include "error.h"
#include "section.h"
#include "opcodes.h"

#define UNKNOWN 65536

enum {
    F_A = 1,
    F_X = 2,
    F_Y = 4,
    F_Z = 8,
    F_B = 16,
    F_M = 32,
    F_C = 64,
    F_F = 128,
    F_CA = 256,
    F_CX = 512,
    F_CY = 1024,
    F_CZ = 2048
};

struct optimizer_s {
    bool branched;
    bool call;
    uint16_t lb;
    unsigned int pc;
    uint8_t ar, xr, yr, zr, sr, srh, br;
    uint8_t av, xv, yv, zv, sv, svh, bv;
    uint8_t cmp;
    struct {
        unsigned int n, v, e, d, i, z, c;
    } p, ps;
};

static const struct cpu_s *cputype;
static bool cputype_65c02, cputype_65ce02;

void cpu_opt_set_cpumode(const struct cpu_s *cpu) {
    cputype = cpu;
    cputype_65c02 = (cpu == &c65c02 || cpu == &r65c02 || cpu == &w65c02);
    cputype_65ce02 = (cpu == &c65ce02 || cpu == &c4510);
}

void cpu_opt_long_branch(uint16_t cod) {
    struct optimizer_s *cpu = current_section->optimizer;
    if (cpu == NULL) {
        cpu_opt_invalidate();
        cpu = current_section->optimizer;
    }
    cpu->lb = cod;
}

void cpu_opt(uint8_t cod, uint32_t adr, int8_t ln, linepos_t epoint) {
    struct optimizer_s old, *cpu = current_section->optimizer;
    const char *optname;
    uint8_t a1;

    if (cpu == NULL) {
        cpu_opt_invalidate();
        cpu = current_section->optimizer;
    }

    if (cpu->branched || cpu->pc != current_section->l_address.address) {
        cpu_opt_invalidate();
        cpu->pc = current_section->l_address.address;
    }
    cpu->pc = (cpu->pc + ln + 1) & 0xffff;

    if (cpu->call) {
        if (cod == 0x60) err_msg2(ERROR_____TAIL_CALL, NULL, epoint);
        cpu->call = false;
    }

    if (cpu->lb != 0) {
        cod = cpu->lb & 0xff;
    }

    switch (cod) {
    case 0x71: /* ADC ($12),y */
    case 0xF1: /* SBC ($12),y */
        if (cputype_65c02 && cpu->yv == 0xff && cpu->yr == 0) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        goto adcsbc;
    case 0x79: /* ADC $1234,y */
    case 0xF9: /* SBC $1234,y */
        if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        goto adcsbc;
    case 0x61: /* ADC ($12,x) */
    case 0xE1: /* SBC ($12,x) */
        if (!cputype_65c02) goto adcsbc;
        /* fall through */
    case 0x7D: /* ADC $1234,x */
    case 0x75: /* ADC $12,x */
    case 0xFD: /* SBC $1234,x */
    case 0xF5: /* SBC $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0x69: /* ADC #$12 */
    case 0x6D: /* ADC $1234 */
    case 0x65: /* ADC $12 */
    case 0xE9: /* SBC #$12 */
    case 0xED: /* SBC $1234 */
    case 0xE5: /* SBC $12 */
    adcsbc:
        cpu->av = 0;
        cpu->p.n = cpu->p.v = cpu->p.c = cpu->p.z = UNKNOWN;
        cpu->ps.n = cpu->ps.z = F_A;
        cpu->ps.v = cpu->ps.c = F_F;
        break;
    case 0x29: /* AND #$12 */
        old.ar = cpu->ar; old.av = cpu->av;
        cpu->ar &= adr;
        cpu->av |= ~adr;
        if ((uint8_t)(adr & ~old.av) == (uint8_t)~old.av && (cpu->ar & old.av) == (old.ar & old.av) && (cpu->ps.n & cpu->ps.z & F_A) != 0) goto remove;
        goto loada;
    case 0x49: /* EOR #$12 */
        if ((adr & 0xff) == 0 && (cpu->ps.n & cpu->ps.z & F_A) != 0) goto remove;
        old.ar = cpu->ar; old.av = cpu->av;
        cpu->ar ^= adr;
        goto loada;
    case 0x09: /* ORA #$12 */
        old.ar = cpu->ar; old.av = cpu->av;
        cpu->ar |= adr;
        cpu->av |= adr;
        if ((uint8_t)(~adr & ~old.av) == (uint8_t)~old.av && (cpu->ar & old.av) == (old.ar & old.av) && (cpu->ps.n & cpu->ps.z & F_A) != 0) goto remove;
        goto loada;
    case 0x31: /* AND ($12),y */
        if (cputype_65c02 && cpu->yv == 0xff && cpu->yr == 0) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        goto anda;
    case 0x39: /* AND $1234,y */
        if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        goto anda;
    case 0x21: /* AND ($12,x) */
        if (!cputype_65c02) goto anda;
        /* fall through */
    case 0x3D: /* AND $1234,x */
    case 0x35: /* AND $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0x2D: /* AND $1234 */
    case 0x25: /* AND $12 */
    anda:
        cpu->av &= ~cpu->ar;
    nzfl:
        cpu->p.n = ((cpu->av & 0x80) == 0) ? UNKNOWN : (cpu->ar >> 7);
        cpu->p.z = ((cpu->ar & cpu->av) == 0 && cpu->av != 0xff) ? UNKNOWN : (((cpu->ar & cpu->av) == 0) ? 1 : 0);
        cpu->ps.n = cpu->ps.z = F_A;
        break;
    case 0x11: /* ORA ($12),y */
        if (cputype_65c02 && cpu->yv == 0xff && cpu->yr == 0) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        goto ora;
    case 0x19: /* ORA $1234,y */
        if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        goto ora;
    case 0x01: /* ORA ($12,x) */
        if (!cputype_65c02) goto ora;
        /* fall through */
    case 0x1D: /* ORA $1234,x */
    case 0x15: /* ORA $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0x0D: /* ORA $1234 */
    case 0x05: /* ORA $12 */
    ora:
        cpu->av &= cpu->ar;
        goto nzfl;
    case 0x68: /* PLA */
        old.sr = cpu->sr;
        cpu->sr++;
        if (((cpu->sr ^ old.sr) & ~cpu->sv) != 0) cpu->sv = 0;
        goto eorlda;
    case 0x51: /* EOR ($12),y */
    case 0xB1: /* LDA ($12),y */
        if (cputype_65c02 && cpu->yv == 0xff && cpu->yr == 0) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        goto eorlda;
    case 0x59: /* EOR $1234,y */
    case 0xB9: /* LDA $1234,y */
        if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        goto eorlda;
    case 0x41: /* EOR ($12,x) */
    case 0xA1: /* LDA ($12,x) */
        if (!cputype_65c02) goto eorlda;
        /* fall through */
    case 0x5D: /* EOR $1234,x */
    case 0x55: /* EOR $12,x */
    case 0xBD: /* LDA $1234,x */
    case 0xB5: /* LDA $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0x4D: /* EOR $1234 */
    case 0x45: /* EOR $12 */
    case 0xAD: /* LDA $1234 */
    case 0xA5: /* LDA $12 */
    eorlda:
        cpu->av = 0;
        cpu->p.n = cpu->p.z = UNKNOWN;
        cpu->ps.n = cpu->ps.z = F_A;
        break;
    case 0x0A: /* ASL A */
        old.ar = cpu->ar; old.av = cpu->av; old.p.c = cpu->p.c;
        cpu->p.c = ((old.av & 0x80) == 0) ? UNKNOWN : (old.ar >> 7);
        cpu->ar = old.ar << 1;
        cpu->av = (old.av << 1) | 1;
    loadac:
        old.p.n = cpu->p.n; old.p.z = cpu->p.z;
        cpu->p.n = ((cpu->av & 0x80) == 0) ? UNKNOWN : (cpu->ar >> 7);
        cpu->p.z = ((cpu->ar & cpu->av) == 0 && cpu->av != 0xff) ? UNKNOWN : (((cpu->ar & cpu->av) == 0) ? 1 : 0);
        cpu->ps.n = cpu->ps.z = F_A; cpu->ps.c = F_F;
        if (cpu->av != 0xff) {
            break;
        }
        if (cpu->ar == old.ar && old.av == 0xff &&
            old.p.n != UNKNOWN && old.p.n == cpu->p.n &&
            old.p.c != UNKNOWN && old.p.c == cpu->p.c &&
            old.p.z != UNKNOWN && old.p.z == cpu->p.z) goto remove;
        break;
    case 0x4A: /* LSR A */
        old.ar = cpu->ar; old.av = cpu->av; old.p.c = cpu->p.c;
        cpu->p.c = ((old.av & 1) == 0) ? UNKNOWN : (old.ar & 1);
        cpu->ar = old.ar >> 1;
        cpu->av = (old.av >> 1) | 0x80;
        goto loadac;
    case 0x2A: /* ROL A */
        old.ar = cpu->ar; old.av = cpu->av; old.p.c = cpu->p.c;
        cpu->p.c = ((old.av & 0x80) == 0) ? UNKNOWN : (old.ar >> 7);
        cpu->ar = (old.ar << 1) | ((old.p.c == 1) ? 1 : 0);
        cpu->av = (old.av << 1) | ((old.p.c != UNKNOWN) ? 1 : 0);
        goto loadac;
    case 0x6A: /* ROR A */
        old.ar = cpu->ar; old.av = cpu->av; old.p.c = cpu->p.c;
        cpu->p.c = ((old.av & 1) == 0) ? UNKNOWN : (old.ar & 1);
        cpu->ar = (old.ar >> 1) | ((old.p.c == 1) ? 0x80 : 0);
        cpu->av = (old.av >> 1) | ((old.p.c != UNKNOWN) ? 0x80 : 0);
        goto loadac;
    case 0xC9: /* CMP #$12 */
        cpu->cmp = adr;
        if ((uint8_t)adr == 0 && (cpu->ps.n & cpu->ps.z & F_A) != 0) {
            cpu->ps.n = cpu->ps.z = cpu->ps.c = F_CA;
            if (cpu->p.c == 1) goto remove;
            cpu->p.c = 1; optname = "sec"; goto replace; /* 0x38 SEC */
        }
        old.ar = cpu->ar;
        old.av = cpu->av;
        cpu->ps.n = cpu->ps.z = cpu->ps.c = F_CA;
    comp:
        old.p.n = cpu->p.n; old.p.z = cpu->p.z; old.p.c = cpu->p.c;
        a1 = old.ar ^ adr;
        cpu->p.z = ((a1 & old.av) == 0 && old.av != 0xff) ? UNKNOWN : (((a1 & old.av) == 0) ? 1 : 0);
        switch ((uint8_t)adr) {
        case 0x00:
            cpu->p.n = ((old.av & 0x80) == 0) ? UNKNOWN : (old.ar >> 7);
            cpu->p.c = 1;
            break;
        case 0x80:
            cpu->p.n = ((old.av & 0x80) == 0) ? UNKNOWN : ((old.ar >> 7) ^ 1);
            cpu->p.c = ((old.av & 0x80) == 0) ? UNKNOWN : (old.ar >> 7);
            break;
        default:
            cpu->p.n = (old.av != 0xff) ? UNKNOWN : (((uint8_t)(old.ar - adr)) >> 7);
            cpu->p.c = (((uint8_t)(old.ar | ~old.av) >= (uint8_t)adr) != ((old.ar & old.av) >= (uint8_t)adr)) ? UNKNOWN : ((old.ar >= (uint8_t)adr) ? 1 : 0);
            break;
        }
        if (old.p.n != UNKNOWN && old.p.n == cpu->p.n &&
            old.p.c != UNKNOWN && old.p.c == cpu->p.c &&
            old.p.z != UNKNOWN && old.p.z == cpu->p.z) goto remove;
        break;
    case 0xE0: /* CPX #$12 */
        cpu->cmp = adr;
        if ((uint8_t)adr == 0 && (cpu->ps.n & cpu->ps.z & F_X) != 0) {
            cpu->ps.n = cpu->ps.z = cpu->ps.c = F_CX;
            if (cpu->p.c == 1) goto remove;
            cpu->p.c = 1; optname = "sec"; goto replace; /* 0x38 SEC */
        }
        old.ar = cpu->xr;
        old.av = cpu->xv;
        cpu->ps.n = cpu->ps.z = cpu->ps.c = F_CX;
        goto comp;
    case 0xC0: /* CPY #$12 */
        cpu->cmp = adr;
        if ((uint8_t)adr == 0 && (cpu->ps.n & cpu->ps.z & F_Y) != 0) {
            cpu->ps.n = cpu->ps.z = cpu->ps.c = F_CY;
            if (cpu->p.c == 1) goto remove;
            cpu->p.c = 1; optname = "sec"; goto replace; /* 0x38 SEC */
        }
        old.ar = cpu->yr;
        old.av = cpu->yv;
        cpu->ps.n = cpu->ps.z = cpu->ps.c = F_CY;
        goto comp;
    case 0xD1: /* CMP ($12),y */
        if (cputype_65c02 && cpu->yv == 0xff && cpu->yr == 0) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        goto cmp;
    case 0xD9: /* CMP $1234,y */
        if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        goto cmp;
    case 0xC1: /* CMP ($12,x) */
        if (!cputype_65c02) goto cmp;
        /* fall through */
    case 0xDD: /* CMP $1234,x */
    case 0xD5: /* CMP $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0xCD: /* CMP $1234 */
    case 0xC5: /* CMP $12 */
    cmp:
        cpu->p.n = cpu->p.z = UNKNOWN;
        cpu->p.c = ((cpu->ar & cpu->av) == 255) ? 1 : UNKNOWN;
        cpu->ps.n = cpu->ps.z = cpu->ps.c = F_C;
        break;
    case 0xEC: /* CPX $1234 */
    case 0xE4: /* CPX $12 */
        cpu->p.n = cpu->p.z = UNKNOWN;
        cpu->p.c = ((cpu->xr & cpu->xv) == 255) ? 1 : UNKNOWN;
        cpu->ps.n = cpu->ps.z = cpu->ps.c = F_C;
        break;
    case 0xCC: /* CPY $1234 */
    case 0xC4: /* CPY $12 */
        cpu->p.n = cpu->p.z = UNKNOWN;
        cpu->p.c = ((cpu->yr & cpu->yv) == 255) ? 1 : UNKNOWN;
        cpu->ps.n = cpu->ps.z = cpu->ps.c = F_C;
        break;
    case 0x1E: /* ASL $1234,x */
    case 0x16: /* ASL $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0x0E: /* ASL $1234 */
    case 0x06: /* ASL $12 */
        cpu->p.n = cpu->p.z = cpu->p.c = UNKNOWN;
        cpu->ps.n = cpu->ps.z = F_M; cpu->ps.c = F_F;
        break;
    case 0x3E: /* ROL $1234,x */
    case 0x36: /* ROL $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0x2E: /* ROL $1234 */
    case 0x26: /* ROL $12 */
        cpu->p.n = UNKNOWN;
        cpu->p.z = (cpu->p.c == 1) ? 0 : UNKNOWN;
        cpu->p.c = UNKNOWN;
        cpu->ps.n = cpu->ps.z = F_M; cpu->ps.c = F_F;
        break;
    case 0x5E: /* LSR $1234,x */
    case 0x56: /* LSR $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0x4E: /* LSR $1234 */
    case 0x46: /* LSR $12 */
        cpu->p.n = 0;
        cpu->p.z = cpu->p.c = UNKNOWN;
        cpu->ps.n = cpu->ps.z = F_M; cpu->ps.c = F_F;
        break;
    case 0x7E: /* ROR $1234,x */
    case 0x76: /* ROR $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0x6E: /* ROR $1234 */
    case 0x66: /* ROR $12 */
        cpu->p.n = cpu->p.c;
        cpu->p.z = (cpu->p.c == 1) ? 0 : UNKNOWN;
        cpu->p.c = UNKNOWN;
        cpu->ps.n = cpu->ps.z = F_M; cpu->ps.c = F_F;
        break;
    case 0x10: /* BPL *+$12 */
    bpl:
        if (cpu->p.n == 1) goto removecond;
        if (ln < 0) break;
        if (cpu->lb > 255) { cpu->branched = true; break; }
        if (adr == 0) goto jump;
        if (cpu->p.n == 0) cpu->branched = true;
        if ((cpu->ps.n & F_A) != 0) { if ((cpu->ps.z & F_A) != 0) cpu->p.z = 0; cpu->ar |= 0x80; cpu->av |= 0x80; }
        if ((cpu->ps.n & F_X) != 0) { if ((cpu->ps.z & F_X) != 0) cpu->p.z = 0; cpu->xr |= 0x80; cpu->xv |= 0x80; }
        if ((cpu->ps.n & F_Y) != 0) { if ((cpu->ps.z & F_Y) != 0) cpu->p.z = 0; cpu->yr |= 0x80; cpu->yv |= 0x80; }
        if ((cpu->ps.n & F_Z) != 0) { if ((cpu->ps.z & F_Z) != 0) cpu->p.z = 0; cpu->zr |= 0x80; cpu->zv |= 0x80; }
        if ((cpu->ps.n & F_B) != 0) { if ((cpu->ps.z & F_B) != 0) cpu->p.z = 0; cpu->br |= 0x80; cpu->bv |= 0x80; }
        if ((cpu->ps.n & F_M) != 0) { if ((cpu->ps.z & F_M) != 0) cpu->p.z = 0; }
        cpu->p.n = 1;
        break;
    case 0x30: /* BMI *+$12 */
    bmi:
        if (cpu->p.n == 0) goto removecond;
        if (ln < 0) break;
        if (cpu->lb > 255) { cpu->branched = true; break; }
        if (adr == 0) goto jump;
        if (cpu->p.n == 1) cpu->branched = true;
        if ((cpu->ps.n & F_A) != 0) { cpu->ar &= ~0x80; cpu->av |= 0x80; }
        if ((cpu->ps.n & F_X) != 0) { cpu->xr &= ~0x80; cpu->xv |= 0x80; }
        if ((cpu->ps.n & F_Y) != 0) { cpu->yr &= ~0x80; cpu->yv |= 0x80; }
        if ((cpu->ps.n & F_Z) != 0) { cpu->zr &= ~0x80; cpu->zv |= 0x80; }
        if ((cpu->ps.n & F_B) != 0) { cpu->br &= ~0x80; cpu->bv |= 0x80; }
        cpu->p.n = 0;
        break;
    case 0x50: /* BVC *+$12 */
    bvc:
        if (cpu->p.v == 1) goto removecond;
        if (ln < 0) break;
        if (cpu->lb > 255) { cpu->branched = true; break; }
        if (adr == 0) goto jump;
        if (cpu->p.v == 0) cpu->branched = true;
        cpu->p.v = 1;
        break;
    case 0x70: /* BVS *+$12 */
    bvs:
        if (cpu->p.v == 0) goto removecond;
        if (ln < 0) break;
        if (cpu->lb > 255) { cpu->branched = true; break; }
        if (adr == 0) goto jump;
        if (cpu->p.v == 1) cpu->branched = true;
        cpu->p.v = 0;
        break;
    case 0x90: /* BCC *+$12 */
    bcc:
        if (cpu->p.c == 1) goto removecond;
        if (ln < 0) break;
        if (cpu->lb > 255) { cpu->branched = true; break; }
        if (adr == 0) goto jump;
        if (cpu->p.c == 0) cpu->branched = true;
        if (cpu->cmp == 255) {
            old.ps.c = cpu->ps.c & cpu->ps.z & cpu->ps.n;
            if ((old.ps.c & F_CA) != 0) { cpu->p.z = 1; cpu->ar = 255; cpu->av = 0xff; }
            if ((old.ps.c & F_CX) != 0) { cpu->p.z = 1; cpu->xr = 255; cpu->av = 0xff; }
            if ((old.ps.c & F_CY) != 0) { cpu->p.z = 1; cpu->yr = 255; cpu->av = 0xff; }
            if ((old.ps.c & F_CZ) != 0) { cpu->p.z = 1; cpu->zr = 255; cpu->av = 0xff; }
        }
        cpu->p.c = 1;
        break;
    case 0xB0: /* BCS *+$12 */
    bcs:
        if (cpu->p.c == 0) goto removecond;
        if (ln < 0) break;
        if (cpu->lb > 255) { cpu->branched = true; break; }
        if (adr == 0) goto jump;
        if (cpu->p.c == 1) cpu->branched = true;
        old.ps.c = cpu->ps.c & cpu->ps.z;
        if ((old.ps.c & F_C)  != 0) cpu->p.z = 0;
        if ((old.ps.c & F_CA) != 0) { cpu->p.z = 0; if (cpu->cmp == 1) { cpu->ar = 0; cpu->av = 0xff; } }
        if ((old.ps.c & F_CX) != 0) { cpu->p.z = 0; if (cpu->cmp == 1) { cpu->xr = 0; cpu->av = 0xff; } }
        if ((old.ps.c & F_CY) != 0) { cpu->p.z = 0; if (cpu->cmp == 1) { cpu->yr = 0; cpu->av = 0xff; } }
        if ((old.ps.c & F_CZ) != 0) { cpu->p.z = 0; if (cpu->cmp == 1) { cpu->zr = 0; cpu->av = 0xff; } }
        cpu->p.c = 0;
        break;
    case 0xD0: /* BNE *+$12 */
    bne:
        if (cpu->p.z == 1) goto removecond;
        if (ln < 0) break;
        if (cpu->lb > 255) { cpu->branched = true; break; }
        if (adr == 0) goto jump;
        if (cpu->p.z == 0) cpu->branched = true;
        if ((cpu->ps.z & F_A)  != 0) { if ((cpu->ps.n & F_A)  != 0) cpu->p.n = 0; cpu->ar = 0; cpu->av = 0xff; }
        if ((cpu->ps.z & F_X)  != 0) { if ((cpu->ps.n & F_X)  != 0) cpu->p.n = 0; cpu->xr = 0; cpu->xv = 0xff; }
        if ((cpu->ps.z & F_Y)  != 0) { if ((cpu->ps.n & F_Y)  != 0) cpu->p.n = 0; cpu->yr = 0; cpu->yv = 0xff; }
        if ((cpu->ps.z & F_Z)  != 0) { if ((cpu->ps.n & F_Z)  != 0) cpu->p.n = 0; cpu->zr = 0; cpu->zv = 0xff; }
        if ((cpu->ps.z & F_B)  != 0) { if ((cpu->ps.n & F_B)  != 0) cpu->p.n = 0; cpu->br = 0; cpu->bv = 0xff; }
        if ((cpu->ps.z & F_M)  != 0) { if ((cpu->ps.n & F_M)  != 0) cpu->p.n = 0; }
        if ((cpu->ps.z & F_C)  != 0) { if ((cpu->ps.n & F_C)  != 0) cpu->p.n = 0; if ((cpu->ps.c & F_C) != 0) cpu->p.c = 1; }
        if ((cpu->ps.z & F_CA) != 0) { if ((cpu->ps.n & F_CA) != 0) cpu->p.n = 0; cpu->ar = cpu->cmp; cpu->av = 0xff; if ((cpu->ps.c & F_CA) != 0) cpu->p.c = 1; }
        if ((cpu->ps.z & F_CX) != 0) { if ((cpu->ps.n & F_CX) != 0) cpu->p.n = 0; cpu->xr = cpu->cmp; cpu->xv = 0xff; if ((cpu->ps.c & F_CX) != 0) cpu->p.c = 1; }
        if ((cpu->ps.z & F_CY) != 0) { if ((cpu->ps.n & F_CY) != 0) cpu->p.n = 0; cpu->yr = cpu->cmp; cpu->yv = 0xff; if ((cpu->ps.c & F_CY) != 0) cpu->p.c = 1; }
        if ((cpu->ps.z & F_CZ) != 0) { if ((cpu->ps.n & F_CZ) != 0) cpu->p.n = 0; cpu->zr = cpu->cmp; cpu->zv = 0xff; if ((cpu->ps.c & F_CZ) != 0) cpu->p.c = 1; }
        cpu->p.z = 1;
        break;
    case 0xF0: /* BEQ *+$12 */
    beq:
        if (cpu->p.z == 0) goto removecond;
        if (ln < 0) break;
        if (cpu->lb > 255) { cpu->branched = true; break; }
        if (adr == 0) goto jump;
        if (cpu->p.z == 1) cpu->branched = true;
        cpu->p.z = 0;
        break;
    case 0x2C: /* BIT $1234 */
    case 0x24: /* BIT $12 */
    bit:
        cpu->p.n = cpu->p.v = UNKNOWN;
        cpu->p.z = (cpu->ar == 0 && cpu->av == 0xff) ? 1 : UNKNOWN;
        cpu->ps.n = cpu->ps.v = cpu->ps.z = F_F;
        break;
    case 0x18: /* CLC */
        if (cpu->p.c == 0) goto removeclr;
        cpu->p.c = 0;
        cpu->ps.c = F_F;
        break;
    case 0x38: /* SEC */
        if (cpu->p.c == 1) goto removeset;
        cpu->p.c = 1;
        cpu->ps.c = F_F;
        break;
    case 0x58: /* CLI */
        if (cpu->p.i == 0) goto removeclr;
        cpu->p.i = 0;
        cpu->ps.i = F_F;
        break;
    case 0x78: /* SEI */
        if (cpu->p.i == 1) goto removeset;
        cpu->p.i = 1;
        cpu->ps.i = F_F;
        break;
    case 0xB8: /* CLV */
        if (cpu->p.v == 0) goto removeclr;
        cpu->p.v = 0;
        cpu->ps.v = F_F;
        break;
    case 0xF8: /* SED */
        if (cpu->p.d == 1) goto removeset;
        cpu->p.d = 1;
        cpu->ps.d = F_F;
        break;
    case 0xD8: /* CLD */
        if (cpu->p.d == 0) goto removeclr;
        cpu->p.d = 0;
        cpu->ps.d = F_F;
        break;
    case 0xDE: /* DEC $1234,x */
    case 0xD6: /* DEC $12,x */
    case 0xFE: /* INC $1234,x */
    case 0xF6: /* INC $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0xCE: /* DEC $1234 */
    case 0xC6: /* DEC $12 */
    case 0xEE: /* INC $1234 */
    case 0xE6: /* INC $12 */
    incdec:
        cpu->p.n = cpu->p.z = UNKNOWN;
        cpu->ps.n = cpu->ps.z = F_M;
        break;
    case 0xCA: /* DEX */
        old.xr = cpu->xr;
        cpu->xr--;
    decx:
        if (((cpu->xr ^ old.xr) & ~cpu->xv) != 0) cpu->xv = 0;
        cpu->p.n = ((cpu->xv & 0x80) == 0) ? UNKNOWN : (cpu->xr >> 7);
        cpu->p.z = ((cpu->xr & cpu->xv) == 0 && cpu->xv != 0xff) ? UNKNOWN : (((cpu->xr & cpu->xv) == 0) ? 1 : 0);
        cpu->ps.n = cpu->ps.z = F_X;
        break;
    case 0x88: /* DEY */
        old.yr = cpu->yr;
        cpu->yr--;
    decy:
        if (((cpu->yr ^ old.yr) & ~cpu->yv) != 0) cpu->yv = 0;
        cpu->p.n = ((cpu->yv & 0x80) == 0) ? UNKNOWN : (cpu->yr >> 7);
        cpu->p.z = ((cpu->yr & cpu->yv) == 0 && cpu->yv != 0xff) ? UNKNOWN : (((cpu->yr & cpu->yv) == 0) ? 1 : 0);
        cpu->ps.n = cpu->ps.z = F_Y;
        break;
    case 0xE8: /* INX */
        old.xr = cpu->xr;
        cpu->xr++;
        goto decx;
    case 0xC8: /* INY */
        old.yr = cpu->yr;
        cpu->yr++;
        goto decy;
    case 0xA9: /* LDA #$12 */
        old.ar = cpu->ar; old.av = cpu->av;
        cpu->ar = adr;
        cpu->av = 0xff;
    loada:
        old.p.n = cpu->p.n; old.p.z = cpu->p.z;
        cpu->p.n = ((cpu->av & 0x80) == 0) ? UNKNOWN : (cpu->ar >> 7);
        cpu->p.z = ((cpu->ar & cpu->av) == 0 && cpu->av != 0xff) ? UNKNOWN : (((cpu->ar & cpu->av) == 0) ? 1 : 0);
        cpu->ps.n = cpu->ps.z = F_A;
        if (cpu->av == 0xff) {
            if (cpu->ar == old.ar && old.av == 0xff &&
                    old.p.n != UNKNOWN && old.p.n == cpu->p.n &&
                    old.p.z != UNKNOWN && old.p.z == cpu->p.z) goto remove;
            if (cpu->ar == cpu->xr && cpu->xv == 0xff) {
                optname = "txa"; /* 0x8A TXA */
                goto replace;
            }
            if (cpu->ar == cpu->yr && cpu->yv == 0xff) {
                optname = "tya"; /* 0x98 TYA */
                goto replace;
            }
            if (cputype_65ce02) {
                if (cpu->ar == cpu->zr && cpu->zv == 0xff) {
                    optname = "tza"; /* 0x6B TZA */
                    goto replace;
                }
                if (cpu->ar == cpu->br && cpu->bv == 0xff) {
                    optname = "tba"; /* 0x7B TBA */
                    goto replace;
                }
            }
            if (old.av == 0xff) {
                if (cpu->p.c == ((old.ar & 0x80) >> 7)) {
                    if (cpu->ar == (uint8_t)(old.ar << 1)) {
                        optname = "asl a"; /* 0x0A ASL A */
                        goto replace;
                    }
                    if (cpu->ar == ((uint8_t)(old.ar << 1) | cpu->p.c)) {
                        optname = "rol a"; /* 0x2A ROL A */
                        goto replace;
                    }
                }
                if (cpu->p.c == (old.ar & 1)) {
                    if (cpu->ar == (old.ar >> 1)) {
                        optname = "lsr a"; /* 0x4A LSR A */
                        goto replace;
                    }
                    if (cpu->ar == ((old.ar >> 1) | (cpu->p.c << 7))) {
                        optname = "ror a"; /* 0x6A ROR A */
                        goto replace;
                    }
                }
            }
        }
        if ((cputype_65c02 || cputype_65ce02) && old.av == cpu->av) {
            if (cpu->ar == (uint8_t)(old.ar + 1) && (((uint8_t)(old.ar + 1) ^ old.ar) & ~old.av) == 0) {
                optname = "inc a"; /* 0x1A INC A */
                goto replace;
            }
            if (cpu->ar == (uint8_t)(old.ar - 1) && (((uint8_t)(old.ar - 1) ^ old.ar) & ~old.av) == 0) {
                optname = "dec a"; /* 0x3A DEC A */
                goto replace;
            }
            if (cputype_65ce02 && old.av == cpu->av) {
                if (cpu->ar == (uint8_t)(-old.ar) && old.av == 255) {
                    optname = "neg a"; /* 0x42 NEG A */
                    goto replace;
                }
            }
        }
        break;
    case 0xA2: /* LDX #$12 */
        old.xr = cpu->xr; old.xv = cpu->xv; old.p.n = cpu->p.n; old.p.z = cpu->p.z;
        cpu->xr = adr;
        cpu->xv = 0xff;
        cpu->p.n = ((cpu->xv & 0x80) == 0) ? UNKNOWN : (cpu->xr >> 7);
        cpu->p.z = ((cpu->xr & cpu->xv) == 0 && cpu->xv != 0xff) ? UNKNOWN : (((cpu->xr & cpu->xv) == 0) ? 1 : 0);
        cpu->ps.n = cpu->ps.z = F_X;
        if (cpu->xr == old.xr && old.xv == 0xff &&
            old.p.n != UNKNOWN && old.p.n == cpu->p.n &&
            old.p.z != UNKNOWN && old.p.z == cpu->p.z) goto remove;
        if (cpu->xr == cpu->ar && cpu->av == 0xff) {
            optname = "tax"; /* 0xAA TAX */
            goto replace;
        }
        if (cpu->xr == cpu->sr && cpu->sv == 0xff) {
            optname = "tsx"; /* 0xBA TSX */
            goto replace;
        }
        if (old.xv != 0xff) {
            break;
        }
        if (cpu->xr == ((old.xr + 1) & 0xff)) {
            optname = "inx"; /* 0xE8 INX */
            goto replace;
        }
        if (cpu->xr == ((old.xr - 1) & 0xff)) {
            optname = "dex"; /* 0xCA DEX */
            goto replace;
        }
        break;
    case 0xA0: /* LDY #$12 */
        old.yr = cpu->yr; old.yv = cpu->yv; old.p.n = cpu->p.n; old.p.z = cpu->p.z;
        cpu->yr = adr;
        cpu->yv = 0xff;
        cpu->p.n = ((cpu->yv & 0x80) == 0) ? UNKNOWN : (cpu->yr >> 7);
        cpu->p.z = ((cpu->yr & cpu->yv) == 0 && cpu->yv != 0xff) ? UNKNOWN : (((cpu->yr & cpu->yv) == 0) ? 1 : 0);
        cpu->ps.n = cpu->ps.z = F_Y;
        if (cpu->yr == old.yr && old.yv == 0xff &&
            old.p.n != UNKNOWN && old.p.n == cpu->p.n &&
            old.p.z != UNKNOWN && old.p.z == cpu->p.z) goto remove;
        if (cpu->yr == cpu->ar && cpu->av == 0xff) {
            optname = "tay"; /* 0xA8 TAY */
            goto replace;
        }
        if (cputype_65ce02) {
            if (cpu->yr == cpu->srh && cpu->svh == 0xff) {
                optname = "tsy"; /* 0x0B TSY */
                goto replace;
            }
        }
        if (old.yv != 0xff) {
            break;
        }
        if (cpu->yr == ((old.yr + 1) & 0xff)) {
            optname = "iny"; /* 0xC8 INY */
            goto replace;
        }
        if (cpu->yr == ((old.yr - 1) & 0xff)) {
            optname = "dey"; /* 0x88 DEY */
            goto replace;
        }
        break;
    case 0xBE: /* LDX $1234,y */
    case 0xB6: /* LDX $12,y */
        if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0xAE: /* LDX $1234 */
    case 0xA6: /* LDX $12 */
        cpu->xv = 0;
        cpu->p.n = cpu->p.z = UNKNOWN;
        cpu->ps.n = cpu->ps.z = F_X;
        break;
    case 0xBC: /* LDY $1234,x */
    case 0xB4: /* LDY $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        /* fall through */
    case 0xAC: /* LDY $1234 */
    case 0xA4: /* LDY $12 */
        cpu->yv = 0;
        cpu->p.n = cpu->p.z = UNKNOWN;
        cpu->ps.n = cpu->ps.z = F_Y;
        break;
    case 0xEA: /* NOP */
        break;
    case 0x91: /* STA ($12),y */
        if (cputype_65c02 && cpu->yv == 0xff && cpu->yr == 0) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        break;
    case 0x99: /* STA $1234,y */
    case 0x96: /* STX $12,y */
        if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        break;
    case 0x81: /* STA ($12,x) */
        if (!cputype_65c02) break;
        /* fall through */
    case 0x9D: /* STA $1234,x */
    case 0x95: /* STA $12,x */
    case 0x94: /* STY $12,x */
        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
        break;
    case 0x8D: /* STA $1234 */
    case 0x85: /* STA $12 */
    case 0x8E: /* STX $1234 */
    case 0x86: /* STX $12 */
    case 0x8C: /* STY $1234 */
    case 0x84: /* STY $12 */
        break;
    case 0x48: /* PHA */
    case 0x08: /* PHP */
    push:
        old.sr = cpu->sr;
        cpu->sr--;
        if (((cpu->sr ^ old.sr) & ~cpu->sv) != 0) cpu->sv = 0;
        break;
    case 0x9A: /* TXS */
        old.sr = cpu->sr; old.sv = cpu->sv;
        cpu->sr = cpu->xr; cpu->sv = cpu->xv;
        if (cpu->sr == old.sr && old.sv == 0xff && cpu->sv == 0xff) goto remove;
        break;
    case 0xBA: /* TSX */
        old.xr = cpu->xr; old.xv = cpu->xv;
        cpu->ps.n = cpu->ps.z = F_X;
        cpu->xr = cpu->sr;
        cpu->xv = cpu->sv;
    loadx:
        old.p.n = cpu->p.n; old.p.z = cpu->p.z;
        cpu->p.n = ((cpu->xv & 0x80) == 0) ? UNKNOWN : (cpu->xr >> 7);
        cpu->p.z = ((cpu->xr & cpu->xv) == 0 && cpu->xv != 0xff) ? UNKNOWN : (((cpu->xr & cpu->xv) == 0) ? 1 : 0);
        if (cpu->xv != 0xff) {
            break;
        }
        if (cpu->xr == old.xr && old.xv == 0xff &&
            old.p.n != UNKNOWN && old.p.n == cpu->p.n &&
            old.p.z != UNKNOWN && old.p.z == cpu->p.z) goto remove;
        break;
    case 0xAA: /* TAX */
        old.xr = cpu->xr; old.xv = cpu->xv;
        cpu->ps.n = cpu->ps.z = F_A | F_X;
        cpu->xr = cpu->ar;
        cpu->xv = cpu->av;
        goto loadx;
    case 0xA8: /* TAY */
        old.yr = cpu->yr; old.yv = cpu->yv;
        cpu->ps.n = cpu->ps.z = F_A | F_Y;
        cpu->yr = cpu->ar;
        cpu->yv = cpu->av;
    loady:
        old.p.n = cpu->p.n; old.p.z = cpu->p.z;
        cpu->p.n = ((cpu->yv & 0x80) == 0) ? UNKNOWN : (cpu->yr >> 7);
        cpu->p.z = ((cpu->yr & cpu->yv) == 0 && cpu->yv != 0xff) ? UNKNOWN : (((cpu->yr & cpu->yv) == 0) ? 1 : 0);
        if (cpu->yv != 0xff) {
            break;
        }
        if (cpu->yr == old.yr && old.yv == 0xff &&
            old.p.n != UNKNOWN && old.p.n == cpu->p.n &&
            old.p.z != UNKNOWN && old.p.z == cpu->p.z) goto remove;
        break;
    case 0x8A: /* TXA */
        old.ar = cpu->ar; old.av = cpu->av;
        cpu->ps.n = cpu->ps.z = F_X | F_A;
        cpu->ar = cpu->xr;
        cpu->av = cpu->xv;
    loada2:
        old.p.n = cpu->p.n; old.p.z = cpu->p.z;
        cpu->p.n = ((cpu->av & 0x80) == 0) ? UNKNOWN : (cpu->ar >> 7);
        cpu->p.z = ((cpu->ar & cpu->av) == 0 && cpu->av != 0xff) ? UNKNOWN : (((cpu->ar & cpu->av) == 0) ? 1 : 0);
        if (cpu->av != 0xff) {
            break;
        }
        if (cpu->ar == old.ar && old.av == 0xff &&
            old.p.n != UNKNOWN && old.p.n == cpu->p.n &&
            old.p.z != UNKNOWN && old.p.z == cpu->p.z) goto remove;
        break;
    case 0x98: /* TYA */
        old.ar = cpu->ar; old.av = cpu->av;
        cpu->ps.n = cpu->ps.z = F_Y | F_A;
        cpu->ar = cpu->yr;
        cpu->av = cpu->yv;
        goto loada2;
    case 0x28: /* PLP */
        old.sr = cpu->sr;
        cpu->sr++;
        if (((cpu->sr ^ old.sr) & ~cpu->sv) != 0) cpu->sv = 0;
        cpu->p.n = cpu->p.v = cpu->p.e = cpu->p.d = cpu->p.i = cpu->p.z = cpu->p.c = UNKNOWN;
        cpu->ps.n = cpu->ps.v = cpu->ps.e = cpu->ps.d = cpu->ps.i = cpu->ps.z = cpu->ps.c = F_F;
        break;
    case 0x4C: /* JMP $1234 */
        if (cpu->pc == (uint16_t)adr) goto jump;
        cpu->branched = true;
        if ((uint16_t)(cpu->pc-adr+0x7e) < 0x100) {
            if (cpu->p.z == 0) {optname = "gne";goto replace;} /* 0xD0 BNE *+$12 */
            if (cpu->p.z == 1) {optname = "geq";goto replace;} /* 0xF0 BEQ *+$12 */
            if (cpu->p.c == 0) {optname = "gcc";goto replace;} /* 0x90 BCC *+$12 */
            if (cpu->p.c == 1) {optname = "gcs";goto replace;} /* 0xB0 BCS *+$12 */
            if (cpu->p.n == 0) {optname = "gpl";goto replace;} /* 0x10 BPL *+$12 */
            if (cpu->p.n == 1) {optname = "gmi";goto replace;} /* 0x30 BMI *+$12 */
            if (cpu->p.v == 0) {optname = "gvc";goto replace;} /* 0x50 BVC *+$12 */
            if (cpu->p.v == 1) {optname = "gvs";goto replace;} /* 0x70 BVS *+$12 */
            if (cputype_65c02 || cputype_65ce02 || cputype == &c65dtv02) {optname = "gra";goto replace;}
        }
        break;
    case 0x6C: /* JMP ($1234) */
    case 0x40: /* RTI */
    case 0x60: /* RTS */
        cpu->branched = true;
        break;
    case 0x20: /* JSR $1234 */
        cpu->call = true;
        cpu->av = 0;
        cpu->xv = 0;
        cpu->yv = 0;
        cpu->zv = 0;
        cpu->sv = 0;
        cpu->svh = 0;
        cpu->bv = 0;
        cpu->p.n = cpu->p.v = cpu->p.e = cpu->p.d = cpu->p.i = cpu->p.z = cpu->p.c = UNKNOWN;
        cpu->ps.n = cpu->ps.v = cpu->ps.e = cpu->ps.d = cpu->ps.i = cpu->ps.z = cpu->ps.c = F_F;
        break;
    case 0x00: /* BRK #$12 */
        cpu_opt_invalidate();
        break;
    default:
        if (cputype == &c6502i || cputype == &c65dtv02) {
            switch (cod) {
            case 0xBF: /* LAX $1234,y */
            case 0xB7: /* LAX $12,y */
                if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                /* fall through */
            case 0xAB: /* LAX #$12 */
            case 0xAF: /* LAX $1234 */
            case 0xA7: /* LAX $12 */
            case 0xA3: /* LAX ($12,x) */
            case 0xB3: /* LAX ($12),y */
                cpu->av = cpu->xv = 0;
                cpu->p.n = cpu->p.z = UNKNOWN;
                cpu->ps.n = cpu->ps.z = F_A | F_X;
                break;
            case 0x8B: /* ANE #$12 */
                cpu->ar &= adr & cpu->xv;
                cpu->av = ~adr;
                cpu->p.n = cpu->p.z = UNKNOWN;
                cpu->ps.n = cpu->ps.z = F_A;
                break;
            case 0x6B: /* ARR #$12 */
            /*case 0xEB:*/ /* SBC #$12 */
                cpu->av = 0;
                cpu->p.n = cpu->p.v = cpu->p.c = cpu->p.z = UNKNOWN;
                cpu->ps.n = cpu->ps.z = F_A;
                break;
            case 0x4B: /* ASR #$12 */
                old.ar = cpu->ar; old.av = cpu->av; old.p.c = cpu->p.c;
                cpu->ar &= adr;
                cpu->av |= ~adr;
                cpu->p.c = ((old.av & 1) == 0) ? UNKNOWN : (old.ar & 1);
                cpu->ar = old.ar >> 1;
                cpu->av = (old.av >> 1) | 0x80;
                goto loadac;
            case 0xDB: /* DCP $1234,y */
                if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                goto cmp;
            case 0xDF: /* DCP $1234,x */
            case 0xD7: /* DCP $12,x */
                if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                /* fall through */
            case 0xCF: /* DCP $1234 */
            case 0xC7: /* DCP $12 */
            case 0xC3: /* DCP ($12,x) */
            case 0xD3: /* DCP ($12),y */
                goto cmp;
            case 0xFB: /* ISB $1234,y */
            case 0x7B: /* RRA $1234,y */
                if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                goto adcsbc;
            case 0xFF: /* ISB $1234,x */
            case 0xF7: /* ISB $12,x */
            case 0x7F: /* RRA $1234,x */
            case 0x77: /* RRA $12,x */
                if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                /* fall through */
            case 0xEF: /* ISB $1234 */
            case 0xE7: /* ISB $12 */
            case 0xE3: /* ISB ($12,x) */
            case 0xF3: /* ISB ($12),y */
            case 0x6F: /* RRA $1234 */
            case 0x67: /* RRA $12 */
            case 0x63: /* RRA ($12,x) */
            case 0x73: /* RRA ($12),y */
                goto adcsbc;
            case 0x3B: /* RLA $1234,y */
                if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                cpu->p.c = UNKNOWN;
                goto anda;
            case 0x3F: /* RLA $1234,x */
            case 0x37: /* RLA $12,x */
                if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                /* fall through */
            case 0x2F: /* RLA $1234 */
            case 0x27: /* RLA $12 */
            case 0x23: /* RLA ($12,x) */
            case 0x33: /* RLA ($12),y */
                cpu->p.c = UNKNOWN;
                goto anda;
            case 0x1B: /* SLO $1234,y */
                if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                cpu->p.c = UNKNOWN;
                goto ora;
            case 0x1F: /* SLO $1234,x */
            case 0x17: /* SLO $12,x */
                if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                /* fall through */
            case 0x0F: /* SLO $1234 */
            case 0x07: /* SLO $12 */
            case 0x03: /* SLO ($12,x) */
            case 0x13: /* SLO ($12),y */
                cpu->p.c = UNKNOWN;
                goto ora;
            case 0x5B: /* SRE $1234,y */
                if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                goto rla;
            case 0x5F: /* SRE $1234,x */
            case 0x57: /* SRE $12,x */
                if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                /* fall through */
            case 0x4F: /* SRE $1234 */
            case 0x47: /* SRE $12 */
            case 0x43: /* SRE ($12,x) */
            case 0x53: /* SRE ($12),y */
            rla:
                cpu->av = 0;
                cpu->p.n = cpu->p.c = cpu->p.z = UNKNOWN;
                cpu->ps.n = cpu->ps.z = F_A;
                cpu->ps.c = F_M;
                break;
            case 0x97: /* SAX $12,y */
                if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                /* fall through */
            case 0x8F: /* SAX $1234 */
            case 0x87: /* SAX $12 */
            case 0x83: /* SAX ($12,x) */
                break;
            default:
                if (cputype == &c6502i) {
                    switch (cod) {
                    case 0x0B: /* ANC #$12 */
                        /*case 0x2B:*/ /* ANC #$12 */
                        cpu->ar &= adr;
                        cpu->av |= ~adr;
                        cpu->p.n = ((cpu->av & 0x80) == 0) ? UNKNOWN : (cpu->ar >> 7);
                        cpu->p.z = ((cpu->ar & cpu->av) == 0 && cpu->av != 0xff) ? UNKNOWN : (((cpu->ar & cpu->av) == 0) ? 1 : 0);
                        cpu->p.c = ((cpu->av & 0x80) == 0) ? UNKNOWN : (cpu->ar >> 7);
                        cpu->ps.n = cpu->ps.z = F_A;
                        break;
                    case 0xBB: /* LDS $1234,y */
                        cpu->av = cpu->xv = cpu->sv = 0;
                        cpu->p.n = cpu->p.z = UNKNOWN;
                        cpu->ps.n = cpu->ps.z = F_A;
                        break;
                    case 0x1C: /* NOP $1234,x */
                    /*case 0x3C:*/ /* NOP $1234,x */
                    /*case 0x5C:*/ /* NOP $1234,x */
                    /*case 0x7C:*/ /* NOP $1234,x */
                    /*case 0xDC:*/ /* NOP $1234,x */
                    /*case 0xFC:*/ /* NOP $1234,x */
                    case 0x14: /* NOP $12,x */
                    /*case 0x34:*/ /* NOP $12,x */
                    /*case 0x54:*/ /* NOP $12,x */
                    /*case 0x74:*/ /* NOP $12,x */
                    /*case 0xD4:*/ /* NOP $12,x */
                    /*case 0xF4:*/ /* NOP $12,x */
                        if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                        /* fall through */
                    case 0x80: /* NOP #$12 */
                    /*case 0x82:*/ /* NOP #$12 */
                    /*case 0xC2:*/ /* NOP #$12 */
                    /*case 0xE2:*/ /* NOP #$12 */
                    /*case 0x89:*/ /* NOP #$12 */
                    case 0x0C: /* NOP $1234 */
                    case 0x04: /* NOP $12 */
                    /*case 0x1A:*/ /* NOP */
                    /*case 0x3A:*/ /* NOP */
                    /*case 0x5A:*/ /* NOP */
                    /*case 0x7A:*/ /* NOP */
                    /*case 0xDA:*/ /* NOP */
                    /*case 0xFA:*/ /* NOP */
                        break;
                    case 0xCB: /* SBX #$12 */
                        old.xr = cpu->xr; old.xv = cpu->xv; old.p.n = cpu->p.n; old.p.z = cpu->p.z; old.p.c = cpu->p.c;
                        cpu->xv = (cpu->xv & cpu->av) | (cpu->av & ~cpu->ar) | (cpu->xv & ~cpu->xr);
                        cpu->xr &= cpu->ar;

                        a1 = cpu->xr ^ adr;
                        cpu->ps.n = cpu->ps.z = F_X; cpu->ps.c = F_F;
                        cpu->p.z = ((a1 & cpu->xv) == 0 && cpu->xv != 0xff) ? UNKNOWN : (((a1 & cpu->xv) == 0) ? 1 : 0);
                        switch ((uint8_t)adr) {
                        case 0x00:
                            cpu->p.n = ((cpu->xv & 0x80) == 0) ? UNKNOWN : (cpu->xr >> 7);
                            cpu->p.c = 1;
                            break;
                        case 0x80:
                            cpu->p.n = ((cpu->xv & 0x80) == 0) ? UNKNOWN : ((cpu->xr >> 7) ^ 1);
                            cpu->p.c = ((cpu->xv & 0x80) == 0) ? UNKNOWN : (cpu->xr >> 7);
                            break;
                        default:
                            cpu->p.n = (cpu->xv != 0xff) ? UNKNOWN : (((uint8_t)(cpu->xr - adr)) >> 7);
                            cpu->p.c = (((uint8_t)(cpu->xr | ~cpu->xv) >= (uint8_t)adr) != ((cpu->xr & cpu->xv) >= (uint8_t)adr)) ? UNKNOWN : ((cpu->xr >= (uint8_t)adr) ? 1 : 0);
                            break;
                        }
                        cpu->xr -= adr;
                        if (adr > cpu->xv) cpu->xv = 0;
                        if (cpu->xv != 0xff) {
                            break;
                        }
                        if (cpu->xr == old.xr && old.xv == 0xff &&
                                old.p.n != UNKNOWN && old.p.n == cpu->p.n &&
                                old.p.c != UNKNOWN && old.p.c == cpu->p.c &&
                                old.p.z != UNKNOWN && old.p.z == cpu->p.z) goto remove;
                        break;
                    case 0x93: /* SHA $1234,x */
                    case 0x9F: /* SHA $1234,y */
                    case 0x9E: /* SHX $1234,y */
                    case 0x9C: /* SHY $1234,x */
                        break;
                    case 0x9B: /* SHS $1234,y */
                        cpu->sv = 0;
                        break;
                    default:
                        cpu_opt_invalidate();
                    }
                    break;
                }
                switch (cod) {
                case 0x12: /* BRA +$12 */
                    if ((uint8_t)adr == 0) goto jump;
                    cpu->branched = true;
                    break;
                case 0x32: /* SAC #$12 */
                case 0x42: /* SIR #$12 */
                    /* fall through */
                default:
                    cpu_opt_invalidate();
                }
                break;
            }
            break;
        }
        if (!cputype_65c02 && !cputype_65ce02) {
            cpu_opt_invalidate();
            break;
        }
        switch (cod) {
        case 0x72: /* ADC ($12) */ /* ADC ($12),z */
        case 0xF2: /* SBC ($12) */ /* SBC ($12),z */
            goto adcsbc;
        case 0x32: /* AND ($12) */ /* AND ($12),z */
            goto anda;
        case 0x80: /* BRA *+$12 */
            if ((uint8_t)adr == 0) goto jump;
            cpu->branched = true;
            break;
        case 0x89: /* BIT #$12 */
            cpu->p.z = ((cpu->ar & adr) == 0 && (cpu->av & adr) != adr) ? UNKNOWN : (((cpu->ar & adr) == 0) ? 1 : 0);
            break;
        case 0x3C: /* BIT $1234,x */
        case 0x34: /* BIT $12,x */
            if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
            goto bit;
        case 0xD2: /* CMP ($12) */ /* CMP ($12),z */
            goto cmp;
        case 0x3A: /* DEC A */
            old.ar = cpu->ar;
            cpu->ar--;
        deca:
            if (((cpu->ar ^ old.ar) & ~cpu->av) != 0) cpu->av = 0;
            cpu->p.n = ((cpu->av & 0x80) == 0) ? UNKNOWN : (cpu->ar >> 7);
            cpu->p.z = ((cpu->ar & cpu->av) == 0 && cpu->av != 0xff) ? UNKNOWN : (((cpu->ar & cpu->av) == 0) ? 1 : 0);
            cpu->ps.n = cpu->ps.z = F_A;
            break;
        case 0x1A: /* INC A */
            old.ar = cpu->ar;
            cpu->ar++;
            goto deca;
        case 0x52: /* EOR ($12) */ /* EOR ($12),z */
        case 0xB2: /* LDA ($12) */ /* LDA ($12),z */
            goto eorlda;
        case 0x7C: /* JMP ($1234,x) */
            if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
            cpu->branched = true;
            break;
        case 0x12: /* ORA ($12) */ /* ORA ($12),z */
            goto ora;
        case 0xDA: /* PHX */
        case 0x5A: /* PHY */
            goto push;
        case 0xFA: /* PLX */
            old.sr = cpu->sr;
            cpu->sr++;
            if (((cpu->sr ^ old.sr) & ~cpu->sv) != 0) cpu->sv = 0;
            cpu->xv = 0;
            cpu->p.n = cpu->p.z = UNKNOWN;
            cpu->ps.n = cpu->ps.z = F_X;
            break;
        case 0x7A: /* PLY */
            old.sr = cpu->sr;
            cpu->sr++;
            if (((cpu->sr ^ old.sr) & ~cpu->sv) != 0) cpu->sv = 0;
            cpu->yv = 0;
            cpu->p.n = cpu->p.z = UNKNOWN;
            cpu->ps.n = cpu->ps.z = F_Y;
            break;
        case 0x92: /* STA ($12) */ /* STA ($12),z */
            break;
        case 0x9E: /* STZ $1234,x */
        case 0x74: /* STZ $12,x */
            if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
            /* fall through */
        case 0x9C: /* STZ $1234 */
        case 0x64: /* STZ $12 */
            break;
        case 0x1C: /* TRB $1234 */
        case 0x14: /* TRB $12 */
        case 0x0C: /* TSB $1234 */
        case 0x04: /* TSB $12 */
            cpu->p.z = UNKNOWN;
            break;
        default:
            if (cputype == &c65c02) {
                cpu_opt_invalidate();
                break;
            }
            if ((cod & 0xF) == 0xF) { /* BBR & BBS */
                if ((adr & 0xff00) == 0) goto jump;
                break;
            }
            if ((cod & 0x7) == 0x7) { /* RMB & SMB */
                break;
            }
            if (cod == 0x54) { /* NOP $12,x */
                if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                break;
            }
            if (cod == 0x82 || cod == 0xDC || cod == 0x44) { /* NOP #$12 */ /* NOP $1234 */ /* NOP $12 */
                break;
            }
            if (cputype == &w65c02) {
                if (cod == 0xDB) { /* 0xDB STP */
                    cpu->branched = true;
                    break;
                }
                if (cod == 0xCB) { /* 0xCB WAI */
                    break;
                }
            }
            if (cputype_65ce02) {
                cpu_opt_invalidate();
                break;
            }
            switch (cod) {
            case 0x43: /* ASR A */
                old.ar = cpu->ar; old.av = cpu->av; old.p.c = cpu->p.c;
                cpu->p.c = ((old.av & 1) == 0) ? UNKNOWN : (old.ar & 1);
                cpu->ar = (old.ar >> 1) | (old.ar & 0x80);
                cpu->av = (old.av >> 1) | (old.av & 0x80);
                goto loadac;
            case 0x54: /* ASR $12,x */
                if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                /* fall through */
            case 0x44: /* ASR $12 */
            case 0xCB: /* ASW $1234 */
                cpu->p.n = cpu->p.z = cpu->p.c = UNKNOWN;
                cpu->ps.n = cpu->ps.z = F_M; cpu->ps.c = F_F;
                break;
            case 0x83: /* BRA *+$1234 */
                if ((uint16_t)adr == 0) goto jump;
                cpu->branched = true;
                break;
            case 0x93: goto bcc; /* BCC *+$1234 */
            case 0xB3: goto bcs; /* BCS *+$1234 */
            case 0xF3: goto beq; /* BEQ *+$1234 */
            case 0x33: goto bmi; /* BMI *+$1234 */
            case 0xD3: goto bne; /* BNE *+$1234 */
            case 0x13: goto bpl; /* BPL *+$1234 */
            case 0x53: goto bvc; /* BVC *+$1234 */
            case 0x73: goto bvs; /* BVS *+$1234 */
            case 0x02: /* CLE */
                       if (cpu->p.e == 0) goto removeclr;
                       cpu->p.e = 0;
                       break;
            case 0x03: /* SEE */
                       if (cpu->p.e == 1) goto removeclr;
                       cpu->p.e = 1;
                       break;
            case 0xC2: /* CPZ #$12 */
                       cpu->cmp = adr;
                       if ((uint8_t)adr == 0 && cpu->ps.n == F_Z && cpu->ps.z == F_Z) {
                           cpu->ps.n = cpu->ps.z = cpu->ps.c = F_CZ;
                           if (cpu->p.c == 1) goto remove;
                           cpu->p.c = 1; optname = "sec"; goto replace; /* 0x38 SEC */
                       }
                       old.ar = cpu->zr;
                       old.av = cpu->zv;
                       cpu->ps.n = cpu->ps.z = cpu->ps.c = F_CZ;
                       goto comp;
            case 0xD4: /* CPZ $12 */
            case 0xDC: /* CPZ $1234 */
                       cpu->p.n = cpu->p.z = UNKNOWN;
                       cpu->p.c = ((cpu->zr & cpu->zv) == 255) ? 1 : UNKNOWN;
                       cpu->ps.n = cpu->ps.z = cpu->ps.c = F_C;
                       break;
            case 0xC3: /* DEW $12 */
            case 0xE3: /* INW $12 */
                       goto incdec;
            case 0x3B: /* DEZ */
                       old.zr = cpu->zr;
                       cpu->zr--;
                   decz:
                       if (((cpu->zr ^ old.zr) & ~cpu->zv) != 0) cpu->zv = 0;
                       cpu->p.n = ((cpu->zv & 0x80) == 0) ? UNKNOWN : (cpu->zr >> 7);
                       cpu->p.z = ((cpu->zr & cpu->zv) == 0 && cpu->zv != 0xff) ? UNKNOWN : (((cpu->zr & cpu->zv) == 0) ? 1 : 0);
                       cpu->ps.n = cpu->ps.z = F_Z;
                       break;
            case 0x1B: /* INZ */
                       old.zr = cpu->zr;
                       cpu->zr++;
                       goto decz;
            case 0xE2: /* LDA ($12,s),y */
                       goto eorlda;
            case 0x23: /* JSR ($1234,x) */
                       if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                       /* fall through */
            case 0x63: /* BSR *+$1234 */
            case 0x22: /* JSR ($1234) */
                       cpu->call = true;
                       cpu->av = 0;
                       cpu->xv = 0;
                       cpu->yv = 0;
                       cpu->zv = 0;
                       cpu->sv = 0;
                       cpu->svh = 0;
                       cpu->bv = 0;
                       cpu->p.n = cpu->p.v = cpu->p.e = cpu->p.d = cpu->p.i = cpu->p.z = cpu->p.c = UNKNOWN;
                       cpu->ps.n = cpu->ps.v = cpu->ps.e = cpu->ps.d = cpu->ps.i = cpu->ps.z = cpu->ps.c = 0;
                       break;
            case 0xA3: /* LDZ #$12 */
                       old.zr = cpu->zr; old.zv = cpu->zv; old.p.n = cpu->p.n; old.p.z = cpu->p.z;
                       cpu->zr = adr;
                       cpu->zv = 0xff;
                       cpu->p.n = ((cpu->zv & 0x80) == 0) ? UNKNOWN : (cpu->zr >> 7);
                       cpu->p.z = ((cpu->zr & cpu->zv) == 0 && cpu->zv != 0xff) ? UNKNOWN : (((cpu->zr & cpu->zv) == 0) ? 1 : 0);
                       cpu->ps.n = cpu->ps.z = F_Z;
                       if (cpu->zr == old.zr && old.zv == 0xff &&
                               old.p.n != UNKNOWN && old.p.n == cpu->p.n &&
                               old.p.z != UNKNOWN && old.p.z == cpu->p.z) goto remove;
                       if (cpu->zr == cpu->ar && cpu->av == 0xff) {
                           optname = "taz"; /* 0x4B TAZ */
                           goto replace;
                       }
                       if (old.zv != 0xff) {
                           break;
                       }
                       if (cpu->zr == ((old.zr + 1) & 0xff)) {
                           optname = "inz"; /* 0x1B INZ */
                           goto replace;
                       }
                       if (cpu->zr == ((old.zr - 1) & 0xff)) {
                           optname = "dez"; /* 0x3B DEZ */
                           goto replace;
                       }
                       break;
            case 0xBB: /* LDZ $1234,x */
                       if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                       /* fall through */
            case 0xAB: /* LDZ $1234 */
                       cpu->zv = 0;
                       cpu->p.n = cpu->p.z = UNKNOWN;
                       cpu->ps.n = cpu->ps.z = F_Z;
                       break;
            case 0x42: /* NEG A */
                       cpu->ar = -cpu->ar;
                       if (cpu->av != 255) cpu->av = 0;
                       cpu->p.n = ((cpu->av & 0x80) == 0) ? UNKNOWN : (cpu->ar >> 7);
                       cpu->p.z = ((cpu->ar & cpu->av) == 0 && cpu->av != 0xff) ? UNKNOWN : (((cpu->ar & cpu->av) == 0) ? 1 : 0);
                       cpu->ps.n = cpu->ps.z = F_A;
                       break;
            case 0xF4: /* PHW #$12 */
            case 0xFC: /* PHW $1234 */
            case 0xDB: /* PHZ */
                       goto push;
            case 0xFB: /* PLZ */
                       old.sr = cpu->sr;
                       cpu->sr++;
                       if (((cpu->sr ^ old.sr) & ~cpu->sv) != 0) cpu->sv = 0;
                       cpu->zv = 0;
                       cpu->p.n = cpu->p.z = UNKNOWN;
                       cpu->ps.n = cpu->ps.z = F_Z;
                       break;
            case 0xEB: /* ROW $1234 */
                       cpu->p.n = UNKNOWN;
                       cpu->p.z = (cpu->p.c == 1) ? 0 : UNKNOWN;
                       cpu->p.c = UNKNOWN;
                       cpu->ps.n = cpu->ps.z = F_M; cpu->ps.c = F_F;
                       break;
            case 0x62: /* RTS #$12 */
                       cpu->branched = true;
                       break;
            case 0x82: /* STA ($12,s),y */
                       break;
            case 0x9B: /* STX $1234,y */
                       if (cpu->yv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                       break;
            case 0x8B: /* STY $1234,x */
                       if (cpu->xv == 0xff) err_msg2(ERROR___CONST_INDEX, NULL, epoint);
                       break;
            case 0x5B: /* TAB */
                       old.br = cpu->br; old.bv = cpu->bv;
                       cpu->br = cpu->ar; cpu->bv = cpu->av;
                       if (cpu->br == old.br && old.bv == 0xff && cpu->bv == 0xff) goto remove;
                       break;
            case 0x4B: /* TAZ */
                       old.zr = cpu->zr; old.zv = cpu->zv; old.p.n = cpu->p.n; old.p.z = cpu->p.z;
                       cpu->ps.n = cpu->ps.z = F_A | F_Z;
                       cpu->zr = cpu->ar;
                       cpu->zv = cpu->av;

                       cpu->p.n = ((cpu->zv & 0x80) == 0) ? UNKNOWN : (cpu->zr >> 7);
                       cpu->p.z = ((cpu->zr & cpu->zv) == 0 && cpu->zv != 0xff) ? UNKNOWN : (((cpu->zr & cpu->zv) == 0) ? 1 : 0);
                       if (cpu->zv != 0xff) {
                           break;
                       }
                       if (cpu->zr == old.zr && old.zv == 0xff &&
                               old.p.n != UNKNOWN && old.p.n == cpu->p.n &&
                               old.p.z != UNKNOWN && old.p.z == cpu->p.z) goto remove;
                       break;
            case 0x7B: /* TBA */
                       old.ar = cpu->ar; old.av = cpu->av;
                       cpu->ps.n = cpu->ps.z = F_B | F_A;
                       cpu->ar = cpu->br;
                       cpu->av = cpu->bv;
                       goto loada2;
            case 0x0B: /* TSY */
                       old.yr = cpu->yr; old.yv = cpu->yv;
                       cpu->ps.n = cpu->ps.z = F_Y;
                       cpu->yr = cpu->srh;
                       cpu->yv = cpu->svh;
                       goto loady;
            case 0x2B: /* TYS */
                       old.srh = cpu->srh; old.svh = cpu->svh;
                       cpu->srh = cpu->yr; cpu->svh = cpu->yv;
                       if (cpu->srh == old.srh && old.svh == 0xff && cpu->svh == 0xff) goto remove;
                       break;
            case 0x6B: /* TZA */
                       old.ar = cpu->ar; old.av = cpu->av;
                       cpu->ps.n = cpu->ps.z = F_Z | F_A;
                       cpu->ar = cpu->zr;
                       cpu->av = cpu->zv;
                       goto loada2;
            default:
                       if (cputype == &c4510 && cod == 0x5C) { /* MAP */
                           break;
                       }
                       cpu_opt_invalidate();
                       break;
            }
            break;
        }
        break;
    }
    return;
remove:
    err_msg2(ERROR_____REMOVABLE, "it does not change anything", epoint);
    return;
removecond:
    err_msg2(ERROR_____REMOVABLE, "the condition is never met", epoint);
    return;
removeset:
    err_msg2(ERROR_____REMOVABLE, "flag is already set", epoint);
    return;
removeclr:
    err_msg2(ERROR_____REMOVABLE, "flag is already clear", epoint);
    return;
jump:
    err_msg2(ERROR_____REMOVABLE, "target is the next instruction", epoint);
    return;
replace:
    err_msg2(ERROR___OPTIMIZABLE, optname, epoint);
}

void cpu_opt_invalidate(void) {
    struct optimizer_s *cpu = current_section->optimizer;

    if (cpu == NULL) {
        cpu = (struct optimizer_s *)mallocx(sizeof *cpu);
        current_section->optimizer = cpu;
    }
    cpu->branched = false;
    cpu->call = false;
    cpu->lb = 0;
    cpu->av = 0;
    cpu->xv = 0;
    cpu->yv = 0;
    cpu->zv = 0;
    cpu->sv = 0;
    cpu->svh = 0;
    cpu->bv = 0;
    cpu->pc = UNKNOWN;
    cpu->p.n = cpu->p.v = cpu->p.e = cpu->p.d = cpu->p.i = cpu->p.z = cpu->p.c = UNKNOWN;
    cpu->ps.n = cpu->ps.v = cpu->ps.e = cpu->ps.d = cpu->ps.i = cpu->ps.z = cpu->ps.c = 0;
}

void cpu_opt_destroy(struct optimizer_s *cpu) {
    free(cpu);
}
