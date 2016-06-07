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

#define UNKNOWN 65536
#define UNKNOWN_A 65537
#define UNKNOWN_X 65538
#define UNKNOWN_Y 65539

struct cpu_model_s {
    bool branched;
    bool call;
    unsigned int pc;
    uint8_t ar, xr, yr, sr;
    uint8_t av, xv, yv, sv;
    struct {
        unsigned int n, v, d, i, z, c;
    } p;
};

static struct cpu_model_s cpu;

const char *cpu_opt_branch_suggest(void) {
    if (cpu.p.z == 0) return "gne"; /* 0xD0 BNE *+$12 */
    if (cpu.p.z == 1) return "geq"; /* 0xF0 BEQ *+$12 */
    if (cpu.p.c == 0) return "gcc"; /* 0x90 BCC *+$12 */
    if (cpu.p.c == 1) return "gcs"; /* 0xB0 BCS *+$12 */
    if (cpu.p.n == 0) return "gpl"; /* 0x10 BPL *+$12 */
    if (cpu.p.n == 1) return "gmi"; /* 0x30 BMI *+$12 */
    if (cpu.p.v == 0) return "gvc"; /* 0x50 BVC *+$12 */
    if (cpu.p.v == 1) return "gvs"; /* 0x70 BVS *+$12 */
    return NULL;
}

void cpu_opt(uint8_t cod, uint32_t adr, int8_t ln, linepos_t epoint) {
    struct cpu_model_s old;
    const char *optname;
    uint8_t a1;

    if (cpu.branched || cpu.pc != current_section->l_address.address) {
        cpu_opt_invalidate();
        cpu.pc = current_section->l_address.address;
    }
    cpu.pc = (cpu.pc + ln + 1) & 0xffff;

    if (cpu.call) {
        if (cod == 0x60) err_msg2(ERROR_____TAIL_CALL, NULL, epoint);
        cpu_opt_invalidate();
    }

    switch (cod) {
    case 0x69: /* ADC #$12 */
    case 0x6D: /* ADC $1234 */
    case 0x65: /* ADC $12 */
    case 0x7D: /* ADC $1234,x */
    case 0x75: /* ADC $12,x */
    case 0x61: /* ADC ($12,x) */
    case 0x79: /* ADC $1234,y */
    case 0x71: /* ADC ($12),y */
    case 0xE9: /* SBC #$12 */
    case 0xED: /* SBC $1234 */
    case 0xE5: /* SBC $12 */
    case 0xFD: /* SBC $1234,x */
    case 0xF5: /* SBC $12,x */
    case 0xE1: /* SBC ($12,x) */
    case 0xF9: /* SBC $1234,y */
    case 0xF1: /* SBC ($12),y */
        cpu.av = 0;
        cpu.p.n = UNKNOWN_A;
        cpu.p.v = UNKNOWN;
        cpu.p.c = UNKNOWN;
        cpu.p.z = UNKNOWN_A;
        break;
    case 0x29: /* AND #$12 */
        if ((adr & 0xff) == 0xff && cpu.p.n == UNKNOWN_A && cpu.p.z == UNKNOWN_A) goto remove;
        old.ar = cpu.ar; old.av = cpu.av; old.p.n = cpu.p.n; old.p.z = cpu.p.z;
        cpu.ar &= adr;
        cpu.av |= ~adr;
        goto loada;
    case 0x49: /* EOR #$12 */
        if ((adr & 0xff) == 0 && cpu.p.n == UNKNOWN_A && cpu.p.z == UNKNOWN_A) goto remove;
        old.ar = cpu.ar; old.av = cpu.av; old.p.n = cpu.p.n; old.p.z = cpu.p.z;
        cpu.ar ^= adr;
        goto loada;
    case 0x09: /* ORA #$12 */
        if ((adr & 0xff) == 0 && cpu.p.n == UNKNOWN_A && cpu.p.z == UNKNOWN_A) goto remove;
        old.ar = cpu.ar; old.av = cpu.av; old.p.n = cpu.p.n; old.p.z = cpu.p.z;
        cpu.ar |= adr;
        cpu.av |= adr;
        goto loada;
    case 0x2D: /* AND $1234 */
    case 0x25: /* AND $12 */
    case 0x3D: /* AND $1234,x */
    case 0x35: /* AND $12,x */
    case 0x21: /* AND ($12,x) */
    case 0x39: /* AND $1234,y */
    case 0x31: /* AND ($12),y */
        cpu.av &= ~cpu.ar;
    nzfl:
        cpu.p.n = ((cpu.av & 0x80) == 0) ? UNKNOWN_A : (cpu.ar >> 7);
        cpu.p.z = ((cpu.ar & cpu.av) == 0 && cpu.av != 0xff) ? UNKNOWN_A : (((cpu.ar & cpu.av) == 0) ? 1 : 0);
        break;
    case 0x0D: /* ORA $1234 */
    case 0x05: /* ORA $12 */
    case 0x1D: /* ORA $1234,x */
    case 0x15: /* ORA $12,x */
    case 0x01: /* ORA ($12,x) */
    case 0x19: /* ORA $1234,y */
    case 0x11: /* ORA ($12),y */
        cpu.av &= cpu.ar;
        goto nzfl;
    case 0x68: /* PLA */
        old.sr = cpu.sr;
        cpu.sr++;
        if (((cpu.sr ^ old.sr) & ~cpu.sv) != 0) cpu.sv = 0;
        /* fall through */
    case 0x4D: /* EOR $1234 */
    case 0x45: /* EOR $12 */
    case 0x5D: /* EOR $1234,x */
    case 0x55: /* EOR $12,x */
    case 0x41: /* EOR ($12,x) */
    case 0x59: /* EOR $1234,y */
    case 0x51: /* EOR ($12),y */
    case 0xAD: /* LDA $1234 */
    case 0xA5: /* LDA $12 */
    case 0xBD: /* LDA $1234,x */
    case 0xB5: /* LDA $12,x */
    case 0xA1: /* LDA ($12,x) */
    case 0xB9: /* LDA $1234,y */
    case 0xB1: /* LDA ($12),y */
        cpu.av = 0;
        cpu.p.n = UNKNOWN_A;
        cpu.p.z = UNKNOWN_A;
        break;
    case 0x0A: /* ASL A */
        old.ar = cpu.ar; old.av = cpu.av; old.p.n = cpu.p.n; old.p.z = cpu.p.z; old.p.c = cpu.p.c;
        cpu.p.c = ((old.av & 0x80) == 0) ? UNKNOWN_A : (old.ar >> 7);
        cpu.ar = old.ar << 1;
        cpu.av = (old.av << 1) | 1;
    loadac:
        cpu.p.n = ((cpu.av & 0x80) == 0) ? UNKNOWN_A : (cpu.ar >> 7);
        cpu.p.z = ((cpu.ar & cpu.av) == 0 && cpu.av != 0xff) ? UNKNOWN_A : (((cpu.ar & cpu.av) == 0) ? 1 : 0);
        if (cpu.av != 0xff) {
            break;
        }
        if (cpu.ar == old.ar && old.av == 0xff &&
            old.p.n < UNKNOWN && old.p.n == cpu.p.n &&
            old.p.c < UNKNOWN && old.p.c == cpu.p.c &&
            old.p.z < UNKNOWN && old.p.z == cpu.p.z) goto remove;
        break;
    case 0x4A: /* LSR A */
        old.ar = cpu.ar; old.av = cpu.av; old.p.n = cpu.p.n; old.p.z = cpu.p.z; old.p.c = cpu.p.c;
        cpu.p.c = ((old.av & 1) == 0) ? UNKNOWN_A : (old.ar & 1);
        cpu.ar = old.ar >> 1;
        cpu.av = (old.av >> 1) | 0x80;
        goto loadac;
    case 0x2A: /* ROL A */
        old.ar = cpu.ar; old.av = cpu.av; old.p.n = cpu.p.n; old.p.z = cpu.p.z; old.p.c = cpu.p.c;
        cpu.p.c = ((old.av & 0x80) == 0) ? UNKNOWN_A : (old.ar >> 7);
        cpu.ar = (old.ar << 1) | ((old.p.c == 1) ? 1 : 0);
        cpu.av = (old.av << 1) | ((old.p.c < UNKNOWN) ? 1 : 0);
        goto loadac;
    case 0x6A: /* ROR A */
        old.ar = cpu.ar; old.av = cpu.av; old.p.n = cpu.p.n; old.p.z = cpu.p.z; old.p.c = cpu.p.c;
        cpu.p.c = ((old.av & 1) == 0) ? UNKNOWN_A : (old.ar & 1);
        cpu.ar = (old.ar >> 1) | ((old.p.c == 1) ? 0x80 : 0);
        cpu.av = (old.av >> 1) | ((old.p.c < UNKNOWN) ? 0x80 : 0);
        goto loadac;
    case 0xC9: /* CMP #$12 */
        old.ar = cpu.ar;
        old.av = cpu.av;
    comp:
        switch ((uint8_t)adr) {
        case 0x00: 
            cpu.p.n = ((old.av & 0x80) == 0) ? UNKNOWN : (old.ar >> 7);
            cpu.p.z = ((old.ar & old.av) == 0 && old.av != 0xff) ? UNKNOWN : (((old.ar & old.av) == 0) ? 1 : 0);
            cpu.p.c = 1; 
            break;
        case 0x80: 
            cpu.p.n = ((old.av & 0x80) == 0) ? UNKNOWN : ((old.ar >> 7) ^ 1);
            cpu.p.z = (((old.ar ^ 0x80) & old.av) == 0 && old.av != 0xff) ? UNKNOWN : ((((old.ar ^ 0x80) & old.av) == 0) ? 1 : 0);
            cpu.p.c = ((old.av & 0x80) == 0) ? UNKNOWN : (old.ar >> 7); 
            break;
        default: 
            a1 = old.ar - adr;
            cpu.p.n = (old.av != 0xff) ? UNKNOWN : (a1 >> 7);
            cpu.p.z = (old.av != 0xff) ? UNKNOWN : ((a1 == 0) ? 1 : 0);
            cpu.p.c = (old.av != 0xff) ? UNKNOWN : ((old.ar >= (uint8_t)adr) ? 1 : 0); 
            break;
        }
        break;
    case 0xE0: /* CPX #$12 */
        old.ar = cpu.xr;
        old.av = cpu.xv;
        goto comp;
    case 0xC0: /* CPY #$12 */
        old.ar = cpu.yr;
        old.av = cpu.yv;
        goto comp;
    case 0xCD: /* CMP $1234 */
    case 0xC5: /* CMP $12 */
    case 0xDD: /* CMP $1234,x */
    case 0xD5: /* CMP $12,x */
    case 0xC1: /* CMP ($12,x) */
    case 0xD9: /* CMP $1234,y */
    case 0xD1: /* CMP ($12),y */
        cpu.p.n = UNKNOWN;
        cpu.p.z = UNKNOWN;
        cpu.p.c = ((cpu.ar & cpu.av) == 255) ? 1 : UNKNOWN;
        break;
    case 0xEC: /* CPX $1234 */
    case 0xE4: /* CPX $12 */
        cpu.p.n = UNKNOWN;
        cpu.p.z = UNKNOWN;
        cpu.p.c = ((cpu.xr & cpu.xv) == 255) ? 1 : UNKNOWN;
        break;
    case 0xCC: /* CPY $1234 */
    case 0xC4: /* CPY $12 */
        cpu.p.n = UNKNOWN;
        cpu.p.z = UNKNOWN;
        cpu.p.c = ((cpu.yr & cpu.yv) == 255) ? 1 : UNKNOWN;
        break;
    case 0x0E: /* ASL $1234 */
    case 0x06: /* ASL $12 */
    case 0x1E: /* ASL $1234,x */
    case 0x16: /* ASL $12,x */
        cpu.p.n = UNKNOWN;
        cpu.p.z = UNKNOWN;
        cpu.p.c = UNKNOWN;
        break;
    case 0x2E: /* ROL $1234 */
    case 0x26: /* ROL $12 */
    case 0x3E: /* ROL $1234,x */
    case 0x36: /* ROL $12,x */
        cpu.p.n = UNKNOWN;
        cpu.p.z = (cpu.p.c == 1) ? 0 : UNKNOWN;
        cpu.p.c = UNKNOWN;
        break;
    case 0x4E: /* LSR $1234 */
    case 0x46: /* LSR $12 */
    case 0x5E: /* LSR $1234,x */
    case 0x56: /* LSR $12,x */
        cpu.p.n = 0;
        cpu.p.z = UNKNOWN;
        cpu.p.c = UNKNOWN;
        break;
    case 0x6E: /* ROR $1234 */
    case 0x66: /* ROR $12 */
    case 0x7E: /* ROR $1234,x */
    case 0x76: /* ROR $12,x */
        cpu.p.n = cpu.p.c;
        cpu.p.z = (cpu.p.c == 1) ? 0 : UNKNOWN;
        cpu.p.c = UNKNOWN;
        break;
    case 0x10: /* BPL *+$12 */
        if (ln < 1) break;
        if (cpu.p.n == 1) goto removecond;
        if (cpu.p.n == 0) cpu.branched = true;
        if (cpu.p.n == UNKNOWN_A) {
            cpu.p.z = 0;
            cpu.ar |= 0x80;
            cpu.av |= 0x80;
        }
        if (cpu.p.n == UNKNOWN_X) {
            cpu.p.z = 0;
            cpu.xr |= 0x80;
            cpu.xv |= 0x80;
        }
        if (cpu.p.n == UNKNOWN_Y) {
            cpu.p.z = 0;
            cpu.yr |= 0x80;
            cpu.yv |= 0x80;
        }
        cpu.p.n = 1;
        break;
    case 0x30: /* BMI *+$12 */
        if (ln < 1) break;
        if (cpu.p.n == 0) goto removecond;
        if (cpu.p.n == 1) cpu.branched = true;
        if (cpu.p.n == UNKNOWN_A) {
            cpu.ar &= ~0x80;
            cpu.av |= 0x80;
        }
        if (cpu.p.n == UNKNOWN_X) {
            cpu.xr &= ~0x80;
            cpu.xv |= 0x80;
        }
        if (cpu.p.n == UNKNOWN_Y) {
            cpu.yr &= ~0x80;
            cpu.yv |= 0x80;
        }
        cpu.p.n = 0;
        break;
    case 0x50: /* BVC *+$12 */
        if (ln < 1) break;
        if (cpu.p.v == 1) goto removecond;
        if (cpu.p.v == 0) cpu.branched = true;
        cpu.p.v = 1;
        break;
    case 0x70: /* BVS *+$12 */
        if (ln < 1) break;
        if (cpu.p.v == 0) goto removecond;
        if (cpu.p.v == 1) cpu.branched = true;
        cpu.p.v = 0;
        break;
    case 0x90: /* BCC *+$12 */
        if (ln < 1) break;
        if (cpu.p.c == 1) goto removecond;
        if (cpu.p.c == 0) cpu.branched = true;
        cpu.p.c = 1;
        break;
    case 0xB0: /* BCS *+$12 */
        if (ln < 1) break;
        if (cpu.p.c == 0) goto removecond;
        if (cpu.p.c == 1) cpu.branched = true;
        cpu.p.c = 0;
        break;
    case 0xD0: /* BNE *+$12 */
        if (ln < 1) break;
        if (cpu.p.z == 1) goto removecond;
        if (cpu.p.z == 0) cpu.branched = true;
        if (cpu.p.z == UNKNOWN_A) {
            cpu.p.n = 0;
            cpu.ar = 0;
            cpu.av = 0xff;
        }
        if (cpu.p.z == UNKNOWN_X) {
            cpu.p.n = 0;
            cpu.xr = 0;
            cpu.xv = 0xff;
        }
        if (cpu.p.z == UNKNOWN_Y) {
            cpu.p.n = 0;
            cpu.yr = 0;
            cpu.yv = 0xff;
        }
        cpu.p.z = 1;
        break;
    case 0xF0: /* BEQ *+$12 */
        if (ln < 1) break;
        if (cpu.p.z == 0) goto removecond;
        if (cpu.p.z == 1) cpu.branched = true;
        cpu.p.z = 0;
        break;
    case 0x2C: /* BIT $1234 */
    case 0x24: /* BIT $12 */
        cpu.p.n = UNKNOWN;
        cpu.p.v = UNKNOWN;
        cpu.p.z = (cpu.ar == 0 && cpu.av == 0xff) ? 1 : UNKNOWN;
        break;
    case 0x18: /* CLC */
        if (cpu.p.c == 0) goto removeclr;
        cpu.p.c = 0;
        break;
    case 0x38: /* SEC */
        if (cpu.p.c == 1) goto removeset;
        cpu.p.c = 1;
        break;
    case 0x58: /* CLI */
        if (cpu.p.i == 0) goto removeclr;
        cpu.p.i = 0;
        break;
    case 0x78: /* SEI */
        if (cpu.p.i == 1) goto removeset;
        cpu.p.i = 1;
        break;
    case 0xB8: /* CLV */
        if (cpu.p.v == 0) goto removeclr;
        cpu.p.v = 0;
        break;
    case 0xF8: /* SED */
        if (cpu.p.d == 1) goto removeset;
        cpu.p.d = 1;
        break;
    case 0xD8: /* CLD */
        if (cpu.p.d == 0) goto removeclr;
        cpu.p.d = 0;
        break;
    case 0xCE: /* DEC $1234 */
    case 0xC6: /* DEC $12 */
    case 0xDE: /* DEC $1234,x */
    case 0xD6: /* DEC $12,x */
    case 0xEE: /* INC $1234 */
    case 0xE6: /* INC $12 */
    case 0xFE: /* INC $1234,x */
    case 0xF6: /* INC $12,x */
        cpu.p.n = UNKNOWN;
        cpu.p.z = UNKNOWN;
        break;
    case 0xCA: /* DEX */
        old.xr = cpu.xr;
        cpu.xr--;
    decx:
        if (((cpu.xr ^ old.xr) & ~cpu.xv) != 0) cpu.xv = 0;
        cpu.p.n = ((cpu.xv & 0x80) == 0) ? UNKNOWN_X : (cpu.xr >> 7);
        cpu.p.z = ((cpu.xr & cpu.xv) == 0 && cpu.xv != 0xff) ? UNKNOWN_X : (((cpu.xr & cpu.xv) == 0) ? 1 : 0);
        break;
    case 0x88: /* DEY */
        old.yr = cpu.yr;
        cpu.yr--;
    decy:
        if (((cpu.yr ^ old.yr) & ~cpu.yv) != 0) cpu.yv = 0;
        cpu.p.n = ((cpu.yv & 0x80) == 0) ? UNKNOWN_Y : (cpu.yr >> 7);
        cpu.p.z = ((cpu.yr & cpu.yv) == 0 && cpu.yv != 0xff) ? UNKNOWN_Y : (((cpu.yr & cpu.yv) == 0) ? 1 : 0);
        break;
    case 0xE8: /* INX */
        old.xr = cpu.xr;
        cpu.xr++;
        goto decx;
    case 0xC8: /* INY */
        old.yr = cpu.yr;
        cpu.yr++;
        goto decy;
    case 0xA9: /* LDA #$12 */
        old.ar = cpu.ar; old.av = cpu.av; old.p.n = cpu.p.n; old.p.z = cpu.p.z;
        cpu.ar = adr;
        cpu.av = 0xff;
    loada:
        cpu.p.n = ((cpu.av & 0x80) == 0) ? UNKNOWN_A : (cpu.ar >> 7);
        cpu.p.z = ((cpu.ar & cpu.av) == 0 && cpu.av != 0xff) ? UNKNOWN_A : (((cpu.ar & cpu.av) == 0) ? 1 : 0);
        if (cpu.av != 0xff) {
            break;
        }
        if (cpu.ar == old.ar && old.av == 0xff &&
            old.p.n < UNKNOWN && old.p.n == cpu.p.n &&
            old.p.z < UNKNOWN && old.p.z == cpu.p.z) goto remove;
        if (cpu.ar == cpu.xr && cpu.xv == 0xff) {
            optname = "txa"; /* 0x8A TXA */
            goto replace;
        }
        if (cpu.ar == cpu.yr && cpu.yv == 0xff) {
            optname = "tya"; /* 0x98 TYA */
            goto replace;
        }
        if (old.av != 0xff) {
            break;
        }
        if (cpu.p.c == ((old.ar & 0x80) ? 1 : 0)) {
            if (cpu.ar == ((old.ar << 1) & 0xff)) {
                optname = "asl a"; /* 0x0A ASL A */
                goto replace;
            }
            if (cpu.ar == (((old.ar << 1) & 0xff) | cpu.p.c)) {
                optname = "rol a"; /* 0x2A ROL A */
                goto replace;
            }
        }
        if (cpu.p.c == ((old.ar & 1) ? 1 : 0)) {
            if (cpu.ar == (old.ar >> 1)) {
                optname = "lsr a"; /* 0x4A LSR A */
                goto replace;
            }
            if (cpu.ar == ((old.ar >> 1) | (cpu.p.c << 7))) {
                optname = "ror a"; /* 0x6A ROR A */
                goto replace;
            }
        }
        break;
    case 0xA2: /* LDX #$12 */
        old.xr = cpu.xr; old.xv = cpu.xv; old.p.n = cpu.p.n; old.p.z = cpu.p.z;
        cpu.xr = adr;
        cpu.xv = 0xff;
        cpu.p.n = ((cpu.xv & 0x80) == 0) ? UNKNOWN_X : (cpu.xr >> 7);
        cpu.p.z = ((cpu.xr & cpu.xv) == 0 && cpu.xv != 0xff) ? UNKNOWN_X : (((cpu.xr & cpu.xv) == 0) ? 1 : 0);
        if (cpu.xr == old.xr && old.xv == 0xff &&
            old.p.n < UNKNOWN && old.p.n == cpu.p.n &&
            old.p.z < UNKNOWN && old.p.z == cpu.p.z) goto remove;
        if (cpu.xr == cpu.ar && cpu.av == 0xff) {
            optname = "tax"; /* 0xAA TAX */
            goto replace;
        }
        if (cpu.xr == cpu.sr && cpu.sv == 0xff) {
            optname = "tsx"; /* 0xBA TSX */
            goto replace;
        }
        if (old.xv != 0xff) {
            break;
        }
        if (cpu.xr == ((old.xr + 1) & 0xff)) {
            optname = "inx"; /* 0xE8 INX */
            goto replace;
        }
        if (cpu.xr == ((old.xr - 1) & 0xff)) {
            optname = "dex"; /* 0xCA DEX */
            goto replace;
        }
        break;
    case 0xA0: /* LDY #$12 */
        old.yr = cpu.yr; old.yv = cpu.yv; old.p.n = cpu.p.n; old.p.z = cpu.p.z;
        cpu.yr = adr;
        cpu.yv = 0xff;
        cpu.p.n = ((cpu.yv & 0x80) == 0) ? UNKNOWN_Y : (cpu.yr >> 7);
        cpu.p.z = ((cpu.yr & cpu.yv) == 0 && cpu.yv != 0xff) ? UNKNOWN_Y : (((cpu.yr & cpu.yv) == 0) ? 1 : 0);
        if (cpu.yr == old.yr && old.yv == 0xff &&
            old.p.n < UNKNOWN && old.p.n == cpu.p.n &&
            old.p.z < UNKNOWN && old.p.z == cpu.p.z) goto remove;
        if (cpu.yr == cpu.ar && cpu.av == 0xff) {
            optname = "tay"; /* 0xA8 TAY */
            goto replace;
        }
        if (old.yv != 0xff) {
            break;
        }
        if (cpu.yr == ((old.yr + 1) & 0xff)) {
            optname = "iny"; /* 0xC8 INY */
            goto replace;
        }
        if (cpu.yr == ((old.yr - 1) & 0xff)) {
            optname = "dey"; /* 0x88 DEY */
            goto replace;
        }
        break;
    case 0xAE: /* LDX $1234 */
    case 0xA6: /* LDX $12 */
    case 0xBE: /* LDX $1234,y */
    case 0xB6: /* LDX $12,y */
        cpu.xv = 0;
        cpu.p.n = UNKNOWN_X;
        cpu.p.z = UNKNOWN_X;
        break;
    case 0xAC: /* LDY $1234 */
    case 0xA4: /* LDY $12 */
    case 0xBC: /* LDY $1234,x */
    case 0xB4: /* LDY $12,x */
        cpu.yv = 0;
        cpu.p.n = UNKNOWN_Y;
        cpu.p.z = UNKNOWN_Y;
        break;
    case 0xEA: /* NOP */
/*        return -1; */
    case 0x8D: /* STA $1234 */
    case 0x85: /* STA $12 */
    case 0x9D: /* STA $1234,x */
    case 0x95: /* STA $12,x */
    case 0x81: /* STA ($12,x) */
    case 0x99: /* STA $1234,y */
    case 0x91: /* STA ($12),y */
    case 0x8E: /* STX $1234 */
    case 0x86: /* STX $12 */
    case 0x96: /* STX $12,y */
    case 0x8C: /* STY $1234 */
    case 0x84: /* STY $12 */
    case 0x94: /* STY $12,x */
        break;
    case 0x48: /* PHA */
    case 0x08: /* PHP */
        old.sr = cpu.sr;
        cpu.sr--;
        if (((cpu.sr ^ old.sr) & ~cpu.sv) != 0) cpu.sv = 0;
        break;
    case 0x9A: /* TXS */
        old.sr = cpu.sr; old.sv = cpu.sv;
        cpu.sr = cpu.xr; cpu.sv = cpu.xv;
        if (cpu.sr == old.sr && old.sv == 0xff) goto remove;
        break;
    case 0xBA: /* TSX */
        old.xr = cpu.xr; old.xv = cpu.xv; old.p.n = cpu.p.n; old.p.z = cpu.p.z;
        cpu.xr = cpu.sr;
        cpu.xv = cpu.sv;
    loadx:
        cpu.p.n = ((cpu.xv & 0x80) == 0) ? UNKNOWN_X : (cpu.xr >> 7);
        cpu.p.z = ((cpu.xr & cpu.xv) == 0 && cpu.xv != 0xff) ? UNKNOWN_X : (((cpu.xr & cpu.xv) == 0) ? 1 : 0);
        if (cpu.xv != 0xff) {
            break;
        }
        if (cpu.xr == old.xr && old.xv == 0xff &&
            old.p.n < UNKNOWN && old.p.n == cpu.p.n &&
            old.p.z < UNKNOWN && old.p.z == cpu.p.z) goto remove;
        break;
    case 0xAA: /* TAX */
        old.xr = cpu.xr; old.xv = cpu.xv; old.p.n = cpu.p.n; old.p.z = cpu.p.z;
        cpu.xr = cpu.ar;
        cpu.xv = cpu.av;
        goto loadx;
    case 0xA8: /* TAY */
        old.yr = cpu.yr; old.yv = cpu.yv; old.p.n = cpu.p.n; old.p.z = cpu.p.z;
        cpu.yr = cpu.ar;
        cpu.yv = cpu.av;
        cpu.p.n = ((cpu.yv & 0x80) == 0) ? UNKNOWN_Y : (cpu.yr >> 7);
        cpu.p.z = ((cpu.yr & cpu.yv) == 0 && cpu.yv != 0xff) ? UNKNOWN_Y : (((cpu.yr & cpu.yv) == 0) ? 1 : 0);
        if (cpu.yv != 0xff) {
            break;
        }
        if (cpu.yr == old.yr && old.yv == 0xff &&
            old.p.n < UNKNOWN && old.p.n == cpu.p.n &&
            old.p.z < UNKNOWN && old.p.z == cpu.p.z) goto remove;
        break;
    case 0x8A: /* TXA */
        old.ar = cpu.ar; old.av = cpu.av; old.p.n = cpu.p.n; old.p.z = cpu.p.z;
        cpu.ar = cpu.xr;
        cpu.av = cpu.xv;
    loada2:
        cpu.p.n = ((cpu.av & 0x80) == 0) ? UNKNOWN_A : (cpu.ar >> 7);
        cpu.p.z = ((cpu.ar & cpu.av) == 0 && cpu.av != 0xff) ? UNKNOWN_A : (((cpu.ar & cpu.av) == 0) ? 1 : 0);
        if (cpu.av != 0xff) {
            break;
        }
        if (cpu.ar == old.ar && old.av == 0xff &&
            old.p.n < UNKNOWN && old.p.n == cpu.p.n &&
            old.p.z < UNKNOWN && old.p.z == cpu.p.z) goto remove;
        break;
    case 0x98: /* TYA */
        old.ar = cpu.ar; old.av = cpu.av; old.p.n = cpu.p.n; old.p.z = cpu.p.z;
        cpu.ar = cpu.yr;
        cpu.av = cpu.yv;
        goto loada2;
    case 0x28: /* PLP */
        old.sr = cpu.sr;
        cpu.sr++;
        if (((cpu.sr ^ old.sr) & ~cpu.sv) != 0) cpu.sv = 0;
        cpu.p.n = UNKNOWN;
        cpu.p.v = UNKNOWN;
        cpu.p.d = UNKNOWN;
        cpu.p.i = UNKNOWN;
        cpu.p.z = UNKNOWN;
        cpu.p.c = UNKNOWN;
        break;
    case 0x4C: /* JMP $1234 */
    case 0x6C: /* JMP ($1234) */
    case 0x40: /* RTI */
    case 0x60: /* RTS */
        cpu.branched = true;
        break;
    case 0x20: /* JSR $1234 */
        cpu.call = true;
        cpu.av = 0;
        cpu.xv = 0;
        cpu.yv = 0;
        cpu.sv = 0;
        cpu.p.n = UNKNOWN;
        cpu.p.v = UNKNOWN;
        cpu.p.d = UNKNOWN;
        cpu.p.i = UNKNOWN;
        cpu.p.z = UNKNOWN;
        cpu.p.c = UNKNOWN;
        break;
    case 0x00: /* BRK #$12 */
    default:
        cpu_opt_invalidate();
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
replace:
    err_msg2(ERROR___OPTIMIZABLE, optname, epoint);
}

void cpu_opt_invalidate(void) {
    cpu.branched = false;
    cpu.call = false;
    cpu.av = 0;
    cpu.xv = 0;
    cpu.yv = 0;
    cpu.sv = 0;
    cpu.pc = UNKNOWN;
    cpu.p.n = UNKNOWN;
    cpu.p.v = UNKNOWN;
    cpu.p.d = UNKNOWN;
    cpu.p.i = UNKNOWN;
    cpu.p.z = UNKNOWN;
    cpu.p.c = UNKNOWN;
}
