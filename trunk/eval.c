#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "misc.h"

extern char ident[];

static struct encoding_s *actual_encoding = ascii_encoding;

static int get_hex(struct value_s *v) {
    uint32_t val=0;
    int large=0;
    ignore();
    v->type=T_INT;
    while ((uint8_t)(here() ^ '0') < 10 || (uint8_t)((here() | 0x20)-'a') < 6 ) {
        if (val & 0xf8000000) large=1;
        val=(val << 4)+(here() & 15);
        if (here() & 0x40) val+=9;
        lpoint++;
    }
    v->u.num=val;
    if (large) {v->u.num=0x7fffffff;err_msg(ERROR_CONSTNT_LARGE,NULL);}
    return large;
}

static int get_bin(struct value_s *v) {
    uint32_t val=0;
    int large=0;
    ignore();
    v->type=T_INT;
    while ((here() & 0xfe)=='0') {
        if (val & 0x40000000) large=1;
        val=(val << 1) | (here() & 1);
        lpoint++;
    }
    v->u.num=val;
    if (large) {v->u.num=0x7fffffff;err_msg(ERROR_CONSTNT_LARGE,NULL); }
    return large;
}

static int get_dec(struct value_s *v) {
    uint32_t val=0;
    int large=0;
    v->type=T_INT;
    while ((uint8_t)(here() ^ '0') < 10) {
        if (val > 0x7fffffff/10) large=1;
        val=(val*10)+(here() & 15);
        if (val & 0x80000000) large=1;
        lpoint++;
    }
    v->u.num=val;
    if (large) {v->u.num=0x7fffffff;err_msg(ERROR_CONSTNT_LARGE,NULL); }
    return large;
}

static uint_fast16_t petascii(uint8_t quo) {
    uint32_t ch;

    if (!(ch=here())) {err_msg(ERROR______EXPECTED,"End of string"); return 256;}
    if (ch & 0x80) lpoint+=utf8in(pline + lpoint, &ch); else lpoint++;
    if (ch==quo) {
        if (here()==quo && !arguments.tasmcomp) lpoint++; // handle 'it''s'
        else return 257; // end of string;
    }
    if (arguments.toascii) {
        unsigned int n, also=0,felso,elozo;

        felso=actual_encoding[0].offset + 1;
        n=felso/2;
        for (;;) {  // do binary search
            struct encoding_s *e = &actual_encoding[n];
            if (ch >= e->start && ch <= e->end) {
                if (e->offset < 0) {
                    char sym[0x10];
                    uint_fast8_t n, end = -e->offset;
                    uint_fast16_t c;

                    for (n=0;;) {
                        if (!(ch=here())) {err_msg(ERROR______EXPECTED,"End of symbol");return 256;}
                        if (ch & 0x80) lpoint+=utf8in(pline + lpoint, &ch); else lpoint++;
                        if (ch == end) break;
                        if (ch == quo) {err_msg(ERROR______EXPECTED,"End of symbol");return 256;}
                        sym[n]=ch;
                        n++;
                        if (n == 0x10) {err_msg(ERROR_CONSTNT_LARGE,NULL);return 256;}
                    }
                    sym[n] = 0;
                    c = petsymbolic(sym);
                    if (c > 255) {err_msg(ERROR______EXPECTED, "PETASCII symbol");return 256;}
                    return encode((uint8_t)c);
                }
                return encode((uint8_t)(ch - e->start + e->offset));
            }

            elozo = n;
            n = ((ch > e->start) ? (felso+(also=n)) : (also+(felso=n)))/2;
            if (elozo == n) break;
        }
        err_msg(ERROR___UNKNOWN_CHR, (char *)ch);
        ch = 0;
    }
    return encode(ch);
}

static void get_string(struct value_s *v, char ch) {
    uint8_t line[linelength];  //current line data
    unsigned int i;
    uint_fast16_t val;

    val = petascii((uint8_t)ch);
    if (val < 256 && here()==ch) {lpoint++;v->type=T_CHR;v->u.num=val;return;}
    if (val == 256) {v->type=T_NONE;return;}
    i=0;
    for (;val < 256 && i < sizeof(line)-1;val = petascii((uint8_t)ch)) {
        line[i++]=(uint8_t)val;
    }
    if (val == 257) {
        v->type=T_TSTR;
        v->u.str.len=i;
        v->u.str.data=malloc(i);
        memcpy(v->u.str.data, line, i);
        return;
    }
    v->type=T_NONE;return;
}

static int get_label(int mode, struct value_s *v) {// 0=unknown stuff, 1=ok
    struct label_s *tmp;

    v->type=T_NONE;

    if (mode) {
        if (mode & 1) {
            sprintf(ident,"+%u+%u",reffile,current_context->forwr+(mode >> 1));
        } else {
            sprintf(ident,"-%u-%u",reffile,current_context->backr-(mode >> 1));
        }
    } else if (get_ident('.')) return 0; //label?

    tmp=find_label(ident);
    if (pass==1) {
        if (tmp) {
            tmp->proclabel=0;tmp->pass=pass;*v=tmp->value;
            if (tmp->varlabel && tmp->upass!=pass) err_msg(ERROR___NOT_DEFINED,ident);
        }
        return 1;
    }
    else {
        if (tmp) {
            if ((tmp->requires & current_provides)!=tmp->requires) err_msg(ERROR_REQUIREMENTS_,ident);
            if (tmp->conflicts & current_provides) err_msg(ERROR______CONFLICT,ident);
            if (tmp->varlabel && tmp->upass!=pass) err_msg(ERROR___NOT_DEFINED,ident);
            tmp->proclabel=0;tmp->pass=pass;*v=tmp->value;return 1;
        }
        if (mode) err_msg(ERROR___NOT_DEFINED,(mode & 1)?"+":"-");
        else
            err_msg(ERROR___NOT_DEFINED,ident); //never reached
    }

    return 0;
}

static void get_star(struct value_s *v) {
    struct star_s *tmp;

    tmp=new_star(vline);
    if (labelexists && tmp->addr != star) {
        fixeddig=0;
    }
    tmp->addr=star;
    v->type=T_INT;v->u.num=star;
}

/*
 * get priority for operator in an expression
 */
static int priority(char ch)
{
    switch (ch) {
    default:
    case '(':return 0;
    case 'l':          // <
    case 'h':          // >
    case 'H':          // `
    case 'S':return 1; // ^
    case 'O':return 2; // ||
    case 'X':return 3; // ^^
    case 'A':return 4; // &&
    case '=':
    case 'o':          // !=
    case '<':
    case '>':
    case 'g':          // >=
    case 's':return 5; // <=
    case '|':return 6;
    case '^':return 7;
    case '&':return 8;
    case 'm':          // <<
    case 'D':          // >>
    case 'd':return 9; // >>>
    case '+':
    case '-':return 10;
    case '*':
    case '/':
    case '%':return 11;// %
    case 'E':return 12;// **
    case 'n':          // -
    case 'p':return 13;// +
    case 'i':return 14;// ~
    case 't':return 15;// !
    }
}

static void get_exp_compat(int *wd, int *df,int *cd, struct value_s *v, enum type_e type) {// length in bytes, defined
    int nd=0,tp=0;
    unsigned int i;
    char ch;
    static uint8_t line[linelength];  //current line data

    struct {struct value_s val; char oper;} o_out[256];
    struct value_s v_stack[256];
    char o_oper[256];
    uint8_t outp = 0, operp = 0, vsp;
    unsigned int conv=0;
    int large=0;

    *wd=3;    // 0=byte 1=word 2=long 3=negative/too big
    *df=1;    // 1=result is ok, result is not yet known
    *cd=0;    // 0=error
    v->type = T_NONE;

    ignore();
    switch (here()) {
    case '!':*wd=1;lpoint++;ignore();break;
    case '<':conv=1;lpoint++;ignore();break;
    case '>':conv=2;lpoint++;ignore();break;
    case '(': tp=1; break;
    }
    for (;;) {
        ignore();ch = here();
        if (!nd) {
            switch (ch) {
            case '(': o_oper[operp++] = ch; lpoint++;continue;
            case '$': lpoint++;large|=get_hex(&o_out[outp].val);goto pushval;
            case '%': lpoint++;large|=get_bin(&o_out[outp].val);goto pushval;
            case '"': lpoint++;get_string(&o_out[outp].val, ch);goto pushval;
            case '*': lpoint++;get_star(&o_out[outp].val);goto pushval;
            }
            if (ch>='0' && ch<='9') { large|=get_dec(&o_out[outp].val);
            pushval:
                if (o_out[outp].val.type == T_INT && (o_out[outp].val.u.num & ~0xffff)) {
                    err_msg(ERROR_CONSTNT_LARGE,NULL);
                    o_out[outp].val.u.num = 0xffff;
                }
                o_out[outp++].oper=' ';nd = 1;continue;
            }
            if (!get_label(0, &o_out[outp].val)) {
            errget:
                for (i=0; i<outp; i++) if (o_out[i].oper==' ' && o_out[i].val.type == T_TSTR) free(o_out[i].val.u.str.data);
                return;
            }
            o_out[outp++].oper=' ';
            nd = 1;
            continue;
	}
	else {
            switch (ch) {
            case '&': goto push2;
            case '.': goto push2;
            case ':': goto push2;
            case '*': goto push2;
            case '/': goto push2;
            case '+': goto push2;
            case '-': goto push2;
            push2:
		if (tp) tp=1;
                while (operp && o_oper[operp-1] != '(') o_out[outp++].oper=o_oper[--operp];
                o_oper[operp++] = ch;
		nd=0;
		lpoint++;
		continue;
            case ')':
		while (operp && o_oper[operp-1] != '(') o_out[outp++].oper=o_oper[--operp];
		lpoint++;
		if (operp==1 && tp) tp=2;
		if (!operp) goto syntaxe;
		operp--;
		continue;
	    }
	    while (operp && o_oper[operp-1] != '(') o_out[outp++].oper=o_oper[--operp];
	    if (!operp) {
		if (tp==2) *cd=3; else *cd=1;
		break;
	    }
	    if (operp>1) goto syntaxe;
	    if (tp) *cd=2;
	    else {
            syntaxe:
                err_msg(ERROR_EXPRES_SYNTAX,NULL);
                goto errget;
            }
	    break;
	}
    }
    vsp = 0;
    for (i=0;i<outp;i++) {
	if ((ch=o_out[i].oper)==' ') {
            v_stack[vsp++]=o_out[i].val;
            continue;
        }
        if (v_stack[vsp-1].type == T_INT) {
            uint16_t val1;
            uint16_t val2;
        reint:
            val1 = v_stack[vsp-1].u.num;
            if (v_stack[vsp-2].type != T_INT && v_stack[vsp-2].type != T_CHR) {
                if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].u.str.data);
                if (v_stack[vsp-2].type != T_NONE) err_msg(ERROR____WRONG_TYPE,NULL);
                vsp--;v_stack[vsp-1].type = T_NONE;
                continue;
            }
            v_stack[vsp-2].type = T_INT;
            val2 = v_stack[vsp-2].u.num;
            switch (ch) {
            case '*': val1 *= val2; break;
            case '/': if (!val1) {err_msg(ERROR_DIVISION_BY_Z,NULL); val1 = 0xffff;large=1;} else val1=val2 / val1; break;
            case '+': val1 += val2; break;
            case '-': val1 = val2 - val1; break;
            case '&': val1 &= val2; break;
            case '.': val1 |= val2; break;
            case ':': val1 ^= val2; break;
            }
            vsp--;

            v_stack[vsp-1].type = T_INT;
            v_stack[vsp-1].u.num = val1;
	} else if (v_stack[vsp-1].type == T_CHR) {
            if (v_stack[vsp-2].type == T_INT || v_stack[vsp-2].type == T_CHR || v_stack[vsp-2].type == T_NONE) goto reint;
            if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].u.str.data);
            vsp--; v_stack[vsp-1].type = T_NONE;
            err_msg(ERROR____WRONG_TYPE,NULL);
            continue;
	} else if (v_stack[vsp-1].type == T_STR || v_stack[vsp-1].type == T_TSTR) {
            if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].u.str.data);
            vsp--; v_stack[vsp-1].type = T_NONE;
            err_msg(ERROR____WRONG_TYPE,NULL);
            continue;
	} else if (v_stack[vsp-1].type == T_NONE) {
            if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].u.str.data);
            if (v_stack[vsp-2].type != T_NONE) err_msg(ERROR____WRONG_TYPE,NULL);
            vsp--; v_stack[vsp-1].type = T_NONE;
            continue;
        } else err_msg(ERROR____WRONG_TYPE,NULL);
    }
    if (v_stack[0].type == T_TSTR) {
        if (v_stack[0].u.str.len<=linelength) memcpy(line, v_stack[0].u.str.data, v_stack[0].u.str.len);
        free(v_stack[0].u.str.data);
        v_stack[0].u.str.data = line;
        v_stack[0].type = T_STR;
    }
    if (v_stack[0].type == T_INT || v_stack[0].type == T_CHR || v_stack[0].type == T_STR) {
        if (large) *cd=0;
        if (type == T_NONE) {
            *v=v_stack[0];
            return;
        }
        if (type == T_INT)
            switch (v_stack[0].type) {
            case T_CHR:
                v->type = T_INT;
            case T_INT:
                *v=v_stack[0];
                if (conv==1) v->u.num = (uint8_t)v->u.num;
                if (conv==2) v->u.num = (uint8_t)(v->u.num >> 8);
                return;
            default:
                break;
        }
        *cd=0;
        err_msg(ERROR____WRONG_TYPE,NULL);
        return;
    }
    *df = 0;
    return;
}

void get_exp(int *wd, int *df,int *cd, struct value_s *v, enum type_e type) {// length in bytes, defined
    int nd=0,tp=0;
    unsigned int i;
    char ch;
    static uint8_t line[linelength];  //current line data

    struct {struct value_s val; char oper;} o_out[256];
    struct value_s v_stack[256];
    char o_oper[256];
    uint8_t outp = 0, operp = 0, vsp, prec, db;
    int large=0;

    if (arguments.tasmcomp) {
        get_exp_compat(wd,df,cd,v,type);
        return;
    }

    *wd=3;    // 0=byte 1=word 2=long 3=negative/too big
    *df=1;    // 1=result is ok, result is not yet known
    *cd=0;    // 0=error
    v->type = T_NONE;

    ignore();
    switch (here()) {
    case '@':
	switch (pline[++lpoint] | 0x20) {
	case 'b':*wd=0;break;
	case 'w':*wd=1;break;
	case 'l':*wd=2;break;
	default:err_msg(ERROR______EXPECTED,"@B or @W or @L"); return;
	}
        lpoint++;
	ignore();
        break;
    case '(': tp=1; break;
    }
    for (;;) {
        ignore();ch = here();
        if (!nd) {
            switch (ch) {
            case '(': o_oper[operp++] = ch; lpoint++;continue;
            case '+': db = 1;
                while ((ch=pline[lpoint+db])=='+') db++;
                if (!(ch>='0' && ch<='9') && ch!='$' && ch!='"' && ch!='\'' && ch!='%' && ch!='(' && ch!='_' && !(ch>='a' && ch<='z') && !(ch>='A' && ch<='Z')) {
                    lpoint += db; db = db * 2 - 1; goto ide;
                }
                ch = 'p'; break;
            case '-': db = 1;
                while ((ch=pline[lpoint+db])=='-') db++;
                if (!(ch>='0' && ch<='9') && ch!='$' && ch!='"' && ch!='\'' && ch!='%' && ch!='(' && ch!='_' && !(ch>='a' && ch<='z') && !(ch>='A' && ch<='Z')) {
                    lpoint += db; db = db * 2; goto ide;
                }
                ch = 'n'; break;
            case '!': ch = 't'; break;
            case '~': ch = 'i'; break;
            case '<': ch = 'l'; break;
            case '>': ch = 'h'; break;
            case '`': ch = 'H'; break;
            case '^': ch = 'S'; break;
            case '$': lpoint++;large|=get_hex(&o_out[outp].val);goto pushval;
            case '%': lpoint++;large|=get_bin(&o_out[outp].val);goto pushval;
            case '"':
            case '\'': lpoint++;get_string(&o_out[outp].val, ch);goto pushval;
            case '*': lpoint++;get_star(&o_out[outp].val);goto pushval;
            default: 
                if (ch>='0' && ch<='9') { large|=get_dec(&o_out[outp].val);
                pushval: o_out[outp++].oper=' ';nd = 1;continue;
                }
                db = 0;
                ide:
                if (!get_label(db, &o_out[outp].val)) {
                errget:
                    for (i=0; i<outp; i++) if (o_out[i].oper==' ' && o_out[i].val.type == T_TSTR) free(o_out[i].val.u.str.data);
                    return;
                }
                o_out[outp++].oper=' ';
                nd = 1;
                continue;
            }
            lpoint++;
            prec = priority(ch);
            while (operp && prec < priority(o_oper[operp-1])) o_out[outp++].oper=o_oper[--operp];
            o_oper[operp++] = ch;
            continue;
	}
	else {
            switch (ch) {
            case '&': if (pline[lpoint+1] == '&') {lpoint++;ch = 'A';} goto push2;
            case '|': if (pline[lpoint+1] == '|') {lpoint++;ch = 'O';} goto push2;
            case '^': if (pline[lpoint+1] == '^') {lpoint++;ch = 'X';} goto push2;
            case '*': if (pline[lpoint+1] == '*') {lpoint++;ch = 'E';} goto push2;
            case '%': goto push2;
            case '/': if (pline[lpoint+1] == '/') {lpoint++;ch = '%';} goto push2;
            case '+': goto push2;
            case '-': goto push2;
            case '=': if (pline[lpoint+1] == '=') lpoint++;
            push2:
		if (tp) tp=1;
                prec = priority(ch);
                while (operp && prec <= priority(o_oper[operp-1])) o_out[outp++].oper=o_oper[--operp];
                o_oper[operp++] = ch;
		nd=0;
		lpoint++;
		continue;
            case '<': 
                switch (pline[lpoint+1]) {
                case '>': lpoint++;ch = 'o'; break;
                case '<': lpoint++;ch = 'm'; break;
                case '=': lpoint++;ch = 's'; break;
                }
                goto push2;
            case '>':
                switch (pline[lpoint+1]) {
                case '<': lpoint++;ch = 'o'; break;
                case '>': lpoint++;if (pline[lpoint+1] == '>') {lpoint++;ch = 'd';} else ch = 'D'; break;
                case '=': lpoint++;ch = 'g'; break;
                }
                goto push2;
            case '!':
                if (pline[lpoint+1]=='=') {lpoint++;ch = 'o';goto push2;}
                break;
            case ')':
		while (operp && o_oper[operp-1] != '(') o_out[outp++].oper=o_oper[--operp];
		lpoint++;
		if (operp==1 && tp) tp=2;
		if (!operp) goto syntaxe;
		operp--;
		continue;
	    }
	    while (operp && o_oper[operp-1] != '(') o_out[outp++].oper=o_oper[--operp];
	    if (!operp) {
		if (tp==2) *cd=3; else *cd=1;
		break;
	    }
	    if (operp>1) goto syntaxe;
	    if (tp) *cd=2;
	    else {
            syntaxe:
                err_msg(ERROR_EXPRES_SYNTAX,NULL);
                goto errget;
            }
	    break;
	}
    }
    vsp = 0;
    for (i=0;i<outp;i++) {
	if ((ch=o_out[i].oper)==' ') {
            v_stack[vsp++]=o_out[i].val;
            continue;
        }
        if (v_stack[vsp-1].type == T_INT) {
            int32_t val1;
            int32_t val2;
        reint:
            val1 = v_stack[vsp-1].u.num;
            switch (ch) {
            case 'l': val1 = (uint8_t)val1; break;
            case 'h': val1 = (uint8_t)(val1 >> 8); break;
            case 'H': val1 = (uint8_t)(val1 >> 16); break;
            case 'S':
                if (v_stack[vsp-1].type == T_CHR) {
                    line[0]=v_stack[vsp-1].u.num;
                    line[1]=0;
                } else sprintf((char *)line,"%d",val1);
                v_stack[vsp-1].type = T_TSTR;
                v_stack[vsp-1].u.str.len=strlen((char *)line);
                v_stack[vsp-1].u.str.data=malloc(v_stack[vsp-1].u.str.len);
                memcpy(v_stack[vsp-1].u.str.data, line, v_stack[vsp-1].u.str.len);
                continue;
            case 'p': break;
            case 'n': val1 = -val1; break;
            case 'i': val1 = ~val1; break;
            case 't': val1 = !val1; break;
            default:
                if (v_stack[vsp-2].type != T_INT && v_stack[vsp-2].type != T_CHR) {
                    if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].u.str.data);
                    if (v_stack[vsp-2].type != T_NONE) err_msg(ERROR____WRONG_TYPE,NULL);
                    vsp--;v_stack[vsp-1].type = T_NONE;
                    continue;
                }
                v_stack[vsp-2].type = T_INT;
                val2 = v_stack[vsp-2].u.num;
                switch (ch) {
                case '=': val1 = (val2 == val1); break;
                case 'o': val1 = (val2 != val1); break;
                case '<': val1 = (val2 < val1); break;
                case '>': val1 = (val2 > val1); break;
                case 'g': val1 = (val2 >= val1); break;
                case 's': val1 = (val2 <= val1); break;
                case 'A': val1 = (val2 && val1); break;
                case 'O': val1 = (val2 || val1); break;
                case 'X': val1 = (!val2 ^ !val1); break;
                case '*': val1 *= val2; break;
                case '/': if (!val1) {err_msg(ERROR_DIVISION_BY_Z,NULL); val1 = 0x7fffffff; large=1;} else val1=val2 / val1; break;
                case '%': if (!val1) {err_msg(ERROR_DIVISION_BY_Z,NULL); val1 = 0x7fffffff; large=1;} else val1=val2 % val1; break;
                case '+': val1 += val2; break;
                case '-': val1 = val2 - val1; break;
                case '&': val1 &= val2; break;
                case '|': val1 |= val2; break;
                case '^': val1 ^= val2; break;
                case 'm':
                    if (val1 >= (signed)sizeof(val2)*8 || val1 <= -(signed)sizeof(val2)*8) val1=0;
                    else val1 = (val1 > 0) ? (val2 << val1) : (signed)((unsigned)val2 >> (-val1));
                    break;
                case 'd': 
                    if (val1 >= (signed)sizeof(val2)*8 || val1 <= -(signed)sizeof(val2)*8) val1=0;
                    else val1 = (val1 > 0) ? (signed)((unsigned)val2 >> val1) : (val2 << (-val1));
                    break;
                case 'D': 
                    if (val1 >= (signed)sizeof(val2)*8) val1 = (val2 > 0) ? 0 : -1;
                    if (val1 <= -(signed)sizeof(val2)*8) val1 = 0;
                    else if (val2 >= 0) val1 = (val1 > 0) ? (val2 >> val1) : (val2 << (-val1));
                    else val1 = ~((val1 > 0) ? ((~val2) >> val1) : ((~val2) << (-val1)));
                    break;
                case 'E': 
                    {
                        int32_t res = 1;

                        if (val1 < 0) {
                            if (!val2) {err_msg(ERROR_DIVISION_BY_Z,NULL); res = 0x7fffffff; large=1;}
                            else res = 0;
                        } else {
                            while (val1) {
                                if (val1 & 1) res *= val2;
                                val2 *= val2;
                                val1 >>= 1;
                            }
                        }
                        val1 = res;
                    }
                }
                vsp--;
            }
            v_stack[vsp-1].type = T_INT;
            v_stack[vsp-1].u.num = val1;
	} else if (v_stack[vsp-1].type == T_CHR) {
            switch (ch) {
            case 'l':
            case 'h':
            case 'H':
            case 'S':
            case 'p':
            case 'n':
            case 'i':
            case 't': goto reint;
            }
            if (v_stack[vsp-2].type == T_INT || v_stack[vsp-2].type == T_CHR || v_stack[vsp-2].type == T_NONE) goto reint;
            if (v_stack[vsp-2].type == T_STR || v_stack[vsp-2].type == T_TSTR) {
                line[0]=v_stack[vsp-1].u.num;
                v_stack[vsp-1].type = T_STR;
                v_stack[vsp-1].u.str.len = 1;
                v_stack[vsp-1].u.str.data = line;
                goto restr;
            }
            err_msg(ERROR____WRONG_TYPE,NULL);
            continue;
	} else if (v_stack[vsp-1].type == T_STR || v_stack[vsp-1].type == T_TSTR) {
            int32_t val1;
            switch (ch) {
            case 'l':
            case 'h':
            case 'H':
            case 'S':
            case 'p':
            case 'n':
            case 'i':
                v_stack[vsp-1].type = T_NONE;
                err_msg(ERROR____WRONG_TYPE,NULL);
                continue;
            case 't':
                v_stack[vsp-1].type = T_INT;
                v_stack[vsp-1].u.num = !v_stack[vsp-1].u.str.len;
                continue;
            }
            if (v_stack[vsp-2].type == T_CHR) {
                line[0]=v_stack[vsp-2].u.num;
                v_stack[vsp-2].type = T_STR;
                v_stack[vsp-2].u.str.len = 1;
                v_stack[vsp-2].u.str.data = line;
            }
        restr:
            if (v_stack[vsp-2].type != T_STR && v_stack[vsp-2].type != T_TSTR) {
                if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].u.str.data);
                if (v_stack[vsp-2].type != T_NONE) err_msg(ERROR____WRONG_TYPE,NULL);
                vsp--;v_stack[vsp-1].type = T_NONE;
                continue;
            }
            switch (ch) {
            case '=': val1=(v_stack[vsp-2].u.str.len == v_stack[vsp-1].u.str.len) && !memcmp(v_stack[vsp-2].u.str.data, v_stack[vsp-1].u.str.data, v_stack[vsp-1].u.str.len); break;
            case 'o': val1=(v_stack[vsp-2].u.str.len != v_stack[vsp-1].u.str.len) || memcmp(v_stack[vsp-2].u.str.data, v_stack[vsp-1].u.str.data, v_stack[vsp-1].u.str.len); break;
            case '<':
                val1=memcmp(v_stack[vsp-2].u.str.data, v_stack[vsp-1].u.str.data, (v_stack[vsp-1].u.str.len < v_stack[vsp-2].u.str.len)?v_stack[vsp-1].u.str.len:v_stack[vsp-2].u.str.len);
                if (val1==0) val1 = (v_stack[vsp-2].u.str.len < v_stack[vsp-1].u.str.len);
                else val1 = val1 < 0;
                break;
            case '>':
                val1=memcmp(v_stack[vsp-2].u.str.data, v_stack[vsp-1].u.str.data, (v_stack[vsp-1].u.str.len < v_stack[vsp-2].u.str.len)?v_stack[vsp-1].u.str.len:v_stack[vsp-2].u.str.len);
                if (val1==0) val1 = (v_stack[vsp-2].u.str.len > v_stack[vsp-1].u.str.len);
                else val1 = val1 > 0;
                break;
            case 's':
                val1=memcmp(v_stack[vsp-2].u.str.data, v_stack[vsp-1].u.str.data, (v_stack[vsp-1].u.str.len < v_stack[vsp-2].u.str.len)?v_stack[vsp-1].u.str.len:v_stack[vsp-2].u.str.len);
                if (val1==0) val1 = (v_stack[vsp-2].u.str.len <= v_stack[vsp-1].u.str.len);
                else val1 = val1 <= 0;
                break;
            case 'g':
                val1=memcmp(v_stack[vsp-2].u.str.data, v_stack[vsp-1].u.str.data, (v_stack[vsp-1].u.str.len < v_stack[vsp-2].u.str.len)?v_stack[vsp-1].u.str.len:v_stack[vsp-2].u.str.len);
                if (val1==0) val1 = (v_stack[vsp-2].u.str.len >= v_stack[vsp-1].u.str.len);
                else val1 = val1 >= 0;
                break;
            default:
                err_msg(ERROR____WRONG_TYPE,NULL);
                if (v_stack[vsp-1].type == T_TSTR) free(v_stack[vsp-1].u.str.data);
                if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].u.str.data);
                vsp--;v_stack[vsp-1].type = T_NONE;
                continue;
            }
            if (v_stack[vsp-1].type == T_TSTR) free(v_stack[vsp-1].u.str.data);
            if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].u.str.data);
            vsp--;
            v_stack[vsp-1].type = T_INT;
            v_stack[vsp-1].u.num = val1;
	} else if (v_stack[vsp-1].type == T_NONE) {
            switch (ch) {
            case '=':
            case 'o':
            case '<':
            case '>':
            case 'g':
            case 's':
            case 'A':
            case 'O':
            case 'X':
            case '*':
            case '/':
            case '%':
            case '+':
            case '-':
            case '&':
            case '|':
            case '^':
            case 'm':
            case 'd':
            case 'D':
            case 'E':
                if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].u.str.data);
                vsp--; break;
            }
            v_stack[vsp-1].type = T_NONE;
        } else err_msg(ERROR____WRONG_TYPE,NULL);
    }
    if (v_stack[0].type == T_TSTR) {
        if (v_stack[0].u.str.len<=linelength) memcpy(line, v_stack[0].u.str.data, v_stack[0].u.str.len);
        free(v_stack[0].u.str.data);
        v_stack[0].u.str.data = line;
        v_stack[0].type = T_STR;
    }
    if (v_stack[0].type == T_INT || v_stack[0].type == T_CHR || v_stack[0].type == T_STR) {
        if (large) *cd=0;
        if (type == T_NONE) {
            *v=v_stack[0];
            return;
        }
        if (type == T_INT)
            switch (v_stack[0].type) {
            case T_STR:
                if (v_stack[0].u.str.len < 5) {
                    v->type = T_INT;
                    v->u.num = 0;
                    for (i=v_stack[0].u.str.len;i;i--) v->u.num = (v->u.num << 8) | v_stack[0].u.str.data[i-1];
                } else {
                    *cd=0;
                    err_msg(ERROR_CONSTNT_LARGE,NULL);
                }
                return;
            case T_CHR:
                v->type = T_INT;
            case T_INT:
                *v=v_stack[0];
                return;
            default:
                break;
        }
        *cd=0;
        err_msg(ERROR____WRONG_TYPE,NULL);
        return;
    }
    *df = 0;
    return;
}

