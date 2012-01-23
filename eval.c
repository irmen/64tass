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

static int touch_label(struct label_s *tmp) {

    if (tmp) {
        tmp->ref=1;tmp->pass=pass;
        if (tmp->type != L_VAR || tmp->upass==pass) return 0;
    }
    if (pass != 1) {
        if (ident[0]=='-' || ident[0]=='+')
            err_msg(ERROR___NOT_DEFINED, (ident[0]=='-') ? "-" : "+");
        else
            err_msg(ERROR___NOT_DEFINED,ident);
    }

    return 1;
}

static void copy_name(struct value_s *val) {
    unsigned int len = val->u.ident.len;
    if (len > linelength - 1) len = linelength - 1;
    if (arguments.casesensitive) memcpy(ident, val->u.ident.name, len);
    else {
        unsigned int i;
        for (i=0;i < len;i++) ident[i]=lowcase(val->u.ident.name[i]);
    }
    ident[len] = 0;
}

static enum type_e try_resolv(struct value_s *val) {
    if (val->type == T_FORWR) {
        sprintf(ident,"+%x+%x", reffile, forwr + val->u.num - 1);
        goto ident;
    }
    if (val->type == T_BACKR) {
        sprintf(ident,"-%x-%x", reffile, backr - val->u.num);
        goto ident;
    }
    if (val->type == T_IDENT) {
        copy_name(val);
    ident:
        val->u.label = find_label(ident);
        if (touch_label(val->u.label)) {
            val->type = T_NONE;
            return T_NONE;
        }
        val->type = T_IDENTREF;
    }
    if (val->type == T_IDENTREF) {
        if ((val->u.label->requires & current_provides)!=val->u.label->requires) err_msg(ERROR_REQUIREMENTS_,ident);
        if (val->u.label->conflicts & current_provides) err_msg(ERROR______CONFLICT,ident);
        *val = val->u.label->value;
    }
    return val->type;
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
    case 'I':          // a[
    case '[':          // [a]
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
    case '~':return 14;// ~
    case '!':return 15;// !
    case '.':return 16;// .
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
            o_out[outp].val.u.ident.name = pline + lpoint;
            while ((((ch=here()) | 0x20) >= 'a' && (ch | 0x20) <= 'z') || (ch>='0' && ch<='9') || ch=='_') lpoint++;
            o_out[outp].val.u.ident.len = pline + lpoint - o_out[outp].val.u.ident.name;
            o_out[outp].val.type = T_IDENT;
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
                for (i=0; i<outp; i++) if (o_out[i].oper==' ' && o_out[i].val.type == T_TSTR) free(o_out[i].val.u.str.data);
                return;
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
        if (vsp < 2) goto syntaxe;
        try_resolv(&v_stack[vsp-1]);
        try_resolv(&v_stack[vsp-2]);
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
        } else {vsp--; v_stack[vsp-1].type = T_NONE; err_msg(ERROR____WRONG_TYPE,NULL);}
    }
    try_resolv(&v_stack[0]);
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
    int nd=0;
    unsigned int i;
    char ch;
    static uint8_t line[linelength];  //current line data

    struct {struct value_s val; char oper;} o_out[256], *values[256], *v1, *v2;
    char o_oper[256];
    uint8_t outp = 0, operp = 0, vsp, prec, db;
    int large=0;
    int32_t val;
    enum type_e t1, t2;

    if (arguments.tasmcomp) {
        get_exp_compat(wd,df,cd,v,type);
        return;
    }

    *wd=3;    // 0=byte 1=word 2=long 3=negative/too big
    *df=1;    // 1=result is ok, result is not yet known
    *cd=0;    // 0=error, 1=ok, 2=(a, 3=(), 4=[]
    v->type = T_NONE;
    o_oper[0]=0;

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
    }
    for (;;) {
        ignore();ch = here();
        if (!nd) {
            switch (ch) {
            case '[':
            case '(': o_oper[operp++] = ch; lpoint++;continue;
            case ')': goto syntaxe;
            case ']': goto syntaxe;
            case '+': db = 1;
                while ((ch=pline[lpoint+db])=='+') db++;
                if (!(ch>='0' && ch<='9') && ch!='$' && ch!='"' && ch!='\'' && ch!='%' && ch!='(' && ch!='_' && !(ch>='a' && ch<='z') && !(ch>='A' && ch<='Z')) {
                    lpoint += db;
                    o_out[outp].val.u.num = db;
                    o_out[outp].val.type = T_FORWR;goto pushval;
                }
                ch = 'p'; break;
            case '-': db = 1;
                while ((ch=pline[lpoint+db])=='-') db++;
                if (!(ch>='0' && ch<='9') && ch!='$' && ch!='"' && ch!='\'' && ch!='%' && ch!='(' && ch!='_' && !(ch>='a' && ch<='z') && !(ch>='A' && ch<='Z')) {
                    lpoint += db;
                    o_out[outp].val.u.num = db;
                    o_out[outp].val.type = T_BACKR;goto pushval;
                }
                ch = 'n'; break;
            case '!': break;
            case '~': break;
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
                o_out[outp].val.u.ident.name = pline + lpoint;
                while ((((ch=here()) | 0x20) >= 'a' && (ch | 0x20) <= 'z') || (ch>='0' && ch<='9') || ch=='_') lpoint++;
                o_out[outp].val.u.ident.len = pline + lpoint - o_out[outp].val.u.ident.name;
                if (!o_out[outp].val.u.ident.len) goto syntaxe;
                o_out[outp].val.type = T_IDENT;
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
            case '(': goto syntaxe;
            case '[': ch = 'I';goto push2;
            case '&': if (pline[lpoint+1] == '&') {lpoint++;ch = 'A';} goto push2;
            case '|': if (pline[lpoint+1] == '|') {lpoint++;ch = 'O';} goto push2;
            case '^': if (pline[lpoint+1] == '^') {lpoint++;ch = 'X';} goto push2;
            case '*': if (pline[lpoint+1] == '*') {lpoint++;ch = 'E';} goto push2;
            case '%': goto push2;
            case '/': if (pline[lpoint+1] == '/') {lpoint++;ch = '%';} goto push2;
            case '+': goto push2;
            case '-': goto push2;
            case '.': goto push2;
            case '=': if (pline[lpoint+1] == '=') lpoint++;
            push2:
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
                while (operp && o_oper[operp-1] != '(') {
                    if (o_oper[operp-1]=='[' || o_oper[operp-1]=='I') {err_msg(ERROR______EXPECTED,"("); goto error;}
                    o_out[outp++].oper=o_oper[--operp];
                }
		lpoint++;
                if (!operp) {err_msg(ERROR______EXPECTED,"("); goto error;}
                operp--;
		continue;
            case ']':
                while (operp && o_oper[operp-1] != '[') {
                    if (o_oper[operp-1]=='(') {err_msg(ERROR______EXPECTED,"["); goto error;}
                    o_out[outp++].oper=o_oper[--operp];
                    if (o_oper[operp] == 'I') break;
                }
		lpoint++;
                if (o_oper[operp] == 'I') continue;
                if (!operp) {err_msg(ERROR______EXPECTED,"["); goto error;}
                operp--;
		continue;
	    }
            if (o_oper[0]=='(') {
                if (!operp) {*cd=3;break;}
                while (operp && o_oper[operp-1] != '(') {
                    if (o_oper[operp-1]=='[' || o_oper[operp-1]=='I') {err_msg(ERROR______EXPECTED,"("); goto error;}
                    o_out[outp++].oper=o_oper[--operp];
                }
                if (operp==1) {*cd=2; break;}
                err_msg(ERROR______EXPECTED,")"); goto error;
            } else if (o_oper[0]=='[') {
                if (!operp) {*cd=4;break;}
                err_msg(ERROR______EXPECTED,"]"); goto error;
            } else {
                while (operp) {
                    if (o_oper[operp-1] == '(') {err_msg(ERROR______EXPECTED,")"); goto error;}
                    if (o_oper[operp-1] == '[' || o_oper[operp-1]=='I') {err_msg(ERROR______EXPECTED,"]"); goto error;}
                    o_out[outp++].oper=o_oper[--operp];
                }
                if (!operp) {*cd=1;break;}
            }
        syntaxe:
            err_msg(ERROR_EXPRES_SYNTAX,NULL);
        error:
            for (i=0; i<outp; i++) if (o_out[i].oper==' ' && o_out[i].val.type == T_TSTR) free(o_out[i].val.u.str.data);
            return;
	}
    }
    vsp = 0;
    for (i=0;i<outp;i++) {
	if ((ch=o_out[i].oper)==' ') {
            values[vsp++]=&o_out[i];
            continue;
        }
        if (vsp == 0) goto syntaxe;
        v1 = values[vsp-1];
        switch (ch) {
        case '.':
            v2 = v1; v1 = values[--vsp-1];
            if (vsp == 0) goto syntaxe;
            if (v1->val.type == T_IDENT) {
                copy_name(&v1->val);
                v1->val.u.label = find_label(ident);
                if (touch_label(v1->val.u.label)) {
                    v1->val.type = T_NONE;
                    continue;
                }
                v1->val.type = T_IDENTREF;
            }
            if (v2->val.type == T_IDENT && v1->val.type == T_IDENTREF) {
                copy_name(&v2->val);
                v1->val.u.label = find_label2(ident, &v1->val.u.label->members);
                if (touch_label(v1->val.u.label)) {
                    v1->val.type = T_NONE;
                    continue;
                }
                v1->val.type = T_IDENTREF;continue;
            }
            err_msg(ERROR____WRONG_TYPE,NULL);goto errtype;
        }

        switch (ch) {
        case 'l':
            switch (try_resolv(&v1->val)) {
            case T_INT:
            case T_CHR: v1->val.type = T_INT; v1->val.u.num = (uint8_t)v1->val.u.num;continue;
            case T_TSTR: free(v1->val.u.str.data);
            default: v1->val.type = T_NONE; err_msg(ERROR____WRONG_TYPE,NULL);
            case T_NONE: continue;
            }
        case 'h':
            switch (try_resolv(&v1->val)) {
            case T_INT:
            case T_CHR: v1->val.type = T_INT; v1->val.u.num = (uint8_t)(v1->val.u.num >> 8);continue;
            case T_TSTR: free(v1->val.u.str.data);
            default: v1->val.type = T_NONE; err_msg(ERROR____WRONG_TYPE,NULL);
            case T_NONE: continue;
            }
        case 'H':
            switch (try_resolv(&v1->val)) {
            case T_INT:
            case T_CHR: v1->val.type = T_INT; v1->val.u.num = (uint8_t)(v1->val.u.num >> 16);continue;
            case T_TSTR: free(v1->val.u.str.data);
            default: v1->val.type = T_NONE; err_msg(ERROR____WRONG_TYPE,NULL);
            case T_NONE: continue;
            }
        case 'S':
            switch (try_resolv(&v1->val)) {
            case T_INT:
                sprintf((char *)line,"%d", v1->val.u.num);
                goto totstr;
            case T_CHR:
                line[0]=v1->val.u.num;
                line[1]=0;
            totstr:
                v1->val.type = T_TSTR;
                v1->val.u.str.len=strlen((char *)line);
                v1->val.u.str.data=malloc(v1->val.u.str.len);
                memcpy(v1->val.u.str.data, line, v1->val.u.str.len);
                continue;
            case T_TSTR: free(v1->val.u.str.data);
            default: v1->val.type = T_NONE; err_msg(ERROR____WRONG_TYPE,NULL);
            case T_NONE: continue;
            }
        case 'p':
            switch (try_resolv(&v1->val)) {
            case T_INT:
            case T_CHR: v1->val.type = T_INT;continue;
            case T_TSTR: free(v1->val.u.str.data);
            default: v1->val.type = T_NONE; err_msg(ERROR____WRONG_TYPE,NULL);
            case T_NONE: continue;
            }
        case 'n':
            switch (try_resolv(&v1->val)) {
            case T_INT:
            case T_CHR: v1->val.type = T_INT; v1->val.u.num = -v1->val.u.num;continue;
            case T_TSTR: free(v1->val.u.str.data);
            default: v1->val.type = T_NONE; err_msg(ERROR____WRONG_TYPE,NULL);
            case T_NONE: continue;
            }
        case '~':
            switch (try_resolv(&v1->val)) {
            case T_INT:
            case T_CHR: v1->val.type = T_INT; v1->val.u.num = ~v1->val.u.num;continue;
            case T_TSTR: free(v1->val.u.str.data);
            default: v1->val.type = T_NONE; err_msg(ERROR____WRONG_TYPE,NULL);
            case T_NONE: continue;
            }
        case '!':
            switch (try_resolv(&v1->val)) {
            case T_INT:
            case T_CHR: v1->val.type = T_INT; v1->val.u.num = !v1->val.u.num;continue;
            case T_TSTR: free(v1->val.u.str.data);
            case T_STR:
                v1->val.type = T_INT;
                v1->val.u.num = !v1->val.u.str.len;
                continue;
            default: v1->val.type = T_NONE; err_msg(ERROR____WRONG_TYPE,NULL);
            case T_NONE: continue;
            }
        }
        v2 = v1; v1 = values[--vsp-1];
        if (vsp == 0) goto syntaxe;
        try_resolv(&v1->val);
        try_resolv(&v2->val);

        t1 = v1->val.type;
        t2 = v2->val.type;
        if (t1 == T_NONE || t2 == T_NONE) {
        errtype:
            if (v1->val.type == T_TSTR) free(v1->val.u.str.data);
            if (v2->val.type == T_TSTR) free(v2->val.u.str.data);
            v1->val.type = T_NONE;
            continue;
        }
        if (t1 == T_CHR && t2 == T_CHR) t1 = T_INT;
        if (t1 == T_CHR && t2 == T_INT) t1 = T_INT;
        if (t1 == T_INT && t2 == T_CHR) t2 = T_INT;

        if (t1 == T_INT && t2 == T_INT) {
            int32_t val1 = v1->val.u.num;
            int32_t val2 = v2->val.u.num;
            switch (ch) {
            case '=': val1 = ( val1 == val2);break;
            case 'o': val1 = ( val1 != val2);break;
            case '<': val1 = ( val1 <  val2);break;
            case '>': val1 = ( val1 >  val2);break;
            case 's': val1 = ( val1 <= val2);break;
            case 'g': val1 = ( val1 >= val2);break;
            case 'A': val1 = ( val1 && val2);break;
            case 'O': val1 = ( val1 || val2);break;
            case 'X': val1 = (!val1 ^ !val2);break;
            case '*': val1 = ( val1 *  val2);break;
            case '/': if (!val2) {err_msg(ERROR_DIVISION_BY_Z,NULL); val1 = 0x7fffffff; large=1;}
                else  val1 = ( val1 /  val2); break;
            case '%': if (!val2) {err_msg(ERROR_DIVISION_BY_Z,NULL); val1 = 0x7fffffff; large=1;}
                else  val1 = ( val1 %  val2); break;
            case '+': val1 = ( val1 +  val2);break;
            case '-': val1 = ( val1 -  val2);break;
            case '&': val1 = ( val1 &  val2);break;
            case '|': val1 = ( val1 |  val2);break;
            case '^': val1 = ( val1 ^  val2);break;
            case 'm':
                if (val2 >= (signed)sizeof(val1)*8 || val2 <= -(signed)sizeof(val1)*8) val1=0;
                else val1 = (val2 > 0) ? (val1 << val2) : (signed)((unsigned)val1 >> (-val2));
                break;
            case 'd': 
                if (val2 >= (signed)sizeof(val1)*8 || val2 <= -(signed)sizeof(val1)*8) val1=0;
                else val1 = (val2 > 0) ? (signed)((unsigned)val1 >> val2) : (val1 << (-val2));
                break;
            case 'D': 
                if (val2 >= (signed)sizeof(val1)*8) val1 = (val1 > 0) ? 0 : -1;
                if (val2 <= -(signed)sizeof(val1)*8) val1 = 0;
                else if (val1 >= 0) val1 = (val2 > 0) ? (val1 >> val2) : (val1 << (-val2));
                else val1 = ~((val2 > 0) ? ((~val1) >> val2) : ((~val1) << (-val2)));
                break;
            case 'E': 
                {
                    int32_t res = 1;

                    if (val2 < 0) {
                        if (!val1) {err_msg(ERROR_DIVISION_BY_Z,NULL); res = 0x7fffffff; large=1;}
                        else res = 0;
                    } else {
                        while (val2) {
                            if (val2 & 1) res *= val1;
                            val1 *= val1;
                            val2 >>= 1;
                        }
                    }
                    val1 = res;
                }
                break;
            case 'I': val1 = ((unsigned)val1 >> val2) & 1;
                break;
            default: v1->val.type = T_NONE; err_msg(ERROR____WRONG_TYPE,NULL);continue;
            }
            v1->val.type = T_INT; v1->val.u.num = val1; continue;
        }

        if (t1 == T_TSTR) t1 = T_STR;
        if (t2 == T_TSTR) t2 = T_STR;
        if (t1 == T_CHR && t2 == T_STR) {
            line[0]=v1->val.u.num;
            t1 = v1->val.type = T_STR;
            v1->val.u.str.data = (uint8_t *)&line;
            v1->val.u.str.len = 1;
        }
        if (t1 == T_STR && t2 == T_CHR) {
            line[0]=v2->val.u.num;
            t2 = v2->val.type = T_STR;
            v2->val.u.str.data = (uint8_t *)&line;
            v2->val.u.str.len = 1;
        }
        if (t1 == T_STR && t2 == T_STR) {
            switch (ch) {
                case '=':
                    val = (v1->val.u.str.len == v2->val.u.str.len) && !memcmp(v1->val.u.str.data, v2->val.u.str.data, v1->val.u.str.len);
                strcomp:
                    if (v1->val.type == T_TSTR) free(v1->val.u.str.data);
                    if (v2->val.type == T_TSTR) free(v2->val.u.str.data);
                    v1->val.type = T_INT; v1->val.u.num = val;
                    continue;
                case 'o':
                    val = (v1->val.u.str.len != v2->val.u.str.len) || memcmp(v1->val.u.str.data, v2->val.u.str.data, v1->val.u.str.len);
                    goto strcomp;
                case '<':
                    val = memcmp(v1->val.u.str.data, v2->val.u.str.data, (v1->val.u.str.len < v2->val.u.str.len) ? v1->val.u.str.len:v2->val.u.str.len);
                    if (!val) val = v1->val.u.str.len < v2->val.u.str.len; else val = val < 0;
                    goto strcomp;
                case '>':
                    val = memcmp(v1->val.u.str.data, v2->val.u.str.data, (v1->val.u.str.len < v2->val.u.str.len) ? v1->val.u.str.len:v2->val.u.str.len);
                    if (!val) val = v1->val.u.str.len > v2->val.u.str.len; else val = val > 0;
                    goto strcomp;
                case 's':
                    val = memcmp(v1->val.u.str.data, v2->val.u.str.data, (v1->val.u.str.len < v2->val.u.str.len) ? v1->val.u.str.len:v2->val.u.str.len);
                    if (!val) val = v1->val.u.str.len <= v2->val.u.str.len; else val = val <= 0;
                    goto strcomp;
                case 'g':
                    val = memcmp(v1->val.u.str.data, v2->val.u.str.data, (v1->val.u.str.len < v2->val.u.str.len) ? v1->val.u.str.len:v2->val.u.str.len);
                    if (!val) val = v1->val.u.str.len >= v2->val.u.str.len; else val = val >= 0;
                    goto strcomp;
                case 'A': val = (v1->val.u.str.len && v2->val.u.str.len); goto strcomp;
                case 'O': val = (v1->val.u.str.len || v2->val.u.str.len); goto strcomp;
                case 'X': val = (!v1->val.u.str.len ^ !v2->val.u.str.len); goto strcomp;
                default: err_msg(ERROR____WRONG_TYPE,NULL);goto errtype;
            }
        }
        if (t1 == T_STR && t2 == T_CHR) t2 = T_INT;
        if (t1 == T_STR && t2 == T_INT) {
            if (ch=='I') {
                val=0;
                if (v2->val.u.num >= 0 && (unsigned)v2->val.u.num < v1->val.u.str.len) val = v1->val.u.str.data[v2->val.u.num];
                else err_msg(ERROR_CONSTNT_LARGE,NULL);
                if (v1->val.type == T_TSTR) free(v1->val.u.str.data);
                v1->val.type = T_CHR; v1->val.u.num = val;continue;
            }
        }
        err_msg(ERROR____WRONG_TYPE,NULL);goto errtype;
    }

    try_resolv(&values[0]->val);
    if (values[0]->val.type == T_TSTR) {
        if (values[0]->val.u.str.len<=linelength) memcpy(line, values[0]->val.u.str.data, values[0]->val.u.str.len);
        free(values[0]->val.u.str.data);
        values[0]->val.u.str.data = line;
        values[0]->val.type = T_STR;
    }
    if (values[0]->val.type == T_INT || values[0]->val.type == T_CHR || values[0]->val.type == T_STR) {
        if (large) *cd=0;
        if (type == T_NONE) {
            *v=values[0]->val;
            return;
        }
        if (type == T_INT)
            switch (values[0]->val.type) {
            case T_STR:
                if (values[0]->val.u.str.len < 5) {
                    v->type = T_INT;
                    v->u.num = 0;
                    for (i=values[0]->val.u.str.len;i;i--) v->u.num = (v->u.num << 8) | values[0]->val.u.str.data[i-1];
                } else {
                    *cd=0;
                    err_msg(ERROR_CONSTNT_LARGE,NULL);
                }
                return;
            case T_CHR:
                v->type = T_INT;
            case T_INT:
                *v=values[0]->val;
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

