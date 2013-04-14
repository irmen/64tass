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

#define _MISC_C_
#define _GNU_SOURCE
#include "misc.h"
#include <stdlib.h>
#include <string.h>
#include "opcodes.h"
#include "getopt.h"
#include "error.h"
#include "section.h"
#include "encoding.h"
#include "file.h"
#include "eval.h"
#include "variables.h"

struct arguments_s arguments={1,1,0,0,0,1,1,0,0,0,0,0,"a.out",OPCODES_C6502,NULL,NULL};

const uint8_t whatis[256]={
    WHAT_EOL,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_HASHMARK,WHAT_EXPRESSION,WHAT_EXPRESSION,0,WHAT_EXPRESSION,WHAT_EXPRESSION,0,WHAT_STAR,WHAT_EXPRESSION,WHAT_COMA,WHAT_EXPRESSION,WHAT_COMMAND,0,
    WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,0,WHAT_COMMENT,WHAT_EXPRESSION,WHAT_EQUAL,WHAT_EXPRESSION,0,
    WHAT_EXPRESSION,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,
    WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_EXPRESSION,0,0,0,WHAT_LBL,
    0,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,
    WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

/* --------------------------------------------------------------------------- */
unsigned int utf8in(const uint8_t *c, uint32_t *out) { /* only for internal use with validated utf-8! */
    uint32_t ch;
    int i, j;
    ch = c[0];

    if (ch < 0xe0) {
        ch ^= 0xc0;i = 2;
    } else if (ch < 0xf0) {
        ch ^= 0xe0;i = 3;
    } else if (ch < 0xf8) {
        ch ^= 0xf0;i = 4;
    } else if (ch < 0xfc) {
        ch ^= 0xf8;i = 5;
    } else {
        ch ^= 0xfc;i = 6;
    }

    for (j = 1;j < i; j++) {
        ch = (ch << 6) ^ c[j] ^ 0x80;
    }
    *out = ch;
    return i;
}

int str_hash(const str_t *s) {
    size_t l = s->len;
    const uint8_t *s2 = s->data;
    unsigned int h;
    if (!l) return 0;
    h = *s2 << 7;
    while (l--) h = (1000003 * h) ^ *s2++;
    h ^= s->len;
    return h & ((~(unsigned int)0) >> 1);
}

int str_casehash(const str_t *s) {
    size_t l = s->len;
    const uint8_t *s2 = s->data;
    uint8_t c;
    unsigned int h;
    if (!l) return 0;
    c = *s2;
    if (c >= 'a' && c <= 'z') c -= 32;
    h = c << 7;
    while (l--) {
        c = *s2++;
        if (c >= 'a' && c <= 'z') c -= 32;
        h = (1000003 * h) ^ c;
    }
    h ^= s->len;
    return h & ((~(unsigned int)0) >> 1);
}

int str_cmp(const str_t *s1, const str_t *s2) {
    if (s1->len != s2->len) return s1->len - s2->len;
    if (s1->data == s2->data) return 0;
    if (!s1->data && s2->data) return -1;
    if (s1->data && !s2->data) return 1;
    return memcmp(s1->data, s2->data, s1->len);
}

int str_casecmp(const str_t *s1, const str_t *s2) {
    if (s1->len != s2->len) return s1->len - s2->len;
    if (s1->data == s2->data) return 0;
    if (!s1->data && s2->data) return -1;
    if (s1->data && !s2->data) return 1;
    return strncasecmp((const char *)s1->data, (const char *)s2->data, s1->len);
}

void str_cpy(str_t *s1, const str_t *s2) {
    s1->len = s2->len;
    if (s2->data) {
        uint8_t *s = malloc(s2->len);
        if (!s) err_msg_out_of_memory();
        memcpy(s, s2->data, s2->len);
        s1->data = s;
    } else s1->data = NULL;
}

uint8_t *utf8out(uint32_t i, uint8_t *c) {
    if (!i) {
        *c++=0xc0;
        *c++=0x80;
	return c;
    }
    if (i < 0x80) {
        *c++=i;
	return c;
    }
    if (i < 0x800) {
        *c++=0xc0 | (i >> 6);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x10000) {
        *c++=0xe0 | (i >> 12);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x200000) {
        *c++=0xf0 | (i >> 18);
        *c++=0x80 | ((i >> 12) & 0x3f);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x4000000) {
        *c++=0xf0 | (i >> 24);
        *c++=0x80 | ((i >> 18) & 0x3f);
        *c++=0x80 | ((i >> 12) & 0x3f);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i & ~0x7fffffff) return c;
    *c++=0xf0 | (i >> 30);
    *c++=0x80 | ((i >> 24) & 0x3f);
    *c++=0x80 | ((i >> 18) & 0x3f);
    *c++=0x80 | ((i >> 12) & 0x3f);
    *c++=0x80 | ((i >> 6) & 0x3f);
    *c++=0x80 | (i & 0x3f);
    return c;
}

void tfree(void) {
    destroy_variables();
    destroy_section();
    destroy_file();
    err_destroy();
    destroy_encoding();
    destroy_values();
}

void tinit(void) {
    root_label.type = T_NONE;
    root_label.parent = NULL;
    root_label.name.data = NULL;
    root_label.name.len = 0;
    init_section();
    init_file();
    init_values();
    init_variables();
    init_eval();
}

static void labelname_print(const struct label_s *l, FILE *flab) {
    if (l->parent->parent) labelname_print(l->parent, flab);
    fputc('.', flab);
    fwrite(l->name.data, l->name.len, 1, flab);
}

static void labelprint2(const struct avltree *members, FILE *flab) {
    const struct avltree_node *n;
    const struct label_s *l;
    n = avltree_first(members);
    while (n) {
        l = cavltree_container_of(n, struct label_s, node);            /* already exists */
        n = avltree_next(n);
        if (l->name.data && l->name.len) {
            if (l->name.data[0]=='-' || l->name.data[0]=='+') continue;
            if (l->name.data[0]=='.' || l->name.data[0]=='#') continue;
        }
        switch (l->value->type) {
        case T_LBL:
        case T_MACRO:
        case T_SEGMENT:
        case T_UNION:
        case T_STRUCT: continue;
        default:break;
        }
        if (0) { /* for future use with VICE */
            if (l->value->type == T_CODE) {
                fprintf(flab, "al %x ", l->value->u.code.addr);
                labelname_print(l, flab);
                switch ((enum dtype_e)l->value->u.code.dtype) {
                case D_CHAR:
                case D_BYTE: 
                    fputs(" byte", flab);
                    if (l->value->u.code.size > 1) {
                        fprintf(flab, " %x", l->value->u.code.size);
                    }
                    break;
                case D_INT:
                case D_WORD: 
                    fputs(" word", flab);
                    if (l->value->u.code.size > 2) {
                        fprintf(flab, " %x", l->value->u.code.size);
                    }
                    break;
                case D_LINT:
                case D_LONG:
                    fputs(" long", flab);
                    if (l->value->u.code.size > 3) {
                        fprintf(flab, " %x", l->value->u.code.size);
                    }
                    break;
                case D_DINT:
                case D_DWORD:
                    fputs(" dword", flab);
                    if (l->value->u.code.size > 4) {
                        fprintf(flab, " %x", l->value->u.code.size);
                    }
                    break;
                case D_NONE:
                    break;
                }
                putc('\n', flab);
            }
            labelprint2(&l->members, flab);
        } else {
            switch (l->type) {
            case L_VAR:
                fwrite(l->name.data, l->name.len, 1, flab);
                if (l->name.len < 15) fputs("               " + l->name.len, flab);
                fputs(" .var ", flab);break;
            default: 
                fwrite(l->name.data, l->name.len, 1, flab);
                if (l->name.len < 16) fputs("                " + l->name.len, flab);
                fputs("= ", flab);break;
            }
            val_print(l->value, flab);
            putc('\n', flab);
        }
    }
}

void labelprint(void) {
    FILE *flab;

    if (arguments.label[0] == '-' && !arguments.label[1]) {
        flab = stdout;
    } else {
        if (!(flab=file_open(arguments.label,"wt"))) err_msg_file(ERROR_CANT_DUMP_LBL, arguments.label);
    }
    clearerr(flab);
    labelprint2(&root_label.members, flab);
    if (ferror(flab)) err_msg_file(ERROR_CANT_DUMP_LBL, arguments.label);
    if (flab != stdout) fclose(flab);
}

void sectionprint2(const struct section_s *l) {
    if (l->name.data) {
        sectionprint2(l->parent);
        fwrite(l->name.data, l->name.len, 1, stdout);
        putchar('.');
    }
}

void sectionprint(void) {
    const struct section_s *l;

    l = root_section.next;
    while (l) {
        char temp[10], temp2[10];
        if (l->defpass == pass) {
            if (l->size) {
                sprintf(temp, "$%04" PRIaddress, l->start);
                sprintf(temp2, "$%04" PRIaddress, l->start + l->size - 1);
                printf("Section:         %7s-%-7s ", temp, temp2);
            } else {
                printf("Section:                         ");
            }
            sectionprint2(l->parent);
            fwrite(l->name.data, l->name.len, 1, stdout);
            putchar('\n');
        }
        l = l->next;
    }
}

/* ------------------------------------------------------------------ */
static const char *short_options= "wqnbfWaTCBicxtel:L:I:msV?o:D:";

static const struct option long_options[]={
    {"no-warn"          , no_argument      , 0, 'w'},
    {"quiet"            , no_argument      , 0, 'q'},
    {"nonlinear"        , no_argument      , 0, 'n'},
    {"nostart"          , no_argument      , 0, 'b'},
    {"flat"             , no_argument      , 0, 'f'},
    {"wordstart"        , no_argument      , 0, 'W'},
    {"ascii"            , no_argument      , 0, 'a'},
    {"tasm-compatible"  , no_argument      , 0, 'T'},
    {"case-sensitive"   , no_argument      , 0, 'C'},
    {"long-branch"      , no_argument      , 0, 'B'},
    {"m65xx"            , no_argument      , 0,   1},
    {"m6502"            , no_argument      , 0, 'i'},
    {"m65c02"           , no_argument      , 0, 'c'},
    {"m65ce02"          , no_argument      , 0,   6},
    {"m65816"           , no_argument      , 0, 'x'},
    {"m65dtv02"         , no_argument      , 0, 't'},
    {"m65el02"          , no_argument      , 0, 'e'},
    {"mr65c02"          , no_argument      , 0,   4},
    {"mw65c02"          , no_argument      , 0,   5},
    {"labels"           , required_argument, 0, 'l'},
    {"list"             , required_argument, 0, 'L'},
    {""                 , required_argument, 0, 'I'},
    {"no-monitor"       , no_argument      , 0, 'm'},
    {"no-source"        , no_argument      , 0, 's'},
    {"version"          , no_argument      , 0, 'V'},
    {"usage"            , no_argument      , 0,   2},
    {"help"             , no_argument      , 0,   3},
    { 0, 0, 0, 0}
};

int testarg(int argc,char *argv[],struct file_s *fin) {
    int opt, longind;
    enum {UNKNOWN, UTF8, ISO1} type = UNKNOWN;

    while ((opt = getopt_long(argc, argv, short_options, long_options, &longind)) != -1)
        switch (opt)
        {
            case 'w':arguments.warning=0;break;
            case 'q':arguments.quiet=0;break;
            case 'W':arguments.wordstart=1;break;
            case 'n':arguments.nonlinear=1;break;
            case 'b':arguments.stripstart=1;break;
            case 'f':arguments.flat=1;break;
            case 'a':arguments.toascii=1;break;
            case 'T':arguments.tasmcomp=1;break;
            case 'o':arguments.output=optarg;break;
            case 'D':
                {
                    const uint8_t *c = (uint8_t *)optarg;
                    uint8_t *p=&fin->data[fin->p], ch2;
                    int i, j;
                    uint32_t ch = 0;

                    if (fin->p + linelength > fin->len) {
                        fin->len += linelength;
                        if (!(fin->data=realloc(fin->data, fin->len))) exit(1);
                    }
                    p=&fin->data[fin->p];
                    while ((ch=*c++)) {
                        switch (type) {
                        case UNKNOWN:
                        case UTF8:
                            if (ch < 0x80) {
                                i = 0;
                            } else if (ch < 0xc0) {
                                if (type == UNKNOWN) {
                                    type = ISO1; break;
                                }
                                ch = 0xfffd; i = 0;
                            } else if (ch < 0xe0) {
                                ch ^= 0xc0;i = 1;
                            } else if (ch < 0xf0) {
                                ch ^= 0xe0;i = 2;
                            } else if (ch < 0xf8) {
                                ch ^= 0xf0;i = 3;
                            } else if (ch < 0xfc) {
                                ch ^= 0xf8;i = 4;
                            } else if (ch < 0xfe) {
                                ch ^= 0xfc;i = 5;
                            } else {
                                type = ISO1; break;
                            }

                            for (j = i; i; i--) {
                                ch2 = *c++;
                                if (ch2 < 0x80 || ch2 >= 0xc0) {
                                    if (type == UNKNOWN) {
                                        type = ISO1;
                                        i = (j - i) * 6;
                                        p = utf8out(((~0x7f >> j) & 0xff) | (ch >> i), p);
                                        for (;i; i-= 6) {
                                            p = utf8out(((ch >> (i-6)) & 0x3f) | 0x80, p);
                                        }
                                        ch = ch2; j = 0;
                                        break;
                                    }
                                    c--;
                                    ch = 0xfffd;break;
                                }
                                ch = (ch << 6) ^ ch2 ^ 0x80;
                            }
                            if (j) type = UTF8;
                            if (ch == 0xfeff) continue;
                        case ISO1:
                            break;
                        }
                        if (ch && ch < 0x80) *p++ = ch; else p = utf8out(ch, p);
                        if (p > &fin->data[fin->p + linelength - 6*6]) break;
                    }
                    *p++ = 0;
                    fin->p = p - fin->data;
                }
                break;
            case 'B':arguments.longbranch=1;break;
            case 1:arguments.cpumode=OPCODES_C6502;break;
            case 'i':arguments.cpumode=OPCODES_C6502I;break;
            case 'c':arguments.cpumode=OPCODES_C65C02;break;
            case 6:arguments.cpumode=OPCODES_C65CE02;break;
            case 'x':arguments.cpumode=OPCODES_C65816;break;
            case 't':arguments.cpumode=OPCODES_C65DTV02;break;
            case 'e':arguments.cpumode=OPCODES_C65EL02;break;
            case 4:arguments.cpumode=OPCODES_CR65C02;break;
            case 5:arguments.cpumode=OPCODES_CW65C02;break;
            case 'l':arguments.label=optarg;break;
            case 'L':arguments.list=optarg;break;
            case 'I':include_list_add(optarg);break;
            case 'm':arguments.monitor=0;break;
            case 's':arguments.source=0;break;
            case 'C':arguments.casesensitive=1;break;
            case 2:puts(
             /* 12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
	       "Usage: 64tass [-abBCfnTqwWcitxmse?V] [-D <label>=<value>] [-o <file>]\n"
	       "	[-l <file>] [-L <file>] [-I <path>] [--ascii] [--nostart]\n"
               "	[--long-branch]	[--case-sensitive] [--flat] [--nonlinear]\n"
	       "	[--tasm-compatible] [--quiet] [--no-warn] [--wordstart] [--m65c02]\n"
               "	[--m6502] [--m65xx] [--m65dtv02] [--m65816] [--m65el02] [--mr65c02]\n"
               "	[--mw65c02] [--m65ce02] [--labels=<file>] [--list=<file>]\n"
               "	[--no-monitor] [--no-source] [--help] [--usage] [--version]\n"
               "	SOURCES");
                   destroy_eval();
                   tfree();
                   exit(0);

            case 'V':puts("64tass Turbo Assembler Macro V" VERSION);
                     destroy_eval();
                     tfree();
                     exit(0);
            case 3:
            case '?':if (optopt=='?' || opt==3) { puts(
	       "Usage: 64tass [OPTIONS...] SOURCES\n"
	       "64tass Turbo Assembler Macro V" VERSION "\n"
	       "\n"
	       "  -a, --ascii		Source is not in PETASCII\n"
	       "  -b, --nostart		Strip starting address\n"
	       "  -B, --long-branch	Automatic bxx *+3 jmp $xxxx\n"
	       "  -C, --case-sensitive	Case sensitive labels\n"
	       "  -D <label>=<value>	Define <label> to <value>\n"
	       "  -f, --flat		Generate flat output file\n"
	       "  -I <path>		Include search path\n"
	       "  -n, --nonlinear	Generate nonlinear output file\n"
	       "  -o <file>		Place output into <file>\n"
	       "  -q, --quiet		Display errors/warnings\n"
	       "  -T, --tasm-compatible Enable TASM compatible mode\n"
	       "  -w, --no-warn		Suppress warnings\n"
	       "  -W, --wordstart	Force 2 byte start address\n"
	       "\n"
	       " Target selection:\n"
	       "  -c, --m65c02		CMOS 65C02\n"
	       "      --m65ce02		CSG 65CE02\n"
	       "  -e, --m65el02		65EL02\n"
	       "  -i, --m6502		NMOS 65xx\n"
	       "      --m65xx		Standard 65xx (default)\n"
	       "  -t, --m65dtv02	65DTV02\n"
	       "  -x, --m65816		W65C816\n"
	       "      --mr65c02		R65C02\n"
	       "      --mw65c02		W65C02\n"
	       "\n"
	       " Source listing:\n"
	       "  -l, --labels=<file>	List labels into <file>\n"
	       "  -L, --list=<file>	List into <file>\n"
	       "  -m, --no-monitor	Don't put monitor code into listing\n"
	       "  -s, --no-source	Don't put source code into listing\n"
	       "\n"
	       " Misc:\n"
	       "  -?, --help		Give this help list\n"
	       "      --usage		Give a short usage message\n"
	       "  -V, --version		Print program version\n"
	       "\n"
	       "Mandatory or optional arguments to long options are also mandatory or optional\n"
	       "for any corresponding short options.\n"
	       "\n"
	       "Report bugs to <soci" "\x40" "c64.rulez.org>.");
               destroy_eval();
               tfree();
               exit(0);
            }
            default:fputs("Try `64tass --help' or `64tass --usage' for more information.\n", stderr);
                    destroy_eval();
                    tfree();
                    exit(1);
        }
    closefile(fin); fin->len = fin->p; fin->p = 0;
    if (fin->data && !(fin->data=realloc(fin->data, fin->len))) exit(1);
    if (argc <= optind) {
        fputs("Usage: 64tass [OPTIONS...] SOURCES\n"
              "Try `64tass --help' or `64tass --usage' for more information.\n", stderr);
        destroy_eval();
        tfree();
        exit(1);
    }
    return optind;
}
