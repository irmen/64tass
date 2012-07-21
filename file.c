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
#include "file.h"
#include "error.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static struct avltree file_tree;

static int star_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct star_s *a = avltree_container_of(aa, struct star_s, node);
    struct star_s *b = avltree_container_of(bb, struct star_s, node);

    return a->line - b->line;
}

static int file_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct file_s *a = avltree_container_of(aa, struct file_s, node);
    struct file_s *b = avltree_container_of(bb, struct file_s, node);

    return strcmp(a->name, b->name);
}

static void star_free(const struct avltree_node *aa)
{
    struct star_s *a = avltree_container_of(aa, struct star_s, node);

    avltree_destroy(&a->tree);
    free(a);
}

static void file_free(const struct avltree_node *aa)
{
    struct file_s *a = avltree_container_of(aa, struct file_s, node);

    avltree_destroy(&a->star);
    free((char *)a->data);
    free((char *)a->name);
    free(a);
}

static struct file_s *lastfi=NULL;
static uint16_t curfnum=1;
struct file_s *openfile(const char* name) {
    const struct avltree_node *b;
    struct file_s *tmp;
    if (!lastfi)
	if (!(lastfi=malloc(sizeof(struct file_s)))) err_msg_out_of_memory();
    lastfi->name=name;
    b=avltree_insert(&lastfi->node, &file_tree);
    if (!b) { //new file
	enum {UNKNOWN, UTF8, UTF16LE, UTF16BE, ISO1} type = UNKNOWN;
        int ch;
        FILE *f;
        uint32_t c = 0;

	if (!(lastfi->name=malloc(strlen(name)+1))) err_msg_out_of_memory();
        strcpy((char *)lastfi->name, name);
	lastfi->data=NULL;
	lastfi->len=0;
	lastfi->p=0;
        lastfi->open=0;
        avltree_init(&lastfi->star, star_compare, star_free);
        tmp = lastfi;
        lastfi=NULL;
        if (name[0]) {
            if (name[0]=='-' && !name[1]) f=stdin;
            else f=fopen(name,"rb");
            if (!f) {
                err_msg(ERROR_CANT_FINDFILE,name);
                return NULL;
            }
            if (arguments.quiet) printf("Assembling file:   %s\n",name);
            ch=getc(f);
            ungetc(ch, f); 
            if (!ch) type=UTF16BE; /* most likely */

            do {
                int i, j, ch2;
                uint8_t *pline;
                uint32_t lastchar;

                if (tmp->p + linelength > tmp->len) {
                    tmp->len += linelength * 2;
                    tmp->data=realloc(tmp->data, tmp->len);
                }
                pline=&tmp->data[tmp->p];
                for (;;) {
                    lastchar = c;
                    if (arguments.toascii) {
                        switch (type) {
                        case UNKNOWN:
                        case UTF8:
                            c = ch = getc(f);
                            if (ch == EOF) break;

                            if (ch < 0x80) goto done;
                            if (ch < 0xc0) {
                                if (type == UNKNOWN) {
                                    type = ISO1; break;
                                }
                                c = 0xfffd; break;
                            } else if (ch < 0xe0) {
                                c ^= 0xc0;i = 1;
                            } else if (ch < 0xf0) {
                                c ^= 0xe0;i = 2;
                            } else if (ch < 0xf8) {
                                c ^= 0xf0;i = 3;
                            } else if (ch < 0xfc) {
                                c ^= 0xf8;i = 4;
                            } else if (ch < 0xfe) {
                                c ^= 0xfc;i = 5;
                            } else {
                                if (type == UNKNOWN) {
                                    ch2=getc(f);
                                    if (ch == 0xff && ch2 == 0xfe) {
                                        type = UTF16LE;continue;
                                    }
                                    if (ch == 0xfe && ch2 == 0xff) {
                                        type = UTF16BE;continue;
                                    }
                                    ungetc(ch2, f); 
                                    type = ISO1; break;
                                }
                                c = 0xfffd; break;
                            }

                            for (j = i; i; i--) {
                                ch2 = getc(f);
                                if (ch2 < 0x80 || ch2 >= 0xc0) {
                                    if (type == UNKNOWN) {
                                        type = ISO1;
                                        i = (j - i) * 6;
                                        pline = utf8out(((~0x7f >> j) & 0xff) | (c >> i), pline);
                                        for (;i; i-= 6) {
                                            pline = utf8out(((c >> (i-6)) & 0x3f) | 0x80, pline);
                                        }
                                        c = ch2; j = 0;
                                        break;
                                    }
                                    ungetc(ch2, f);
                                    c = 0xfffd;break;
                                }
                                c = (c << 6) ^ ch2 ^ 0x80;
                            }
                            if (j) type = UTF8;
                            break;
                        case UTF16LE:
                            c = getc(f);
                            ch = getc(f);
                            if (ch == EOF) break;
                            c |= ch << 8;
                            if (c == 0xfffe) {
                                type = UTF16BE;
                                continue;
                            }
                            break;
                        case UTF16BE:
                            c = getc(f) << 8;
                            ch = getc(f);
                            if (ch == EOF) break;
                            c |= ch;
                            if (c == 0xfffe) {
                                type = UTF16LE;
                                continue;
                            }
                            break;
                        case ISO1:
                            c = ch = getc(f);
                            if (ch == EOF) break;
                            goto done;
                        }
                        if (c == 0xfeff) continue;
                        if (type != UTF8) {
                            if (c >= 0xd800 && c < 0xdc00) {
                                if (lastchar < 0xd800 || lastchar >= 0xdc00) continue;
                                c = 0xfffd;
                            } else if (c >= 0xdc00 && c < 0xe000) {
                                if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                                    c ^= 0x361dc00 ^ (lastchar << 10);
                                } else
                                    c = 0xfffd;
                            } else if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                                c = 0xfffd;
                            }
                        }
                    } else c = ch = getc(f);

                    if (ch == EOF) break;
                done:
                    if (c == 10) {
                        if (lastchar == 13) continue;
                        break;
                    } else if (c == 13) {
                        break;
                    }
                    if (c && c < 0x80) *pline++ = c; else pline = utf8out(c, pline);
                    if (pline > &tmp->data[tmp->p + linelength - 6*6]) {
                        err_msg(ERROR_LINE_TOO_LONG,NULL);ch=EOF;break;
                    }
                }
                i = pline - &tmp->data[tmp->p];
                pline=&tmp->data[tmp->p];
                if (i)
                    while (i && pline[i-1]==' ') i--;
                pline[i++] = 0;
                tmp->p += i;

                if (ch == EOF) break;
            } while (1);
            if (ferror(f)) err_msg(ERROR__READING_FILE,name);
            if (f!=stdin) fclose(f);
            tmp->len = tmp->p;
            tmp->data=realloc(tmp->data, tmp->len);
        }
	tmp->p = 0;

        tmp->uid=curfnum++;
    } else {
        tmp = avltree_container_of(b, struct file_s, node);
    }
    tmp->open++;
    return tmp;
}

void closefile(struct file_s *f) {
    if (f->open) f->open--;
}

static struct star_s *lastst=NULL;
struct star_s *new_star(line_t line) {
    const struct avltree_node *b;
    struct star_s *tmp;
    if (!lastst)
	if (!(lastst=malloc(sizeof(struct star_s)))) err_msg_out_of_memory();
    lastst->line=line;
    b=avltree_insert(&lastst->node, star_tree);
    if (!b) { //new label
	labelexists=0;
        avltree_init(&lastst->tree, star_compare, star_free);
	tmp=lastst;
	lastst=NULL;
	return tmp;
    }
    labelexists=1;
    return avltree_container_of(b, struct star_s, node);            //already exists
}

void destroy_file(void) {
    avltree_destroy(&file_tree);
    free(lastst);
    free(lastfi);
}

void init_file(void) {
    avltree_init(&file_tree, file_compare, file_free);
}

