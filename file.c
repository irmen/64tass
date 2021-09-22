/*
    $Id: file.c 2712 2021-09-18 23:09:42Z soci $

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
#include "file.h"
#include <string.h>
#include "wchar.h"
#include <errno.h>
#ifdef _WIN32
#include <locale.h>
#include <windows.h>
#endif
#if defined _POSIX_C_SOURCE || defined __unix__ || defined __MINGW32__
#include <sys/stat.h>
#endif
#include "64tass.h"
#include "unicode.h"
#include "error.h"
#include "arguments.h"
#include "unicodedata.h"
#include "avl.h"

static struct {
    size_t len, mask;
    struct file_s **data;
    uint16_t uid;
} file_table;

static struct file_s *file_table_update(struct file_s *p) {
    size_t mask, hash, offs;
    if (file_table.len * 3 / 2 >= file_table.mask) {
        size_t i, max = (file_table.data == NULL) ? 8 : (file_table.mask + 1) << 1;
        struct file_s **n;
        new_array(&n, max);
        memset(n, 0, max * sizeof *n);
        mask = max - 1;
        if (file_table.data != NULL) {
            for (i = 0; i <= file_table.mask; i++) if (file_table.data[i] != NULL) {
                hash = (size_t)file_table.data[i]->hash;
                offs = hash & mask;
                while (n[offs] != NULL) {
                    hash >>= 5;
                    offs = (5 * offs + hash + 1) & mask;
                }
                n[offs] = file_table.data[i];
            }
            free(file_table.data);
        }
        file_table.mask = mask;
        file_table.data = n;
    }
    mask = file_table.mask;
    hash = (size_t)p->hash;
    offs = hash & mask;
    while (file_table.data[offs] != NULL) {
        struct file_s *d = file_table.data[offs];
        if (p->hash == d->hash && strcmp(p->name, d->name) == 0) {
            if (p->base.data == d->base.data) return d;
            if (p->base.len == d->base.len && memcmp(p->base.data, d->base.data, p->base.len) == 0) {
                return d;
            }
        }
        hash >>= 5;
        offs = (5 * offs + hash + 1) & mask;
    } 
    file_table.data[offs] = p;
    file_table.len++;
    return NULL;
}

#if defined _WIN32 || defined __WIN32__ || defined __MSDOS__ || defined __DOS__
static inline bool is_driveletter(const char *name) {
    return (uint8_t)((name[0] | 0x20) - 'a') < 26 && name[1] == ':';
}
#endif

static size_t get_base(const char *base) {
#if defined _WIN32 || defined __WIN32__ || defined __MSDOS__ || defined __DOS__
    size_t i, j = is_driveletter(base) ? 2 : 0;
    for (i = j; base[i] != '\0'; i++) {
        if (base[i] == '/' || base[i] == '\\') j = i + 1;
    }
    return j;
#else
    const char *c = strrchr(base, '/');
    return (c != NULL) ? (size_t)(c - base) + 1 : 0;
#endif
}

static char *get_path(const str_t *v, const char *base) {
    char *path;
    size_t i, len;

#if defined _WIN32 || defined __WIN32__ || defined __MSDOS__ || defined __DOS__
    if (v->len != 0 && (v->data[0] == '/' || v->data[0] == '\\')) i = is_driveletter(base) ? 2 : 0;
    else i = (v->len > 1 && is_driveletter((const char *)v->data)) ? 0 : get_base(base);
#else
    i = (v->len != 0 && v->data[0] == '/') ? 0 : get_base(base);
#endif
    len = i + 1;
    if (inc_overflow(&len, v->len)) err_msg_out_of_memory();
    new_array(&path, len);
    memcpy(path, base, i);
    memcpy(path + i, v->data, v->len);
    path[i + v->len] = 0;
    return path;
}

static bool portability(const str_t *name, linepos_t epoint) {
    struct linepos_s epoint2;
    const uint8_t *pos;
#if defined _WIN32 || defined __WIN32__ || defined __MSDOS__ || defined __DOS__
    if (name->len == 0) return true;
    pos = (const uint8_t *)memchr(name->data, '\\', name->len);
    if (pos != NULL) {
        epoint2.line = epoint->line;
        epoint2.pos = interstring_position(epoint, name->data, (size_t)(pos - name->data));
        err_msg2(ERROR_____BACKSLASH, name, &epoint2);
        return false;
    }
    if (name->data[0] == '/' || is_driveletter((const char *)name->data)) {
        err_msg2(ERROR_ABSOLUTE_PATH, name, epoint);
        return false;
    }
#else
    const char *c;
    if (name->len == 0) return true;
    for (c = "\\:*?\"<>|"; *c != '\0'; c++) {
        pos = (const uint8_t *)memchr(name->data, *c, name->len);
        if (pos == NULL) continue;
        epoint2.line = epoint->line;
        epoint2.pos = interstring_position(epoint, name->data, (size_t)(pos - name->data));
        err_msg2(ERROR__RESERVED_CHR, name, &epoint2);
        return false;
    }
    if (name->data[0] == '/') {
        err_msg2(ERROR_ABSOLUTE_PATH, name, epoint);
        return false;
    }
#endif
    return true;
}

#ifdef _WIN32
static wchar_t *get_real_name(const wchar_t *name) {
    typedef DWORD _stdcall (*Getfinalpathnamebyhandleptr)(HANDLE, LPWSTR, DWORD, DWORD);
    static Getfinalpathnamebyhandleptr get_final_path_by_handle;
    static HINSTANCE kernel_handle;
    DWORD ret;
    size_t len = wcslen(name) + 1;
    wchar_t *real_name = allocate_array(wchar_t, len);
    if (real_name == NULL) return NULL;
    if (get_final_path_by_handle == NULL && kernel_handle == NULL) {
        kernel_handle = LoadLibrary("kernel32.dll");
        if (kernel_handle != NULL) get_final_path_by_handle = (Getfinalpathnamebyhandleptr)GetProcAddress(kernel_handle, "GetFinalPathNameByHandleW");
    } 
    if (get_final_path_by_handle != NULL) {
        HANDLE handle = CreateFileW(name, 0, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL); 
        if (handle != INVALID_HANDLE_VALUE) {
            ret = get_final_path_by_handle(handle, real_name, len, 12);
            if (ret > len) {
                wchar_t *tmp = reallocate_array(real_name, ret);
                if (tmp != NULL) {
                    real_name = tmp;
                    len = ret;
                    ret = get_final_path_by_handle(handle, real_name, len, 12);
                }
            }
            CloseHandle(handle);
            if (ret < len && ret != 0) {
                return real_name;
            }
        }
    }
    ret = GetLongPathNameW(name, real_name, len);
    if (ret > len) {
        wchar_t *tmp = reallocate_array(real_name, ret);
        if (tmp != NULL) {
            real_name = tmp;
            len = ret;
            ret = GetLongPathNameW(name, real_name, len);
        }
    }
    if (ret < len && ret != 0) {
        return real_name;
    }
    free(real_name);
    return NULL;
}

static bool portability2(const str_t *name, const char *realname, linepos_t epoint) {
    wchar_t *wname = utf8_to_wchar(realname, SIZE_MAX);
    if (wname != NULL) {
        bool different = false;
        wchar_t *wname2 = get_real_name(wname);
        if (wname2 != NULL) {
            wchar_t *bname = utf8_to_wchar((const char *)name->data, name->len);
            if (bname != NULL) {
                size_t len = wcslen(wname);
                size_t len2 = wcslen(bname);
                size_t ret = wcslen(wname2);
                free(bname);
                if (len2 > len) len2 = len;
                if (len2 > ret) len2 = ret;
                for (; len2 > 0; len2--) {
                    if (wname[len - len2] != wname2[ret - len2] && wname2[ret - len2] != '\\') {
                        different = true;
                        break;
                    }
                }
            }
            free(wname2);
        }
        free(wname);
        if (different) {
            err_msg2(ERROR___INSENSITIVE, name, epoint);
            return false;
        }
    }
    return portability(name, epoint);
}
#endif

static void file_free_static(struct file_s *a)
{
    free(a->source.data);
    free(a->binary.data);
    free(a->line);
    free(a->nomacro);
}

static void file_free(struct file_s *a)
{
    file_free_static(a);
    if (a->name != a->realname) free((char *)a->name);
    free((char *)a->realname);
    free(a);
}

static bool file_extend(struct file_data_s *file) {
    uint8_t *d;
    filesize_t len2;
    if (add_overflow(file->len, 4096, &len2)) return true;
    d = reallocate_array(file->data, len2);
    if (d == NULL) return true;
    file->data = d;
    file->len = len2;
    return false;
}

static void file_normalize(struct file_data_s *file, filesize_t fp) {
    if (fp == 0) {
        free(file->data);
        file->data = NULL;
    } else if (file->len != fp) {
        uint8_t *d = reallocate_array(file->data, fp);
        if (d != NULL) file->data = d;
    }
    file->len = fp;
    file->read = true;
}

static bool flush_ubuff(struct ubuff_s *ubuff, filesize_t *p2, struct file_data_s *tmp) {
    uint32_t i;
    filesize_t p = *p2;
    for (i = 0; i < ubuff->p; i++) {
        unichar_t ch;
        if (p + 6*6 + 1 > tmp->len && file_extend(tmp)) return true;
        ch = ubuff->data[i];
        if (ch != 0 && ch < 0x80) tmp->data[p++] = (uint8_t)ch; else p += utf8out(ch, tmp->data + p);
    }
    *p2 = p;
    return false;
}

static unichar_t fromiso2(unichar_t c) {
    mbstate_t ps;
    wchar_t w;
    int olderrno;
    ssize_t l;
    uint8_t c2 = (uint8_t)(c | 0x80);

    memset(&ps, 0, sizeof ps);
    olderrno = errno;
    l = (ssize_t)mbrtowc(&w, (char *)&c2, 1,  &ps);
    errno = olderrno;
    if (l < 0) return c2;
    return (unichar_t)w;
}

static inline unichar_t fromiso(unichar_t c) {
    static unichar_t conv[128];
    c &= 0x7f;
    if (conv[c] == 0) conv[c] = fromiso2(c);
    return conv[c];
}

static filesize_t fsize(FILE *f) {
#if defined _POSIX_C_SOURCE || defined __unix__ || defined __MINGW32__
    struct stat st;
    if (fstat(fileno(f), &st) == 0) {
        if (S_ISREG(st.st_mode) && st.st_size > 0) {
            return (st.st_size & ~(off_t)~(filesize_t)0) == 0 ? (filesize_t)st.st_size : ~(filesize_t)0;
        }
    }
#else
    if (fseek(f, 0, SEEK_END) == 0) {
        long len = ftell(f);
        rewind(f);
        if (len > 0) {
            return (unsigned long)len < ~(filesize_t)0 ? (filesize_t)len : ~(filesize_t)0;
        }
    }
#endif
    return 0;
}

static int read_binary(struct file_s *file, FILE *f) {
    filesize_t fp = 0;
    int err = 1;
    filesize_t fs = fsize(f);
    if (fs > 0) {
        file->binary.data = allocate_array(uint8_t, fs);
        if (file->binary.data != NULL) file->binary.len = fs;
    }
    clearerr(f); errno = 0;
    if (file->binary.len != 0 || !file_extend(&file->binary)) {
        bool check = (file->binary.data != NULL);
        for (;;) {
            fp += (filesize_t)fread(file->binary.data + fp, 1, file->binary.len - fp, f);
            if (feof(f) == 0 && fp >= file->binary.len && !signal_received) {
                if (check) {
                    int c2 = getc(f);
                    check = false;
                    if (c2 != EOF) {
                        if (file_extend(&file->binary)) break;
                        file->binary.data[fp++] = (uint8_t)c2;
                        continue;
                    }
                } else {
                    if (file_extend(&file->binary)) break;
                    continue;
                }
            }
            err = 0;
            break;
        }
    }
    file_normalize(&file->binary, fp);
    return err;
}

static struct ubuff_s last_ubuff;
static int read_source(struct file_s *file, FILE *f) {
    enum { REPLACEMENT_CHARACTER = 0xfffd };
    Encoding_types encoding = arguments.to_ascii ? E_UNKNOWN : E_RAW;
    filesize_t fp = 0, bfp = 0;
    unichar_t c = 0;
    struct ubuff_s ubuff = last_ubuff;
    size_t max_lines = 0;
    linenum_t lines = 0;
    uint8_t buffer[BUFSIZ * 2];
    filesize_t bp = 0, bl;
    unsigned int qr = 1;
    int err = 1;
    filesize_t fs = (f == NULL) ? file->binary.len : fsize(f);
    if (fs > 0) {
        filesize_t len2;
        if (add_overflow(fs, 4096, &len2)) len2 = ~(filesize_t)0;
        file->source.data = allocate_array(uint8_t, len2);
        if (file->source.data != NULL) file->source.len = len2;
        max_lines = (len2 / 20 + 1024) & ~(size_t)1023;
        file->line = allocate_array(filesize_t, max_lines);
        if (file->line == NULL) max_lines = 0;
    }
    if (f == NULL) {
        bfp = fs < BUFSIZ ? fs : BUFSIZ;
        bl = bfp;
        if (bl > 0) memcpy(buffer, file->binary.data, bl);
    } else {
        clearerr(f); errno = 0;
        bl = (filesize_t)fread(buffer, 1, BUFSIZ, f);
    }
    if (encoding == E_UNKNOWN && bl != 0 && buffer[0] == 0) encoding = E_UTF16BE; /* most likely */
#ifdef _WIN32
    setlocale(LC_CTYPE, "");
#endif
    ubuff.p = 0;
    do {
        filesize_t p;
        unichar_t lastchar;
        bool qc = true;
        uint8_t cclass = 0;

        if (lines >= max_lines) {
            filesize_t *d;
            size_t len2;
            if (add_overflow(max_lines, 1024, &len2)) goto failed;
            d = reallocate_array(file->line, len2);
            if (d == NULL) goto failed;
            file->line = d;
            max_lines = len2;
        }
        file->line[lines] = fp;
        if (inc_overflow(&lines, 1)) goto failed;
        p = fp;
        for (;;) {
            unsigned int i, j;
            uint8_t ch2;
            if (p + 6*6 + 1 > file->source.len && file_extend(&file->source)) goto failed;
            if (bp / (BUFSIZ / 2) == qr) {
                if (qr == 1) {
                    qr = 3;
                    if (f == NULL) {
                        filesize_t rl = fs - bfp;
                        if (rl > BUFSIZ) rl = BUFSIZ;
                        if (rl > 0) {
                            memcpy(buffer + BUFSIZ, file->binary.data + bfp, rl);
                            bfp += rl;
                        }
                        bl = BUFSIZ + rl;
                    } else {
                        if (feof(f) == 0) bl = BUFSIZ + (filesize_t)fread(buffer + BUFSIZ, 1, BUFSIZ, f);
                    }
                } else {
                    qr = 1;
                    if (f == NULL) {
                        filesize_t rl = fs - bfp;
                        if (rl > BUFSIZ) rl = BUFSIZ;
                        if (rl > 0) {
                            memcpy(buffer, file->binary.data + bfp, rl);
                            bfp += rl;
                        }
                        bl = rl;
                    } else {
                        if (feof(f) == 0) bl = (filesize_t)fread(buffer, 1, BUFSIZ, f);
                    }
                }
                if (signal_received) bl = bp;
            }
            if (bp == bl) break;
            lastchar = c;
            c = buffer[bp]; bp = (bp + 1) % (BUFSIZ * 2);
            switch (encoding) {
            case E_RAW:
                if (c == 10) {
                    if (lastchar == 13) continue;
                    goto eol;
                }
                if (c == 13) {
                    goto eol;
                }
                if (c != 0 && c < 0x80) file->source.data[p++] = (uint8_t)c; else p += utf8out(c, file->source.data + p);
                continue;
            case E_UNKNOWN:
            case E_UTF8:
                if (c < 0x80) goto done;
                if (c < 0xc0) {
                invalid:
                    if (encoding == E_UNKNOWN) {
                        c = fromiso(c);
                        encoding = E_ISO; break;
                    }
                    c = REPLACEMENT_CHARACTER; break;
                }
                ch2 = (bp == bl) ? 0 : buffer[bp];
                if (c < 0xe0) {
                    if (c < 0xc2) goto invalid;
                    c ^= 0xc0; i = 1;
                } else if (c < 0xf0) {
                    if ((c ^ 0xe0) == 0 && (ch2 ^ 0xa0) >= 0x20) goto invalid;
                    c ^= 0xe0; i = 2;
                } else if (c < 0xf8) {
                    if ((c ^ 0xf0) == 0 && (uint8_t)(ch2 - 0x90) >= 0x30) goto invalid;
                    c ^= 0xf0; i = 3;
                } else if (c < 0xfc) {
                    if ((c ^ 0xf8) == 0 && (uint8_t)(ch2 - 0x88) >= 0x38) goto invalid;
                    c ^= 0xf8; i = 4;
                } else if (c < 0xfe) {
                    if ((c ^ 0xfc) == 0 && (uint8_t)(ch2 - 0x84) >= 0x3c) goto invalid;
                    c ^= 0xfc; i = 5;
                } else {
                    if (encoding != E_UNKNOWN) goto invalid;
                    if (c == 0xff && ch2 == 0xfe) encoding = E_UTF16LE;
                    else if (c == 0xfe && ch2 == 0xff) encoding = E_UTF16BE;
                    else goto invalid;
                    bp = (bp + 1) % (BUFSIZ * 2);
                    continue;
                }

                for (j = i; i != 0; i--) {
                    if (bp != bl) {
                        ch2 = buffer[bp];
                        if ((ch2 ^ 0x80) < 0x40) {
                            c = (c << 6) ^ ch2 ^ 0x80;
                            bp = (bp + 1) % (BUFSIZ * 2);
                            continue;
                        }
                    }
                    if (encoding != E_UNKNOWN) {
                        c = REPLACEMENT_CHARACTER;break;
                    }
                    encoding = E_ISO;
                    i = (j - i) * 6;
                    qc = false;
                    if (ubuff.p >= ubuff.len && extend_ubuff(&ubuff)) goto failed;
                    ubuff.data[ubuff.p++] = fromiso(((~0x7f >> j) & 0xff) | (c >> i));
                    for (;i != 0; i-= 6) {
                        if (ubuff.p >= ubuff.len && extend_ubuff(&ubuff)) goto failed;
                        ubuff.data[ubuff.p++] = fromiso(((c >> (i-6)) & 0x3f) | 0x80);
                    }
                    if (bp == bl) goto eol;
                    c = (ch2 >= 0x80) ? fromiso(ch2) : ch2;
                    j = 0;
                    bp = (bp + 1) % (BUFSIZ * 2);
                    break;
                }
                if (j != 0) encoding = E_UTF8;
                break;
            case E_UTF16LE:
                if (bp == bl) goto invalid;
                c |= (unichar_t)buffer[bp] << 8; bp = (bp + 1) % (BUFSIZ * 2);
                if (c == 0xfffe) {
                    encoding = E_UTF16BE;
                    continue;
                }
                break;
            case E_UTF16BE:
                if (bp == bl) goto invalid;
                c = (c << 8) | buffer[bp]; bp = (bp + 1) % (BUFSIZ * 2);
                if (c == 0xfffe) {
                    encoding = E_UTF16LE;
                    continue;
                }
                break;
            case E_ISO:
                if (c >= 0x80) c = fromiso(c);
                goto done;
            }
            if (c == 0xfeff) continue;
            if (encoding != E_UTF8) {
                if (c >= 0xd800 && c < 0xdc00) {
                    if (lastchar < 0xd800 || lastchar >= 0xdc00) continue;
                    c = REPLACEMENT_CHARACTER;
                } else if (c >= 0xdc00 && c < 0xe000) {
                    if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                        c ^= 0x360dc00 ^ (lastchar << 10);
                        c += 0x10000;
                    } else c = REPLACEMENT_CHARACTER;
                } else if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                    c = REPLACEMENT_CHARACTER;
                }
            }
        done:
            if (c < 0xc0) {
                if (c == 10) {
                    if (lastchar == 13) continue;
                    goto eol;
                }
                if (c == 13) {
                    goto eol;
                }
                cclass = 0;
                if (!qc) {
                    if (unfc(&ubuff)) goto failed;
                    qc = true;
                }
                if (ubuff.p == 1) {
                    if (ubuff.data[0] != 0 && ubuff.data[0] < 0x80) file->source.data[p++] = (uint8_t)ubuff.data[0]; else p += utf8out(ubuff.data[0], file->source.data + p);
                } else {
                    if (ubuff.p != 0 && flush_ubuff(&ubuff, &p, &file->source)) goto failed;
                    ubuff.p = 1;
                }
                ubuff.data[0] = c;
            } else {
                const struct properties_s *prop = uget_property(c);
                uint8_t ncclass = prop->combclass;
                if ((ncclass != 0 && cclass > ncclass) || (prop->property & (qc_N | qc_M)) != 0) {
                    qc = false;
                    if (ubuff.p >= ubuff.len && extend_ubuff(&ubuff)) goto failed;
                    ubuff.data[ubuff.p++] = c;
                } else {
                    if (!qc) {
                        if (unfc(&ubuff)) goto failed;
                        qc = true;
                    }
                    if (ubuff.p == 1) {
                        if (ubuff.data[0] != 0 && ubuff.data[0] < 0x80) file->source.data[p++] = (uint8_t)ubuff.data[0]; else p += utf8out(ubuff.data[0], file->source.data + p);
                    } else {
                        if (ubuff.p != 0 && flush_ubuff(&ubuff, &p, &file->source)) goto failed;
                        ubuff.p = 1;
                    }
                    ubuff.data[0] = c;
                }
                cclass = ncclass;
            }
        }
    eol:
        if (!qc && unfc(&ubuff)) goto failed;
        if (ubuff.p == 1) {
            if (ubuff.data[0] != 0 && ubuff.data[0] < 0x80) file->source.data[p++] = (uint8_t)ubuff.data[0]; else p += utf8out(ubuff.data[0], file->source.data + p);
        } else {
            if (ubuff.p != 0 && flush_ubuff(&ubuff, &p, &file->source)) goto failed;
        }
        ubuff.p = 0;
        while (p > fp && (file->source.data[p - 1] == ' ' || file->source.data[p - 1] == '\t')) p--;
        if (fp == 0 && p > 1 && file->source.data[0] == '#' && file->source.data[1] == '!') p = 0;
        file->source.data[p++] = 0;
        fp = p;
    } while (bp != bl);
    err = 0;
failed:
#ifdef _WIN32
    setlocale(LC_CTYPE, "C");
#endif
    last_ubuff = ubuff;
    file->lines = lines;
    if (lines != max_lines) {
        filesize_t *d = reallocate_array(file->line, lines);
        if (lines == 0 || d != NULL) file->line = d;
    }
    file->encoding = encoding;
    file->uid = ++file_table.uid;
    file_normalize(&file->source, fp);
    return err;
}

static struct file_s file_defines;
static struct file_s file_stdin;
static struct file_s *lastfi;
struct file_s *file_open(const str_t *name, const char *base, unsigned int ftype, linepos_t epoint) {
    struct file_s *file;
    switch (ftype) {
    case 0:
        file = &file_stdin;
        break;
    case 3:
        file = &file_defines;
        break;
    default:
        {
            str_t n;
            if (lastfi == NULL) new_instance(&lastfi);
            if (base == NULL) {
                lastfi->name = get_path(name, "");
                lastfi->base.data = (const uint8_t *)lastfi->name;
                lastfi->base.len = get_base(lastfi->name);
            } else {
                lastfi->name = get_path(name, base);
                lastfi->base.data = (const uint8_t *)base;
                lastfi->base.len = get_base(base);
            }
            n.data = (const uint8_t *)lastfi->name;
            n.len = strlen(lastfi->name);
            lastfi->hash = ((unsigned int)str_hash(&n) + (unsigned int)str_hash(&lastfi->base)) & ((~0U) >> 1);
            file = file_table_update(lastfi);
        }
        if (file == NULL) { /* new file */
            file = lastfi;
            lastfi = NULL;
            file->nomacro = NULL;
            file->line = NULL;
            file->lines = 0;
            file->source.data = NULL;
            file->source.len = 0;
            file->source.read = false;
            file->binary.data = NULL;
            file->binary.len = 0;
            file->binary.read = false;
            file->open = 0;
            file->err_no = 0;
            file->read_error = false;
            file->portable = false;
            file->pass = 0;
            file->uid = 0;
            file->entercount = 0;
            file->encoding = E_UNKNOWN;
            file->realname = file->name;
            file->cmdline = false;
        } else {
            free((char *)lastfi->name);
        }
    }

    if (file->err_no == 0 && !(ftype == 1 ? file->binary.read : file->source.read)) {
        int err = 1;
        if (ftype == 3) { 
            file->read_error = true;
            file->binary.data = (uint8_t *)arguments.defines.data;
            arguments.defines.data = NULL;
            file->binary.len = (arguments.defines.len & ~(size_t)~(filesize_t)0) == 0 ? (filesize_t)arguments.defines.len : ~(filesize_t)0;
            arguments.defines.len = 0;
            file->binary.read = true;
        } else if (arguments.quiet) {
            fputs((ftype == 1) ? "Reading file:      " : "Assembling file:   ", stdout);
            argv_print(file->realname, stdout);
            putchar('\n');
            fflush(stdout);
        }
        if (ftype != 1 && file->binary.read) {
            err = read_source(file, NULL);
            if (err != 0) errno = ENOMEM;
        } else {
            FILE *f;
            if (ftype == 0) {
                f = stdin;
            } else {
                f = fopen_utf8(file->realname, "rb");
                if (f == NULL && base != NULL) {
                    struct include_list_s *i;
                    for (i = arguments.include; i != NULL; i = i->next) {
                        char *path = get_path(name, i->path);
                        f = fopen_utf8(path, "rb");
                        if (f != NULL) {
                            file->realname = path;
                            break;
                        }
                        free(path);
                    }
                }
            }
            if (f != NULL) {
                file->read_error = true;
                if (ftype == 1) {
                    err = read_binary(file, f);
                } else {
                    err = read_source(file, f);
                }
                if (err != 0) errno = ENOMEM;
                err |= ferror(f);
                if (f != stdin) err |= fclose(f);
            }
        }
        if (signal_received) err = errno = EINTR;
        if (err != 0 && errno != 0) {
            file->err_no = errno;
            if (ftype == 1) {
                free(file->binary.data);
                file->binary.data = NULL;
                file->binary.len = 0;
            } else {
                free(file->source.data);
                file->source.data = NULL;
                file->source.len = 0;
                free(file->line);
                file->line = NULL;
                file->lines = 0;
            }
        }
    }

    if (file->err_no != 0) {
        if (file->pass != pass) {
            errno = file->err_no;
            err_msg_file(file->read_error ? ERROR__READING_FILE : ERROR_CANT_FINDFILE, file->realname, epoint);
            file->pass = pass;
        }
        return NULL;
    }
    if (!file->portable && base != NULL && diagnostics.portable) {
#ifdef _WIN32
        file->portable = portability2(name, file->realname, epoint);
#else
        file->portable = portability(name, epoint);
#endif
    }
    return file;
}

struct starnode_s {
    struct star_s star;
    struct avltree tree;
    struct avltree_node node;
};

static FAST_CALL int star_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    linenum_t a = cavltree_container_of(aa, struct starnode_s, node)->star.line;
    linenum_t b = cavltree_container_of(bb, struct starnode_s, node)->star.line;
    return (a > b) - (a < b);
}

static struct stars_s {
    struct starnode_s stars[255];
    struct stars_s *next;
} *stars = NULL;

static struct starnode_s *lastst;
static int starsp;
struct star_s *new_star(linenum_t line) {
    struct avltree_node *b;
    struct starnode_s *tmp;
    lastst->star.line = line;
    b = avltree_insert(&lastst->node, &((struct starnode_s *)star_tree)->tree, star_compare);
    if (b == NULL) { /* new label */
        avltree_init(&lastst->tree);
        if (starsp == 254) {
            struct stars_s *old = stars;
            new_instance(&stars);
            stars->next = old;
            starsp = 0;
        } else starsp++;
        tmp = lastst;
        lastst = &stars->stars[starsp];
        tmp->star.pass = 0;
        tmp->star.vline = 0;
        return &tmp->star;
    }
    tmp = avltree_container_of(b, struct starnode_s, node);
    if (tmp->star.pass != pass) {
        tmp->star.pass = pass;
        tmp->star.vline = 0;
    }
    return &tmp->star;            /* already exists */
}

static struct starnode_s star_root;

struct star_s *init_star(linenum_t i) {
    struct star_s *s;
    star_tree = &star_root.star;
    s = new_star(i);
    s->addr = 0;
    return s;
}

void destroy_file(void) {
    struct stars_s *old;

    if (file_table.data != NULL) {
        size_t i;
        for (i = 0; i <= file_table.mask; i++) {
            struct file_s *p = file_table.data[i];
            if (p != NULL) file_free(p);
        }
        free(file_table.data);
    }
    free(lastfi);
    free(last_ubuff.data);
    file_free_static(&file_stdin);
    file_free_static(&file_defines);

    while (stars != NULL) {
        old = stars;
        stars = stars->next;
        free(old);
    }
}

void init_file(void) {
    file_table.len = 0;
    file_table.mask = 0;
    file_table.data = NULL;
    file_table.uid = 0;
    file_stdin.name = file_stdin.realname = "-";
    file_defines.name = ""; 
    file_defines.realname = "<command line>";
    new_instance(&stars);
    stars->next = NULL;
    starsp = 0;
    lastst = &stars->stars[starsp];
    new_array(&last_ubuff.data, 16);
    last_ubuff.len = 16;
    lastfi = NULL;
    avltree_init(&star_root.tree);
}

static size_t wrap_print(const char *txt, FILE *f, size_t len) {
    if (len != 0) {
        if (len > 64) {
            fputs(" \\\n", f);
            len = 0;
        }
        len++;
        putc(' ', f);
    }
    return len + makefile_print(txt, f);
}

void makefile(int argc, char *argv[], bool make_phony) {
    FILE *f;
    struct linepos_s nopoint = {0, 0};
    size_t len = 0, j;
    int i, err;

    f = dash_name(arguments.make) ? stdout : fopen_utf8(arguments.make, "wt");
    if (f == NULL) {
        err_msg_file(ERROR_CANT_WRTE_MAK, arguments.make, &nopoint);
        return;
    }
    clearerr(f); errno = 0;
    for (j = 0; j < arguments.output_len; j++) {
        const struct output_s *output = &arguments.output[j];
        if (dash_name(output->name)) continue;
        len = wrap_print(output->name, f, len);
    }
    if (arguments.list.name != NULL) {
        if (!dash_name(arguments.list.name)) {
            len = wrap_print(arguments.list.name, f, len);
        }
    }
    for (j = 0; j < arguments.symbol_output_len; j++) {
        const struct symbol_output_s *output = &arguments.symbol_output[j];
        if (dash_name(output->name)) continue;
        len = wrap_print(output->name, f, len);
    }
    if (len != 0) {
        len++;
        putc(':', f);

        for (i = 0; i < argc; i++) {
            if (dash_name(argv[i])) continue;
            len = wrap_print(argv[i], f, len);
        }

        if (file_table.data != NULL) {
            for (j = 0; j <= file_table.mask; j++) {
                const struct file_s *a = file_table.data[j];
                if (a == NULL) continue;
                if (a->cmdline) continue;
                len = wrap_print(a->realname, f, len);
            }
        }
        putc('\n', f);

        if (file_table.data != NULL && make_phony) {
            len = 0;
            for (j = 0; j <= file_table.mask; j++) {
                const struct file_s *a = file_table.data[j];
                if (a == NULL) continue;
                if (a->cmdline) continue;
                len = wrap_print(a->realname, f, len);
            }
            if (len != 0) fputs(":\n", f);
        }
    }

    err = ferror(f);
    err |= (f != stdout) ? fclose(f) : fflush(f);
    if (err != 0 && errno != 0) err_msg_file(ERROR_CANT_WRTE_MAK, arguments.make, &nopoint);
}
