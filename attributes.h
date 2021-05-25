/*
    $Id: attributes.h 2673 2021-05-16 05:10:23Z soci $

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
#ifndef ATTRIBUTES_H
#define ATTRIBUTES_H

#ifdef __has_attribute
#elif defined __GNUC__
# define __has_attribute(a) __has_attribute ## a
# define __has_attribute__unused__ (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7))
# define __has_attribute__warn_unused_result__ (__GNUC__ >= 4)
# define __has_attribute__malloc__ (__GNUC__ >= 3)
# define __has_attribute__noreturn__ (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 95))
# define __has_attribute__regparm__ (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 95))
# define __has_attribute__noinline__ (__GNUC__ >= 3)
# define __has_attribute__fallthrough__ (__GNUC__ >= 7)
#else
# define __has_attribute(a) (0)
#endif

#if __has_attribute(__unused__)
# define UNUSED(x) UNUSED_ ## x __attribute__((__unused__))
#else
# define UNUSED(x) x
#endif

#if __has_attribute(__warn_unused_result__)
# define MUST_CHECK __attribute__((__warn_unused_result__))
#else
# define MUST_CHECK
#endif

#if __has_attribute(__malloc__)
# if __has_attribute(__warn_unused_result__)
#  define MALLOC __attribute__((__malloc__,__warn_unused_result__))
# else
#  define MALLOC __attribute__((__malloc__))
# endif
#elif __has_attribute(__warn_unused_result__)
#  define MALLOC __attribute__((__warn_unused_result__))
#else
# define MALLOC
#endif

#if __has_attribute(__noreturn__)
# define NO_RETURN __attribute__((__noreturn__))
#else
# define NO_RETURN
#endif

#if __has_attribute(__regparm__) && defined __i386__
# define FAST_CALL __attribute__((__regparm__(3)))
#else
# define FAST_CALL
#endif

#if __has_attribute(__noinline__)
# define NO_INLINE __attribute__((__noinline__))
#else
# define NO_INLINE
#endif

#if defined __cplusplus || (defined __STDC_VERSION__ && __STDC_VERSION__ >= 199901L)
#elif defined __GNUC__ || (defined _MSC_VER && _MSC_VER >= 900)
# define inline __inline
#else
# define inline
#endif

#if __has_attribute(__fallthrough__) && (!defined __clang__ || __clang_major__ >= 5)
# define FALL_THROUGH __attribute__((__fallthrough__))
#else
# define FALL_THROUGH do {} while (false)
#endif

#ifdef __has_builtin
#elif defined __GNUC__
# define __has_builtin(a) __has_builtin ## a
# define __has_builtin__builtin_add_overflow (__GNUC__ > 5)
# define __has_builtin__builtin_mul_overflow (__GNUC__ > 5)
# define __has_builtin__builtin_expect (__GNUC__ > 2)
#else
# define __has_builtin(a) (0)
#endif

#if __has_builtin(__builtin_add_overflow)
# define add_overflow(a, b, c) __builtin_add_overflow(a, b, c)
# define inc_overflow(a, b) __builtin_add_overflow(*a, b, a)
#else
# define add_overflow(a, b, c) ((*(c) = (a) + (b)) < (b))
# define inc_overflow(a, b) ((*(a) = *(a) + (b)) < (b))
#endif

#if __has_builtin(__builtin_expect)
# define likely(a)      (__builtin_expect(!!(a), 1))
# define unlikely(a)    (__builtin_expect(!!(a), 0))
#else
# define likely(a)      (a)
# define unlikely(a)    (a)
#endif

#endif
