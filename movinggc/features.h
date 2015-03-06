/* This file is part of GNU epsilon.

   Copyright (C) 2012 Université Paris 13
   Updated in 2015 by Luca Saiu
   Written by Luca Saiu

   GNU epsilon is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   GNU epsilon is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU epsilon.  If not, see <http://www.gnu.org/licenses/>. */


#ifndef MOVINGGC_FEATURES_H_
#define MOVINGGC_FEATURES_H_

#if 0
// Horrible kludges to make the gprof output more useful:
#define __attribute__(...)  /* nothing */
#define static  /* nothing */
#define inline  /* nothing */
#endif // #if 0

//#define MOVINGGC_DEBUG
#define MOVINGGC_VERBOSE
//#define MOVINGGC_VERY_VERBOSE

#define MOVINGGC_USE_MEMCPY

#define MOVINGGC_USE_GLOBAL_POINTERS
#define MOVINGGC_USE_REGISTER_POINTERS

#ifdef MOVINGGC_USE_REGISTER_POINTERS
#if defined(MOVINGGC_ARCHITECTURE_i686) \
    || defined(MOVINGGC_ARCHITECTURE_i686_AT386 /* for the Hurd */)
// x86: We can use %esi, %ebx and %edi
#define MOVINGGC_REGISTER_1 "esi"
#define MOVINGGC_REGISTER_2 "ebx"
#define MOVINGGC_REGISTER_3 "edi"
#elif defined(MOVINGGC_ARCHITECTURE_x86_64)
// x86_64: We can use %rbp, %rbx, %r12, %r13, %r14, %r15.
#define MOVINGGC_REGISTER_1 "%r13"
#define MOVINGGC_REGISTER_2 "%r14"
#define MOVINGGC_REGISTER_3 "%r15"
#define MOVINGGC_REGISTER_4 "%r12"
#elif defined(MOVINGGC_ARCHITECTURE_ppc)
// ppc: We can use ...?
#define MOVINGGC_REGISTER_1 "??"
#define MOVINGGC_REGISTER_2 "??"
#define MOVINGGC_REGISTER_3 "??"
// MIPS: $16-$23, $30
#elif defined(MOVINGGC_ARCHITECTURE_mips) || defined(MOVINGGC_ARCHITECTURE_mips64)
#define MOVINGGC_REGISTER_1 "%20"
#define MOVINGGC_REGISTER_2 "%21"
#define MOVINGGC_REGISTER_3 "%22"
#define MOVINGGC_REGISTER_4 "%23"
#define MOVINGGC_REGISTER_5 "%30"
#else
#error Unknown architecture: can not use register pointers
#endif // ... architecture-specific code
register void ** movinggc_fromspace_next_unallocated_word
asm (MOVINGGC_REGISTER_1);
register void ** movinggc_fromspace_after_payload_end
asm (MOVINGGC_REGISTER_2);
#endif

#if defined(MOVINGGC_USE_REGISTER_POINTERS) && !defined(MOVINGGC_USE_GLOBAL_POINTERS)
#error Register pointers can only be enabled with global pointers
#endif

#endif // #ifndef MOVINGGC_FEATURES_H_
