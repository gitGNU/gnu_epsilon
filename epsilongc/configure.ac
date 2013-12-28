# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

## This file is part of GNU epsilon
## Copyright (C) 2002, 2003, 2005, 2006  Luca Saiu
## minor updates by Luca Saiu in 2004
## (based on GNU epsilon's and NAUSEA's configure.ac, by Luca Saiu)
## Copyright (C) 2012 Université Paris 13
## Written by Luca Saiu

## GNU epsilon is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## GNU epsilon is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU epsilon.  If not, see <http://www.gnu.org/licenses/>.


# ######################### Global values: begin #######################

AC_INIT([GNU epsilon garbage collector],
	[trunk],
        [epsilon-devel@gnu.org],
        [epsilongc])
AC_PREREQ(2.61)

# Use ./build-aux/ as the directory where to find auxiliary scripts:
AC_CONFIG_AUX_DIR([build-aux])

# Use ./tests/ as the directory for the (experimental) AutoTest test suite:
#AC_CONFIG_TESTDIR([tests])

# Get the canonical system and target names:
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE
#AM_GNU_GETTEXT
AM_CONFIG_HEADER([include/epsilongc/config.h])
AC_PROG_INSTALL

AC_LANG(C)
# First check the C compiler:
AC_PROG_CC
# This seems to be required by other stuff in configure.ac:
AC_GNU_SOURCE
AC_USE_SYSTEM_EXTENSIONS

# We need the C source code to know the installation prefix:
if test "$prefix" != NONE; then
   AC_DEFINE_UNQUOTED([PREFIX],
                      "$prefix",
                      [The installation prefix])
else
   AC_DEFINE_UNQUOTED([PREFIX],
                      "/usr/local",
                      [The installation prefix])
fi

# We need to build libraries:
AC_PROG_RANLIB # This is apparently obsolete with new Libtool versions... [2008]
# And we like GNU Libtool, also explicitly dlopen()ing Libtool modules:
#AC_LIBTOOL_DLOPEN
#AC_PROG_LIBTOOL   # This *must* come after AC_LIBTOOL_DLOPEN

# ######################### Global values: end ############################


# ######################### Feature check: begin ##########################

# Check for architecture and compiler features:
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_C_BIGENDIAN
AC_C_INLINE
AM_PROG_CC_C_O
AC_C_STRINGIZE

# Be sure make defines the MAKE variable: [To do: do we still need it?]
AC_PROG_MAKE_SET

# 
dnl AC_DECL_YYTEXT

# Checks that required libraries are installed:
AC_LANG(C) # We're looking for C libraries:

AC_CHECK_LIB([m], [ceil], , AC_SUBST([DONTHAVEIT], [yes]))
if test "$DONTHAVEIT" == "yes"; then
  AC_MSG_WARN([This should never happen. Please report to epsilon-devel@gnu.org])
  AC_MSG_ERROR([could not find the m library (for math)])
fi

# We like GNU readline, and GNU readline depends on an implementation of curses:
#AC_CHECK_LIB([curses], [intrflush])
#AC_CHECK_LIB([readline], [readline])

# We need pthreads:
AC_CHECK_LIB([pthread], [pthread_mutex_lock], , AC_SUBST([NO_PTHREAD], [yes]))
if test "$NO_PTHREAD" == "yes"; then
  AC_MSG_ERROR([libpthread was not found; you should have it.])
fi

## Check for the POSIX runtime extension, which can be used for profiling the
## eAM:
#AC_CHECK_LIB([rt], [clock_gettime])

## Check for the Libtool libdl wrapper:
AC_CHECK_LIB([ltdl],
             [lt_dlopen],
             ,
             AC_MSG_ERROR([ERROR: GNU Libtool is required (also at runtime)]))

## Check for the Boehm GC library (used for comparative benchmarks):
AC_CHECK_LIB([gc],
             [GC_malloc],
             ,
             AC_MSG_ERROR([The Boehm GC is (currently...) required.]))

## Very good, we checked libraries. Now we need to check headers, to make sure
## the user has also them installed:
## To do: add checks for the same libraries' *headers*. It's easy.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h limits.h malloc.h stddef.h stdlib.h string.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_SIZE_T
#AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STRNLEN
AC_CHECK_FUNCS([memalign posix_memalign mmap mmap64])

# We use the "alternative" single-makefile style, as documented in the
# Automake manual:
AC_CONFIG_FILES([Makefile])

# We performed some work, we don't want to redo all of it if something fails
# after this point. Checkpoint the configure cache:
AC_CACHE_SAVE
# ########################## Feature check: end ###############################


# ########################## Command-line options: begin ######################

# A positron-style command-line option allows to set an option with a name,
# default value and help chosen by the caller. An appropriate preprocessor
# symbol is also #define'd in config.h, unless the value is 'no'; this follows
# the Autoconf convention.
#
# Note how POSITRON_COMMAND_LINE_WITH can't do all the work by itself,
# because when the user asks to use a package then configure must somehow
# check for its presence and correctness.
#
# Syntax:
# POSITRON_COMMAND_LINE_ENABLE(option-name, OPTION_NAME, default, help)
# POSITRON_COMMAND_LINE_WITH(package-name, PACKAGE_NAME, default, help)
#
# Example:
# POSITRON_COMMAND_LINE_ENABLE([frob-foos], [FROB_FOOS], [no],
#                              [frob any single existing foo on earth])
# POSITRON_COMMAND_LINE_WITH([gnu-common-lisp], [GNU_COMMON_LISP], [no],
#                            [enable bindings GNU Common Lisp bindings])
AC_DEFUN([POSITRON_COMMAND_LINE_ENABLE], [
AC_ARG_ENABLE([$1],
              AS_HELP_STRING([--enable-$1@<:@=VALUE@:>@],
                             [$4 (default is $3)]),
              ENABLE_$2=$enableval,
              ENABLE_$2=$3)
AC_MSG_NOTICE([The option --enable-$1 was given the value "$ENABLE_$2".])
if test "$ENABLE_$2" != "no"; then
  AC_DEFINE_UNQUOTED(ENABLE_$2, $ENABLE_$2,
                     [Configure option --enable-$1: $4])
fi
])
AC_DEFUN([POSITRON_COMMAND_LINE_WITH], [
AC_ARG_WITH([$1],
            AS_HELP_STRING([--with-$1@<:@=VALUE@:>@],
                           [$4 (default is $3)]),
            WITH_$2=$withval,
            WITH_$2=$3)
AC_MSG_NOTICE([The option --with-$1 was given the value "$WITH_$2".])
if test "$WITH_$2" != "no"; then
  AC_DEFINE_UNQUOTED(WITH_$2, $WITH_$2,
                     [Configure option --with-$1: $4])
fi
])

# Enable expensive runtime assertions:
# To do: change the default on release
POSITRON_COMMAND_LINE_ENABLE([assertions], [ASSERTIONS],
                             [no],
                             [make expensive integrity checks at runtime])

# Dump (quite verbose) page information while running; this is useful for debugging
# the collector:
POSITRON_COMMAND_LINE_ENABLE([dump-page-information], [DUMP_PAGE_INFORMATION],
                             [no],
                             [dump verbose page information while running, for debugging the collector itself])

# Use one bit per object for marking:
POSITRON_COMMAND_LINE_ENABLE([spinlocks], [SPINLOCKS],
                             [yes],
                             [use spinlocks instead of mutexes])

# Use one bit per object for marking:
POSITRON_COMMAND_LINE_ENABLE([mark-bits], [MARK_BITS],
                             [yes],
                             [use one bit per object for marking])

# Use one byte per object for marking:
POSITRON_COMMAND_LINE_ENABLE([mark-bytes], [MARK_BYTES],
                             [no],
                             [use one byte per object for marking])

# Use one word per object for marking:
POSITRON_COMMAND_LINE_ENABLE([mark-words], [MARK_WORDS],
                             [no],
                             [use one word per object for marking (warning: this is very expensive and is only available as a last measure)])

# Sweep backward, from the last object in a page to the first; this should improve
# space locality:
POSITRON_COMMAND_LINE_ENABLE([backward-sweep], [BACKWARD_SWEEP],
                             [yes],
                             [sweep backward, from the last object in a page to the first])

# Mark and sweep in parallel:
POSITRON_COMMAND_LINE_ENABLE([parallel-collection], [PARALLEL_COLLECTION],
                             [yes],
                             [mark and sweep in parallel, from more than one thread])

# Overwrite dead objects with crap at sweep time: # To do: disable this in production
POSITRON_COMMAND_LINE_ENABLE([overwrite-dead-objects], [OVERWRITE_DEAD_OBJECTS],
                             [yes],
                             [overwrite dead objects with crap, which makes debugging easier])

# Destroy completely empty pages not currently owned by any allocator:
POSITRON_COMMAND_LINE_ENABLE([destroy-empty-pages], [DESTROY_EMPTY_PAGES],
                             [no],
                             [destroy completely empty pages not owned by allocators])

# Store human-readable names for thread-related objects, which helps debugging and
# has very little cost:
POSITRON_COMMAND_LINE_ENABLE([thread-object-names], [THREAD_OBJECT_NAMES],
                             [yes],
                             [store human-readable names for thread-related objects])

# Perform sweeping on-demand when possible:
POSITRON_COMMAND_LINE_ENABLE([deferred-sweep], [DEFERRED_SWEEP],
                             [yes],
                             [Perform sweeping on-demand where possible])

# Keep mark arrays out of pages:
POSITRON_COMMAND_LINE_ENABLE([out-of-page-mark-array], [OUT_OF_PAGE_MARK_ARRAY],
                             [no],
                             [keep mark arrays out of pages])

# Support interior pointers:
POSITRON_COMMAND_LINE_ENABLE([interior-pointers], [INTERIOR_POINTERS],
                             [no],
                             [enable tracing of interior pointers])

# Support large objects:
POSITRON_COMMAND_LINE_ENABLE([large-objects], [LARGE_OBJECTS],
                             [no],
                             [enable large object support])

# Support finalization:
POSITRON_COMMAND_LINE_ENABLE([finalization], [FINALIZATION],
                             [no],
                             [enable finalization support])

# Just an example, of course:
POSITRON_COMMAND_LINE_ENABLE([frob-foos], [FROB_FOOS],
                             [no],
                             [frob every single existing foo])

# Enable *very* verbose debug prints:
POSITRON_COMMAND_LINE_ENABLE([verbose-debug], [VERBOSE_DEBUG],
                             [no],
                             [print a lot of debugging information at runtime])

# Prevent the collector to trace free-list pointers by clobbering them:
POSITRON_COMMAND_LINE_ENABLE([conceal-free-list], [CONCEAL_FREE_LIST],
                             [yes],
                             [Prevent the collector to trace free-list pointers by clobbering them])

# Temporary kludge:
POSITRON_COMMAND_LINE_ENABLE([active], [ACTIVELY_WAIT_FOR_THE_SECOND_TIME],
                             [yes],
                             [just a temporary test])

# Perform very aggressive inlining by defining the allocator function, and a couple
# of other performance-critical functions, in public headers so that they can always
# be inlined by callers even out of the library:
POSITRON_COMMAND_LINE_ENABLE([aggressively-inline], [AGGRESSIVELY_INLINE],
                             [yes],
                             [allow the caller to aggressively inline performance-critical code])

# Prefetch the first cache line of the object we are marking:
POSITRON_COMMAND_LINE_ENABLE([prefetch-objects-to-mark], [PREFETCH_OBJECTS_TO_MARK],
                             [no],
                             [prefetch the first cache line of the object we are marking])

# Use mmap() instead of calloc() for allocating mark stack blocks:
POSITRON_COMMAND_LINE_ENABLE([mmap-mark-stack-blocks], [MMAP_MARK_STACK_BLOCKS],
                             [yes],
                             [use mmap() instead of calloc() for allocating mark stack blocks])

# Do load-balancing for parallel marking
POSITRON_COMMAND_LINE_ENABLE([balance-marking-load], [BALANCE_MARKING_LOAD],
                             [yes],
                             [do load-balancing for parallel marking])

# Create implicit kinds
POSITRON_COMMAND_LINE_ENABLE([implicit-kinds], [IMPLICIT_KINDS],
                             [yes],
                             [create implicit kinds])

# This stresses the garbage collector, for debugging it. Performance will be very
# bad, but this should make page changes and collections very frequent, as an
# attempt to make bugs more visible:
POSITRON_COMMAND_LINE_ENABLE([stress-test], [STRESS_TEST],
                             [no],
                             [stress the collector for testing (performance will be very bad)])

# Treat compiler warnings as errors when compiling:
POSITRON_COMMAND_LINE_ENABLE([Werror], [WERROR],
                             [no],
                             [compile with -Werror (mostly for the maintainer)])
if test "$ENABLE_WERROR" != "no"; then
  AC_SUBST(WERROR_OR_NOTHING, "-Werror")
else
  AC_SUBST(WERROR_OR_NOTHING, "")
fi

# We performed some work, we don't want to redo all of it if something fails
# after this point. Checkpoint the configure cache:
AC_CACHE_SAVE
# ######################### Command-line options: end #########################


# ######################### Output config.h: begin ############################
AC_OUTPUT
# ######################### Output config.h: end ##############################
