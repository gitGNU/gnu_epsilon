# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

## Copyright (C) 2002, 2003, 2005, 2006, 2007, 2008 Luca Saiu
## Copyright (C) 2012 Université Paris 13
## minor updates by Luca Saiu in 2004, 2013 and 2014

## Written by Luca Saiu
## (based on old GNU epsilon's configure.ac and NAUSEA's configure.ac,
## by Luca Saiu)

## This file is part of GNU epsilon.

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


## We need to check for Guile features:
m4_include([guile.m4])


# ######################### Global values: begin #######################
AC_INIT([GNU epsilon],
        [bzr-trunk],
        [epsilon-devel@gnu.org],
        [epsilon],
        [http://www.gnu.org/software/epsilon])
AC_PREREQ(2.65)

# Use ./build-aux/ as the directory where to find auxiliary configuration-time
# scripts:
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([build-aux])

# Use ./tests/ as the directory for the (experimental) AutoTest test suite:
#AC_CONFIG_TESTDIR([tests])

# Get the canonical system and target names:
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE
#AM_GNU_GETTEXT
AC_CONFIG_HEADERS([config.h])
AC_PROG_INSTALL

# I like to make canonical system types available to the program:
AC_DEFINE_UNQUOTED(BUILD, "$build",
                   [Canonical system type: build system])
AC_DEFINE_UNQUOTED(BUILD_CPU, "$build_cpu",
                   [Canonical system type: build cpu])
AC_DEFINE_UNQUOTED(BUILD_VENDOR, "$build_vendor",
                   [Canonical system type: build vendor])
AC_DEFINE_UNQUOTED(BUILD_OS, "$build_os",
                   [Canonical system type: build os])
AC_DEFINE_UNQUOTED(HOST, "$host",
                   [Canonical system type: host system])
AC_DEFINE_UNQUOTED(HOST_CPU, "$host_cpu",
                   [Canonical system type: host cpu])
AC_DEFINE_UNQUOTED(HOST_VENDOR, "$host_vendor",
                   [Canonical system type: host vendor])
AC_DEFINE_UNQUOTED(HOST_OS, "$host_os",
                   [Canonical system type: host os])
AC_DEFINE_UNQUOTED(TARGET, "$target",
                   [Canonical system type: target system])
AC_DEFINE_UNQUOTED(TARGET_CPU, "$target_cpu",
                   [Canonical system type: target cpu])
AC_DEFINE_UNQUOTED(TARGET_VENDOR, "$target_vendor",
                   [Canonical system type: target vendor])
AC_DEFINE_UNQUOTED(TARGET_OS, "$target_os",
                   [Canonical system type: target os])

AC_LANG(C)
# First check the C compiler:
AC_PROG_CC
# This seems to be required by other stuff in configure.ac:
AC_USE_SYSTEM_EXTENSIONS

# We also have some assembly files:
AM_PROG_AS

dnl # We need the C source code to know the installation prefix:
dnl if test "$prefix" != NONE; then
dnl    AC_DEFINE_UNQUOTED([PREFIX],
dnl                       "$prefix",
dnl                       [The installation prefix])
dnl else
dnl    AC_DEFINE_UNQUOTED([PREFIX],
dnl                       "/usr/local",
dnl                       [The installation prefix])
dnl fi

# We need to build libraries:
AC_PROG_RANLIB # This is apparently obsolete with new Libtool versions... [2008]

dnl # We want Libtool:
dnl LT_INIT(dlopen)

dnl # In particular, we want ltdl:
dnl LT_CONFIG_LTDL_DIR(libltdl)
dnl LTDL_INIT
dnl #AC_CONFIG_AUX_DIR([libltdl/config])
dnl #AC_CONFIG_MACRO_DIR([libltdl/m4])

dnl #_LT_SET_OPTION([LT_INIT],[dlopen])
dnl #AC_DIAGNOSE([obsolete],[AC_LIBTOOL_DLOPEN: Remove this warning and the call to _LT_SET_OPTION when you
dnl #put the `dlopen' option into LT_INIT's first parameter.])

dnl LT_FUNC_DLSYM_USCORE # define DLSYM_USCORE if we need to prepend _ to dlsym()'d names 
dnl LT_LIB_DLLOAD 
dnl LT_SYS_DLOPEN_SELF
dnl LT_SYS_DLOPEN_DEPLIBS
dnl LT_SYS_MODULE_EXT
dnl LT_SYS_MODULE_PATH
dnl AC_DEFINE_UNQUOTED(LT_MODULE_PATH_VAR,
dnl                    "$LT_MODULE_PATH_VAR",
dnl                    [shell variable determining the search path for dynamic modules])
dnl AC_DEFINE_UNQUOTED(LT_CAN_DLOPEN_SELF,
dnl                    "$enable_dlopen_self",
dnl                    [can a program dlopen() itself?])
dnl AC_DEFINE_UNQUOTED(LT_CAN_STATIC_PROGRAM_DLOPEN_SELF,
dnl                    "$enable_dlopen_self_static",
dnl                    [can a statically-linked program dlopen() itself?])
dnl AC_DEFINE_UNQUOTED(LT_UNDERSCORED_GLOBAL_SYMBOL,
dnl                    "$sys_symbol_underscore",
dnl                    [does the compiler prefix global symbols with '_'?])

# ######################### Global values: end ############################


# ######################### Feature check: begin ##########################

# Check for architecture and compiler features:
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(long double)
AC_C_BIGENDIAN
AC_C_INLINE
AM_PROG_CC_C_O
AC_PROG_CC_C99
AC_C_CHAR_UNSIGNED
AC_C_STRINGIZE

# Be sure make defines the MAKE variable: [To do: do we still need it?]
AC_PROG_MAKE_SET

# Check for Awk:
AC_PROG_AWK

# Check for Lex:
AM_PROG_LEX
if test "$LEX" != flex; then
  AC_MSG_ERROR([ERROR: flex is required.
                You can get it from ftp://ftp.gnu.org/non-gnu/flex])
fi

# Check for Yacc:
AC_PROG_YACC
if test "$YACC" != "bison -y"; then
   AC_MSG_WARN([we suggest to use GNU bison.
                You can get it from ftp://ftp.gnu.org/gnu/bison])
   AC_CHECK_PROG(PLAINOLDYACC, yacc, yes, no)
   if test "$PLAINOLDYACC" == "no"; then
      AC_MSG_ERROR([not even the plain old Yacc was found.
                    Please get and install GNU Bison])
   else
      AC_MSG_WARN([ok, at least the plain old Yacc was found])
      AC_SUBST([YACC], yacc)
   fi
fi

# Checks that required libraries are installed:
AC_LANG(C) # We're looking for C libraries:

AC_CHECK_LIB([m], [ceil], , AC_SUBST([DONTHAVEIT], [yes]))
if test "$DONTHAVEIT" == "yes"; then
  AC_MSG_WARN([This should never happen. Please report to epsilon-devel@gnu.org])
  AC_MSG_ERROR([could not find the m library (for math)])
fi

# We like GNU readline:
AC_CHECK_LIB([ncurses], [main])
AC_CHECK_LIB([termcap], [main])
AC_CHECK_LIB([tinfo], [main])
AC_CHECK_LIB([readline], [readline], , AC_SUBST([DONTHAVEIT], [yes]))
if test "$DONTHAVEIT" == "yes"; then
  AC_MSG_ERROR([GNU readline was not found; please install it, along with its headers.])
fi

# We need pthreads:
AC_CHECK_LIB([pthread], [pthread_mutex_lock], , AC_SUBST([NO_PTHREAD], [yes]))
if test "$NO_PTHREAD" == "yes"; then
  AC_MSG_ERROR([libpthread was not found; you should have it.])
fi

# Check for dlopen() and friends.  To do: I should probably switch to Libtool.
AC_CHECK_LIB([c], [dlopen])
AC_CHECK_LIB([dl], [dlopen])

## Check for the POSIX runtime extension, which can be used for profiling the
## eAM:
#AC_CHECK_LIB([rt], [clock_gettime])

dnl ## Check for the Libtool libdl wrapper:
dnl AC_CHECK_LIB([ltdl],
dnl              [lt_dlopen],
dnl              ,
dnl              AC_MSG_ERROR([ERROR: GNU Libtool is required (also at runtime)]))

#Check for the Boehm GC library:
AC_CHECK_LIB([gc],
             [GC_malloc],
             ,
             AC_MSG_ERROR([The Boehm GC is (currently...) required.]))
# Is the GC new enough?
AC_CHECK_FUNCS([GC_allow_register_threads], , AC_MSG_WARN([Your GC library is old: trying to go on anyway]))

## I use libunistring for Unicode support:
AC_CHECK_LIB([unistring], [ulc_fprintf], , AC_SUBST([NO_UNISTRING], [yes]))
if test "$NO_UNISTRING" == "yes"; then
  AC_MSG_WARN([libunistring was not found; it will be needed in a future version.])
fi
AC_CHECK_HEADERS([unistr.h], , AC_SUBST([NO_UNISTRING_HEADER], [yes]))
if test "$NO_UNISTRING_HEADER" == "yes"; then
  AC_MSG_WARN([libunistring C headers were not found; they will be needed in a future version.])
fi

dnl ??????AC_CHECK_LIB([pthread], [pthread_mutex_lock], , AC_SUBST([NO_PTHREAD], [yes]))
dnl if test "$NO_PTHREAD" == "yes"; then
dnl   AC_MSG_ERROR([libpthread was not found; you should have it.])
dnl fi


## Check for Guile:
GUILE_FLAGS
AC_MSG_CHECKING([whether $GUILE works])
if test "$( $GUILE -q -c '(format #t "~s\n" (+ 1 2))' )" == 3; then
   AC_MSG_RESULT([yes, it seems to work])
else
   AC_MSG_ERROR([Guile does not seem to work])
fi

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

# If the system will be hosted on a supported architecture, then also generate
# a native version.  We're gonna generate support for the particular native
# architecture 
#mkdir -p "runtime/backend-specific"
if test -d "$srcdir/runtime/backend-specific/$host_cpu"; then
   echo "The host architecture \"$host_cpu\" is supported.  Good!"
   echo "Enabling native code support."
   AC_DEFINE_UNQUOTED(NATIVE_CPU,
                      "$host_cpu",
                      [Name of the host architecture if it's supported, otherwise "alwaysfail"])
   # This should be visible to Automake:
   AC_SUBST(NATIVE_CPU, "$host_cpu")
   dnl cd runtime/backend-specific
   dnl ln -sf "$abs_srcdir/runtime/backend-specific/$host_cpu" "native"
   dnl cd ../../
   NATIVE_COMPILING=yes
else
   echo "The host architecture \"$host_cpu\" is not supported."
   echo "Disabling native code support."
   NATIVE_COMPILING=no
   AC_DEFINE_UNQUOTED(NATIVE_CPU,
                      "alwaysfail",
                      [Name of the host architecture if it's supported, otherwise "alwaysfail"])
   # To do: do like in the other branch.  Well, no, I should factorize...
fi
AM_CONDITIONAL([NATIVE_COMPILING], [test "$NATIVE_COMPILING" = "yes"])
# I want to enable -mregnames on PowerPC, because the assembly output it much
# easier to read that way:
AM_CONDITIONAL([COMPILING_FOR_POWERPC], [test "$host_cpu" = "powerpc"])
AM_CONDITIONAL([COMPILING_FOR_MIPS], [echo "$host_cpu" | grep mips &> /dev/null])
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
#                            [enable GNU Common Lisp bindings])
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

# Enable potentially expensive runtime checks and assertions:
# To do: change the default on release
POSITRON_COMMAND_LINE_ENABLE([debug], [DEBUG],
                             [yes],
                             [make potentially expensive integrity checks at runtime])

# Use one bit per object for marking:
POSITRON_COMMAND_LINE_ENABLE([verbose-svm], [VERBOSE_SVM],
                             [no],
                             [compile-in SVM logging (not enabled by default anyway)])

dnl # Dump (quite verbose) page information while running; this is useful for debugging
dnl # the collector:
dnl POSITRON_COMMAND_LINE_ENABLE([dump-page-information], [DUMP_PAGE_INFORMATION],
dnl                              [no],
dnl                              [dump verbose page information while running, for debugging the collector itself])

dnl # Use one bit per object for marking:
dnl POSITRON_COMMAND_LINE_ENABLE([spinlocks], [SPINLOCKS],
dnl                              [yes],
dnl                              [use spinlocks instead of mutexes])

dnl # Use one bit per object for marking:
dnl POSITRON_COMMAND_LINE_ENABLE([mark-bits], [MARK_BITS],
dnl                              [yes],
dnl                              [use one bit per object for marking])

dnl # Use one byte per object for marking:
dnl POSITRON_COMMAND_LINE_ENABLE([mark-bytes], [MARK_BYTES],
dnl                              [no],
dnl                              [use one byte per object for marking])

dnl # Use one word per object for marking:
dnl POSITRON_COMMAND_LINE_ENABLE([mark-words], [MARK_WORDS],
dnl                              [no],
dnl                              [use one word per object for marking (warning: this is very expensive and is only available as a last measure)])

dnl # Sweep backward, from the last object in a page to the first; this should improve
dnl # space locality:
dnl POSITRON_COMMAND_LINE_ENABLE([backward-sweep], [BACKWARD_SWEEP],
dnl                              [yes],
dnl                              [sweep backward, from the last object in a page to the first])

dnl # Mark and sweep in parallel:
dnl POSITRON_COMMAND_LINE_ENABLE([parallel-collection], [PARALLEL_COLLECTION],
dnl                              [yes],
dnl                              [mark and sweep in parallel, from more than one thread])

dnl # Overwrite dead objects with crap at sweep time: # To do: disable this in production
dnl POSITRON_COMMAND_LINE_ENABLE([overwrite-dead-objects], [OVERWRITE_DEAD_OBJECTS],
dnl                              [yes],
dnl                              [overwrite dead objects with crap, which makes debugging easier])

dnl # Destroy completely empty pages not currently owned by any allocator:
dnl POSITRON_COMMAND_LINE_ENABLE([destroy-empty-pages], [DESTROY_EMPTY_PAGES],
dnl                              [no],
dnl                              [destroy completely empty pages not owned by allocators])

dnl # Store human-readable names for thread-related objects, which helps debugging and
dnl # has very little cost:
dnl POSITRON_COMMAND_LINE_ENABLE([thread-object-names], [THREAD_OBJECT_NAMES],
dnl                              [yes],
dnl                              [store human-readable names for thread-related objects])

dnl # Perform sweeping on-demand when possible:
dnl POSITRON_COMMAND_LINE_ENABLE([deferred-sweep], [DEFERRED_SWEEP],
dnl                              [yes],
dnl                              [Perform sweeping on-demand where possible])

dnl # Keep mark arrays out of pages:
dnl POSITRON_COMMAND_LINE_ENABLE([out-of-page-mark-array], [OUT_OF_PAGE_MARK_ARRAY],
dnl                              [no],
dnl                              [keep mark arrays out of pages])

dnl # Support interior pointers:
dnl POSITRON_COMMAND_LINE_ENABLE([interior-pointers], [INTERIOR_POINTERS],
dnl                              [no],
dnl                              [enable tracing of interior pointers])

dnl # Support large objects:
dnl POSITRON_COMMAND_LINE_ENABLE([large-objects], [LARGE_OBJECTS],
dnl                              [no],
dnl                              [enable large object support])

dnl # Support finalization:
dnl POSITRON_COMMAND_LINE_ENABLE([finalization], [FINALIZATION],
dnl                              [no],
dnl                              [enable finalization support])

dnl # Just an example, of course:
dnl POSITRON_COMMAND_LINE_ENABLE([frob-foos], [FROB_FOOS],
dnl                              [no],
dnl                              [frob every single existing foo])

dnl # Enable *very* verbose debug prints:
dnl POSITRON_COMMAND_LINE_ENABLE([verbose-debug], [VERBOSE_DEBUG],
dnl                              [no],
dnl                              [print a lot of debugging information at runtime])

dnl # Prevent the collector to trace free-list pointers by clobbering them:
dnl POSITRON_COMMAND_LINE_ENABLE([conceal-free-list], [CONCEAL_FREE_LIST],
dnl                              [yes],
dnl                              [Prevent the collector to trace free-list pointers by clobbering them])

dnl # Temporary kludge:
dnl POSITRON_COMMAND_LINE_ENABLE([active], [ACTIVELY_WAIT_FOR_THE_SECOND_TIME],
dnl                              [yes],
dnl                              [just a temporary test])

dnl # Perform very aggressive inlining by defining the allocator function, and a couple
dnl # of other performance-critical functions, in public headers so that they can always
dnl # be inlined by callers even out of the library:
dnl POSITRON_COMMAND_LINE_ENABLE([aggressively-inline], [AGGRESSIVELY_INLINE],
dnl                              [yes],
dnl                              [allow the caller to aggressively inline performance-critical code])

dnl # Prefetch the first cache line of the object we are marking:
dnl POSITRON_COMMAND_LINE_ENABLE([prefetch-objects-to-mark], [PREFETCH_OBJECTS_TO_MARK],
dnl                              [no],
dnl                              [prefetch the first cache line of the object we are marking])

dnl # Use mmap() instead of calloc() for allocating mark stack blocks:
dnl POSITRON_COMMAND_LINE_ENABLE([mmap-mark-stack-blocks], [MMAP_MARK_STACK_BLOCKS],
dnl                              [yes],
dnl                              [use mmap() instead of calloc() for allocating mark stack blocks])

dnl # Do load-balancing for parallel marking
dnl POSITRON_COMMAND_LINE_ENABLE([balance-marking-load], [BALANCE_MARKING_LOAD],
dnl                              [yes],
dnl                              [do load-balancing for parallel marking])

dnl # Create implicit kinds
dnl POSITRON_COMMAND_LINE_ENABLE([implicit-kinds], [IMPLICIT_KINDS],
dnl                              [yes],
dnl                              [create implicit kinds])

dnl # This stresses the garbage collector, for debugging it. Performance will be very
dnl # bad, but this should make page changes and collections very frequent, as an
dnl # attempt to make bugs more visible:
dnl POSITRON_COMMAND_LINE_ENABLE([stress-test], [STRESS_TEST],
dnl                              [no],
dnl                              [stress the collector for testing (performance will be very bad)])

# Treat compiler warnings as errors when compiling:
POSITRON_COMMAND_LINE_ENABLE([Werror], [WERROR],
                             [no],
                             [compile with -Werror (mostly for the maintainer)])
if test "$ENABLE_WERROR" != "no"; then
  AC_SUBST(WERROR_OR_NOTHING, "-Werror")
else
  AC_SUBST(WERROR_OR_NOTHING, "")
fi

# Do we have GCC? I like to know whether I can use GCC extensions:
if [[ "$GCC" == "yes" ]]; then
  AC_DEFINE_UNQUOTED(HAVE_GCC, 1,
                     [Can we use GCC extensions?])
fi

# The SVM optionally supports threading dispatch by using the GCC computed
# goto extension. Threading is on by default, but only if using GCC.
if [[ "$GCC" == "yes" ]]; then
  SVM_THREADING_DEFAULT=yes
else
  SVM_THREADING_DEFAULT=no
fi
POSITRON_COMMAND_LINE_ENABLE([svm-threading], [SVM_THREADING],
                             [$SVM_THREADING_DEFAULT],
                             [use threading in the SVM; this only works with GCC])


# We performed some work, we don't want to redo all of it if something fails
# after this point. Checkpoint the configure cache:
AC_CACHE_SAVE
# ######################### Command-line options: end #########################

# ######################### Output config.h: begin ############################
AC_OUTPUT
# ######################### Output config.h: end ##############################
