## Process this file with automake to produce Makefile.in -*-Makefile-*-

## This file is part of GNU epsilon
## Copyright (C) 2006, 2007, 2008, 2013, 2014, 2015, 2016 Luca Saiu
## (based on the eAM and NAUSEA Makefile.am files, both by Luca Saiu)
## Copyright (C) 2012 Université Paris 13
## Copyright (C) 2012 Luca Saiu

## Written by Luca Saiu

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


## This Automake template follows the "Alternative Approach to Subdirectories".
## See the GNU Automake manual for more information.


## ##################################################### installation paths: begin
## Define Automake prefixes to the primaries I need.

# epsilon dumps files are installed as edump_pkgdatadir_DATA.  I'm using
# pkgdatadir instead of pkglibdir because epsilon dumps are supposed to be
# architecture-agnostic.
esrc_pkgdatadir = $(pkgdatadir)/src

# epsilon source files are installed as esrc_pkgdata_DATA.  Sources are data as
# well for Automake.
edump_pkgdatadir = $(pkgdatadir)/dumps
## ##################################################### installation paths: end

nobase_nodist_data_DATA =
EXTRA_DIST =
data_DATA =
nodist_data_DATA =
AM_LDFLAGS =
AUTOMAKE_OPTIONS = subdir-objects

AM_YFLAGS = -d
AM_LFLAGS = -s

AM_CFLAGS = -Wall
AM_CFLAGS += @WERROR_OR_NOTHING@

BUILT_SOURCES =
nodist_BUILT_SOURCES =
nodist_nobase_BUILT_SOURCES =
dist_noinst_SCRIPTS =
dist_pkgdata_DATA =
edump_pkgdata_DATA =
esrc_pkgdata_DATA =

#AM_CFLAGS += -pthread
#AM_CFLAGS += -static
#AM_CFLAGS += -static-libgcc
#AM_LDFLAGS += /usr/lib/libc_nonshared.a

# Optimization:
#AM_CFLAGS += -fomit-frame-pointer -O3
#AM_CFLAGS += -foptimize-sibling-calls
#AM_CFLAGS += -funroll-all-loops -finline-functions

#AM_CFLAGS += -fno-inline-functions

#AM_CFLAGS += -O0 -g

#AM_CFLAGS += -I @srcdir@/include/epsilon -I .

# GCov support:
#AM_CFLAGS += -fprofile-arcs -ftest-coverage

# GProf support:
#AM_CFLAGS += -fPIC -fpic
#AM_CFLAGS += -pg
#AM_LDFLAGS += /usr/lib/libdl_p.a /usr/lib/libpthread_p.a /usr/lib/libc_p.a
#AM_CFLAGS += -static
#AM_CFLAGS += -O2 -fno-unroll-loops -fno-inline-functions -fno-optimize-sibling-calls

#AM_CFLAGS += -g
#AM_CFLAGS += -save-temps

# Make generated assembly easier to read:
AM_CFLAGS += -fverbose-asm
if COMPILING_FOR_POWERPC
  AM_CFLAGS += -mregnames
endif

# # To do: this is good for threading (only)
# AM_CFLAGS += -fno-gcse

# # More crazyness:
# AM_CFLAGS += -funsafe-loop-optimizations

# # This wins on the threaded SVM:
# AM_CFLAGS += -fno-crossjumping

# To do: this is experimental, of course, but it actually looks like a good idea in the threaded interpreter:
#AM_CFLAGS += --param max-goto-duplication-insns=1000

# To do: this is experimental. Inline like crazy (100x the default values):
#AM_CFLAGS += --param max-inline-insns-single=45000 --param max-inline-insns-auto=9000 --param large-function-insns=270000
#AM_CFLAGS += --param large-function-growth=10000 --param large-stack-frame=25600 --param large-stack-frame-growth=100000 
#AM_CFLAGS += --param max-jump-thread-duplication-stmts=10000
#AM_CFLAGS += -finline-limit=10000000

# # I should use Libtool instead of this, which is most likely not portable. This is needed on GNU/Linux
# # for executables dlopen()ing their own functions:
# AM_CFLAGS += -rdynamic

AM_CFLAGS += @GUILE_CFLAGS@
#AM_CFLAGS += -p -pg
#AM_CFLAGS += -O3 -funroll-loops -finline-functions
#AM_CFLAGS += -fno-strict-aliasing
#AM_CFLAGS += -rdynamic

#AM_LDFLAGS += -export-dynamic -rdynamic -dlopen self @GUILE_LDFLAGS@
AM_LDFLAGS += @GUILE_LDFLAGS@

## A variant of what is suggested by libtoolize:
#ACLOCAL_AMFLAGS = -I build-aux

# This is needed for config.h
AM_CFLAGS += -I @builddir@

AM_MAKEINFOFLAGS = --no-split
AM_MAKEINFOHTMLFLAGS = --no-split --number-sections

EXTRA_DIST += \
  BUGS NO-WARRANTY doc/COPYING.DOC PLATFORMS TODO \
  svm/examples/memory.s \
  svm/examples/memory-2.s \
  svm/examples/simple.s \
  svm/examples/c.s \
  svm/examples/euclid.s \
  svm/examples/float.s \
  svm/examples/q.s \
  svm/examples/euclid-c.c \
  runtime/backend-specific/x86_64/backend-specific-c.c \
  runtime/backend-specific/x86_64/backend-specific-s.S \
  runtime/backend-specific/mips/backend-specific-c.c \
  runtime/backend-specific/mips/backend-specific-s.S \
  runtime/backend-specific/c64/runtime.a \
  runtime/backend-specific/always-fail runtime/backend-specific/__disabled__/i386 runtime/backend-specific/__disabled__/i486 runtime/backend-specific/__disabled__/i586 runtime/backend-specific/__disabled__/i686 runtime/backend-specific/mips runtime/backend-specific/mips64 runtime/backend-specific/mips64el runtime/backend-specific/mipsel runtime/backend-specific/__disabled__/powerpc runtime/backend-specific/__disabled__/ppc runtime/backend-specific/__disabled__/x86

# Source files which are *not* dumped into unexeced images, so that
# they can be modified without having to bootstrap.  These are just
# loaded every time at the end of the startup process from guile+whatever.
EPSILON_UNDUMPED_SOURCE_FILES = \
  bootstrap/scheme/scratch.e \
  bootstrap/scheme/gc-tests.e \
  bootstrap/scheme/brainfuck.e \
  bootstrap/scheme/peano.e

FIXED_POINT_TABLES = @top_builddir@/bootstrap/scheme/fixed-point-tables.e
$(FIXED_POINT_TABLES): $(srcdir)/bootstrap/generate-fixedpoint-tables
	echo "Generating number tables for fixed-point procedures..."
	$(srcdir)/bootstrap/generate-fixedpoint-tables > $@
	echo "  ...done."
EXTRA_DIST += \
  bootstrap/generate-fixedpoint-tables

EPSILON_EARLY_DUMPED_NON_GENERATED_SOURCE_FILES = \
  bootstrap/scheme/bootstrap-from-guile.scm \
  bootstrap/scheme/epsilon0-in-scheme.scm \
  bootstrap/scheme/debugging-stuff-for-guile.scm \
  bootstrap/scheme/conversion.scm \
  bootstrap/scheme/core.e \
  bootstrap/scheme/unexec.e \
  bootstrap/scheme/fill-reflective-structures.scm \
  bootstrap/scheme/toplevel-in-scheme.scm \
  bootstrap/scheme/epsilon1.scm
EPSILON_EARLY_DUMPED_SOURCE_FILES = \
  $(EPSILON_EARLY_DUMPED_NON_GENERATED_SOURCE_FILES) \
  @top_builddir@/bootstrap/scheme/configuration.e

EPSILON_LATE_DUMPED_NON_GENERATED_SOURCE_FILES = \
  bootstrap/scheme/unexec-repl.e \
  bootstrap/scheme/repl.e \
  bootstrap/scheme/fixed-point.e \
  bootstrap/scheme/analyses-and-optimizations.e \
  bootstrap/scheme/compiler.e \
  bootstrap/scheme/export-toplevel-forms-to-guile.scm \
  bootstrap/scheme/bootstrap-from-epsilon1.e \
  bootstrap/scheme/bootstrap-from-epsilon1-secondary.e \
  bootstrap/scheme/check-for-spills.e
EPSILON_LATE_DUMPED_SOURCE_FILES = \
  $(EPSILON_LATE_DUMPED_NON_GENERATED_SOURCE_FILES) \
  $(FIXED_POINT_TABLES)

EPSILON_DUMPED_SOURCE_FILES = \
  $(EPSILON_EARLY_DUMPED_SOURCE_FILES) \
  $(EPSILON_LATE_DUMPED_SOURCE_FILES)

EPSILON_NON_GENERATED_SOURCE_FILES = \
  $(EPSILON_EARLY_DUMPED_NON_GENERATED_SOURCE_FILES) \
  $(EPSILON_LATE_DUMPED_NON_GENERATED_SOURCE_FILES) \
  $(EPSILON_UNDUMPED_SOURCE_FILES)

EPSILON_SOURCE_FILES = \
  $(EPSILON_EARLY_DUMPED_SOURCE_FILES) \
  $(EPSILON_LATE_DUMPED_SOURCE_FILES) \
  $(EPSILON_UNDUMPED_SOURCE_FILES)

EXTRA_DIST += \
  $(EPSILON_NON_GENERATED_SOURCE_FILES)

## Some files are only useful to manually re-generate some code in core.e,
## and are not automatically called by the build system.
EXTRA_DIST += \
  bootstrap/scheme/generate-sum-type-from-scheme.scm \
  bootstrap/COMPRESS-WHITESPACE

## Yes, no subdirs. Both easier and faster; and also more correct with respect
## to dependencies. Ha.
SUBDIRS =

## Let's give an initial value to some variables, so that we can safely use
## += later:
noinst_PROGRAMS =
bin_PROGRAMS =
lib_LIBRARIES =
lib_LTLIBRARIES =
noinst_LTLIBRARIES =
#nobase_dist_hdir_include_HEADERS =
# nobase_dist_hdir_include_HEADERS += \
#   @srcdir@/include/epsilongc/allocator.h \
#   @srcdir@/include/epsilongc/compile_time_parameters.h \
#   @srcdir@/include/epsilongc/epsilongc_threads.h \
#   @srcdir@/include/epsilongc/global_structures.h \
#   @srcdir@/include/epsilongc/epsilongc_debug.h \
#   @srcdir@/include/epsilongc/epsilongc_debugging_hack.h \
#   @srcdir@/include/epsilongc/epsilongc_features.h \
#   @srcdir@/include/epsilongc/heuristics.h \
#   @srcdir@/include/epsilongc/run_time_settings.h \
#   @srcdir@/include/epsilongc/kind.h \
#   @srcdir@/include/epsilongc/page.h \
#   @srcdir@/include/epsilongc/pool.h \
#   @srcdir@/include/epsilongc/roots.h \
#   @srcdir@/include/epsilongc/set_of_pages.h \
#   @srcdir@/include/epsilongc/trace.h \
#   @srcdir@/include/epsilongc/epsilongc_macros.h \
#   @srcdir@/include/epsilongc/epsilongc_types.h \
#   @srcdir@/include/epsilongc/doubly_linked_list_macros.h \
#   @srcdir@/include/epsilongc/fatal.h \
#   @srcdir@/include/epsilongc/malloc.h \
#   @srcdir@/include/epsilongc/time.h \
#   @srcdir@/include/epsilongc/epsilongc.h \
#   @srcdir@/include/epsilongc/large.h \
#   @srcdir@/include/epsilongc/myrand.h \
#   @srcdir@/include/epsilongc/declarationstoinline.h \
#   @srcdir@/include/epsilongc/definitionstoinline.h \
#   include/epsilongc/config.h

#nobase_nodist_hdir_include_HEADERS =

## ##################################################### commodity scripts: begin
dist_noinst_SCRIPTS += \
  svm/generate-c-support \
  autogen.sh unautogen.sh
## ##################################################### commodity scripts: end

## ##################################################### not for the user: begin
# Regenerate Emacs tags:
TAGS:
	etags `find -name \*.c` `find -name \*.h` \
	      `find -name \*.scm` \
	      `find -name \*.l` `find -name \*.y` \
	      `find -name \*.cc` `find -name \*.hh` \
	      `find -name \*.ll` `find -name \*.yy`

# Count the source lines:
count-lines: clean
	@echo "=============================================================="
	@echo "This count includes C source files, Lex and Yacc source files,"
	@echo "Scheme files and makefiles."
	@echo "- Automatically generated source files are *not* included."
	@echo "- Documentation is *not* included (of course)."
	@echo "- Comment lines *are* counted."
	@echo
	@echo -n "Source lines:           "
	@cat */*.[chly] Makefile.am configure.ac | wc -l
	@echo -n "Non-empty source lines:"
#       This is quite fun:
	@echo " " | (cat */*.[chly]* Makefile.am configure.ac -) \
	          | nl --body-numbering=t | tail --lines=1
	@echo "=============================================================="
## ##################################################### not for the user: begin


## ####################################################### libraries: begin
## Configuration info:
#nobase_nodist_hdir_include_HEADERS += \
#  config.h

lib_LIBRARIES += lib/libepsilonutility.a
lib_libepsilonutility_a_SOURCES = \
  utility/utility.c utility/utility.h \
  utility/types.h \
  utility/hints.h \
  utility/debug.h \
  utility/malloc.c utility/malloc.h \
  utility/fatal.c utility/fatal.h \
  utility/dynamic-array.c utility/dynamic-array.h \
  utility/list.c utility/list.h \
  utility/unboxed-hash.c utility/unboxed-hash.h \
  utility/string-hash.c utility/string-hash.h \
  utility/stack.c utility/stack.h \
  config.h

RUNTIME_SOURCES = \
  runtime/interface-with-c.c runtime/interface-with-c.h \
  runtime/backend-specific.h \
  runtime/data.c runtime/data.h \
  runtime/marshal.c runtime/marshal.h \
  runtime/c-primitives.c runtime/c-primitives.h \
  runtime/epsilon0-interpreter.c runtime/epsilon0-interpreter.h \
  runtime/thread-context.c runtime/thread-context.h \
  runtime/runtime.c runtime/runtime.h \
  runtime/jit.c runtime/jit.h \
  config.h

lib_LIBRARIES += lib/libepsilonruntime-untagged.a
lib_libepsilonruntime_untagged_a_SOURCES = $(RUNTIME_SOURCES)
lib_libepsilonruntime_untagged_a_CPPFLAGS = -DEPSILON_RUNTIME_UNTAGGED

lib_LIBRARIES += lib/libepsilonruntime-tagged.a
lib_libepsilonruntime_tagged_a_SOURCES = $(RUNTIME_SOURCES)
lib_libepsilonruntime_tagged_a_CPPFLAGS = -DEPSILON_RUNTIME_TAGGED

lib_LIBRARIES += lib/libepsilonruntime-smob.a
lib_libepsilonruntime_smob_a_SOURCES = $(RUNTIME_SOURCES)
lib_libepsilonruntime_smob_a_CPPFLAGS = -DEPSILON_RUNTIME_SMOB

lib_LIBRARIES += lib/libepsilonruntime-tagged-egc.a
lib_libepsilonruntime_tagged_egc_a_SOURCES = $(RUNTIME_SOURCES)
lib_libepsilonruntime_tagged_egc_a_SOURCES += \
  movinggc/movinggc.c movinggc/movinggc.h movinggc/features.h
lib_libepsilonruntime_tagged_egc_a_CPPFLAGS = -DEPSILON_RUNTIME_TAGGED -DEPSILON_EGC

lib_LIBRARIES += lib/libepsilondriver-c-untagged.a
lib_libepsilondriver_c_untagged_a_SOURCES = \
  runtime/backend-specific/c/backend-specific-c.c \
  driver/driver.c
lib_libepsilondriver_c_untagged_a_LIBADD = \
  lib/libepsilonruntime-untagged.a lib/libepsilonutility.a
# # FIXME: It was like this, with the SMOB runtime.  I strongly suspect that was a mistake.
# lib_libepsilondriver_c_untagged_a_LIBADD = \
#   lib/libepsilonruntime-smob.a lib/libepsilonutility.a

lib_LIBRARIES += lib/libepsilondriver-native-untagged.a
lib_libepsilondriver_native_untagged_a_SOURCES = \
  driver/driver.c
if NATIVE_COMPILING
lib_libepsilondriver_native_untagged_a_SOURCES += \
  runtime/backend-specific/@NATIVE_CPU@/backend-specific-c.c \
  runtime/backend-specific/@NATIVE_CPU@/backend-specific-s.S
else
lib_libepsilondriver_native_untagged_a_SOURCES += \
  runtime/backend-specific/always-fail/backend-specific-c.c
endif

lib_LIBRARIES += lib/libepsilondriver-native-tagged-egc.a
lib_libepsilondriver_native_tagged_egc_a_SOURCES = \
  driver/driver.c
if NATIVE_COMPILING
lib_libepsilondriver_native_tagged_egc_a_SOURCES += \
  runtime/backend-specific/@NATIVE_CPU@/backend-specific-c.c \
  runtime/backend-specific/@NATIVE_CPU@/backend-specific-s.S
else
lib_libepsilondriver_native_tagged_egc_a_SOURCES += \
  runtime/backend-specific/always-fail/backend-specific-c.c
endif
lib_libepsilondriver_native_tagged_egc_a_CPPFLAGS = -DEPSILON_RUNTIME_TAGGED -DEPSILON_EGC
## ####################################################### libraries: end


## ##################################################### GNU epsilon executables: begin
# One of these is conditionally #include'd by runtime/data.c:
EXTRA_DIST += \
  runtime/data-tagged.c \
  runtime/data-untagged.c \
  runtime/data-smob.c

# EXTRA_DIST += \
#   repl

# A little sanity test for the utility functions:
bin_PROGRAMS += \
  bin/epsilon-test-utility
bin_epsilon_test_utility_SOURCES = \
  utility/test.c
bin_epsilon_test_utility_LDADD = \
  lib/libepsilonutility.a
bin_epsilon_test_utility_CPPFLAGS = -DEPSILON_RUNTIME_UNTAGGED

# A temporary utility for testing and developing threaded code.
bin_PROGRAMS += \
  bin/epsilon-test-jit-untagged
bin_epsilon_test_jit_untagged_SOURCES = \
  runtime/test-jit.c
bin_epsilon_test_jit_untagged_LDADD = \
  lib/libepsilonruntime-untagged.a lib/libepsilonutility.a
bin_epsilon_test_jit_untagged_CPPFLAGS = -DEPSILON_RUNTIME_UNTAGGED

bin_PROGRAMS += \
  bin/epsilon-test-jit-tagged
bin_epsilon_test_jit_tagged_SOURCES = \
  runtime/test-jit.c
bin_epsilon_test_jit_tagged_LDADD = \
  lib/libepsilonruntime-tagged.a lib/libepsilonutility.a
bin_epsilon_test_jit_tagged_CPPFLAGS = -DEPSILON_RUNTIME_TAGGED

# Currently unused
bin_PROGRAMS += \
  bin/epsilon-svm
bin_epsilon_svm_CPPFLAGS = -DEPSILON_RUNTIME_UNTAGGED

# To do: this improves the threaded code performance, but it should be
# enabled only for interpreter.c and only when threading is also enabled.
# See the Automake documentation for how to use mumble_CFLAGS; here the
# general idea will be to use convenience libraries; possibly with
# Libtool, as I guess I will really need it in the future...
#bin_epsilon_svm_CFLAGS = $(AM_CFLAGS) -fno-gcse
# FIXME: should the runtime c-wrapper sources be part of the runtime?  I have to rethink this part
#bin_epsilon_svm_SOURCES += \
#  runtime/c-wrapper.c runtime/c-wrapper.h BACKSLASH...
bin_epsilon_svm_SOURCES = \
  runtime/c-wrapper.c runtime/c-wrapper.h \
  svm/instructions.c \
  svm/instructions.h \
  svm/interpreter.c \
  svm/interpreter.h \
  svm/registers.c \
  svm/registers.h \
  svm/svm.h \
  svm/frontend.c \
  svm/frontend.h \
  svm/scanner.c \
  svm/parser.c \
  svm/parser.h \
  svm/svm.c \
  svm/useful-primitives-in-c.c \
  runtime/backend-specific/svm/backend-specific-c.c \
  runtime/backend-specific/svm/backend-specific-s.S
bin_epsilon_svm_LDADD = \
  lib/libepsilonruntime-smob.a lib/libepsilonutility.a
BUILT_SOURCES += \
  svm/parser.y \
  svm/scanner.l \
  svm/scanner.c \
  svm/parser.c \
  svm/parser.h \
  svm/c-instructions.c \
  svm/c-instructions.h
EXTRA_DIST += \
  svm/parser.y-template \
  svm/scanner.l-template \
  svm/parser.y \
  svm/scanner.l \
  svm/c-instructions.c \
  svm/c-instructions.h
$(srcdir)/svm/parser.c: svm/parser.y
	$(YACC) -p svm_ -o $@ $(AM_YFLAGS) $^
$(srcdir)/svm/parser.h: svm/parser.c
	true # Do nothing: this is generated together with its companion .c file
$(srcdir)/svm/scanner.c: svm/scanner.l
	$(LEX) -Psvm_ -o$@ $(AM_LFLAGS) $^
$(srcdir)/svm/parser.y: svm/parser.y-template svm/scanner.l-template svm/generate-c-support
	$(srcdir)/svm/generate-c-support
$(srcdir)/svm/scanner.l: svm/parser.y
$(srcdir)/svm/interpreter.c: svm/parser.y
$(srcdir)/svm/instructions.h: svm/parser.y
$(srcdir)/svm/c-instructions.c: svm/parser.y
$(srcdir)/svm/c-instructions.h: svm/parser.y
$(srcdir)/svm/instructions.h: svm/parser.y
$(srcdir)/svm/instructions.c: svm/parser.y
$(srcdir)/svm/interpreter.c: svm/parser.y

bin_PROGRAMS += \
  bin/epsilon-svm-config
bin_epsilon_svm_config_SOURCES = \
  svm/svm-config.c \
  config.h
bin_epsilon_svm_config_CPPFLAGS = -DEPSILON_RUNTIME_UNTAGGED

bin_PROGRAMS += \
  bin/guile+whatever
bin_guile_whatever_SOURCES = \
  bootstrap/guile+whatever/guile+whatever.c
bin_guile_whatever_LDADD = \
  lib/libepsilonruntime-smob.a lib/libepsilonutility.a

bin_PROGRAMS += bin/epsilon-image-interpreter-untagged
bin_epsilon_image_interpreter_untagged_SOURCES = \
  image-interpreter/image-interpreter.c
bin_epsilon_image_interpreter_untagged_LDADD = \
  lib/libepsilonruntime-untagged.a lib/libepsilonutility.a
# FIXME: this has to go:
bin_epsilon_image_interpreter_untagged_CPPFLAGS = -DEPSILON_RUNTIME_UNTAGGED

bin_PROGRAMS += bin/epsilon-image-interpreter-tagged
bin_epsilon_image_interpreter_tagged_SOURCES = \
  image-interpreter/image-interpreter.c
bin_epsilon_image_interpreter_tagged_LDADD = \
  lib/libepsilonruntime-tagged.a lib/libepsilonutility.a
# FIXME: this has to go:
bin_epsilon_image_interpreter_tagged_CPPFLAGS = -DEPSILON_RUNTIME_TAGGED

bin_PROGRAMS += bin/epsilon-image-interpreter-smob
bin_epsilon_image_interpreter_smob_SOURCES = \
  image-interpreter/image-interpreter.c
bin_epsilon_image_interpreter_smob_LDADD = \
  lib/libepsilonruntime-smob.a lib/libepsilonutility.a
# FIXME: this has to go:
bin_epsilon_image_interpreter_smob_CPPFLAGS = -DEPSILON_RUNTIME_SMOB


# # Test for the MIPS runtime.
# if COMPILING_FOR_MIPS
# $(srcdir)/bootstrap/scheme/temporary-c-driver/q.s:
# 	echo "  .text" > $@
# 	echo "  .set noreorder" >> $@
# 	echo "  .set macro #  .set nomacro" >> $@
# 	echo "  .set nomips16" >> $@
# 	echo "  .align 2" >> $@
# 	echo "  .globl f" >> $@
# 	echo "  .ent f" >> $@
# 	echo "  .type f, @function" >> $@
# 	echo "f:" >> $@
# 	echo "  .end f" >> $@
# 	echo "  .size f, .-f" >> $@
# bin_PROGRAMS += \
#   bin/epsilon-native-test-untagged
# bin_epsilon_native_test_untagged_SOURCES = \
#   bootstrap/scheme/temporary-c-driver/q.s
# bin_epsilon_native_test_untagged_LDADD = \
#   lib/libepsilondriver-native-untagged.a \
#   lib/libepsilonruntime-untagged.a lib/libepsilonutility.a
# #bin_epsilon_native_test_untagged_CPPFLAGS = -DEPSILON_RUNTIME_UNTAGGED
# endif

# # Test for the C runtime.
# bin_PROGRAMS += \
#   bin/epsilon-c-test-untagged
# $(srcdir)/bootstrap/scheme/temporary-c-driver/w.c:
# 	echo "#include \"runtime/runtime.h\"" > $@
# 	echo "void epsilon_main_entry_point(epsilon_value *stack){}" >> $@
# 	echo "epsilon_word global_data_beginning __attribute__(( aligned(sizeof(void*)) ));" >> $@
# 	echo "epsilon_word global_data_end __attribute__(( aligned(sizeof(void*)) ));" >> $@
# bin_epsilon_c_test_untagged_SOURCES = \
#   bootstrap/scheme/temporary-c-driver/w.c
# bin_epsilon_c_test_untagged_LDADD = \
#   lib/libepsilondriver-c-untagged.a \
#   lib/libepsilonruntime-untagged.a lib/libepsilonutility.a
# # FIXME: this has to go:
# bin_epsilon_c_test_untagged_CPPFLAGS = -I $(builddir) -DEPSILON_RUNTIME_UNTAGGED

# bin_PROGRAMS += \
#   bin/epsilon-c-driver-test-tagged
# bin_epsilon_c_driver_test_tagged_SOURCES = \
#   runtime/c-driver.c bootstrap/scheme/temporary-c-driver/q.c \
#   $(UTILITY_SOURCES) $(RUNTIME_SOURCES)
# bin_epsilon_c_driver_test_tagged_CPPFLAGS = -DEPSILON_RUNTIME_TAGGED

# bin_PROGRAMS += \
#   bin/epsilon-c-driver-test-smob
# bin_epsilon_c_driver_test_smob_SOURCES = \
#   runtime/c-driver.c bootstrap/scheme/temporary-c-driver/q.c \
#   $(UTILITY_SOURCES) $(RUNTIME_SOURCES)
# bin_epsilon_c_driver_test_smob_CPPFLAGS = -DEPSILON_RUNTIME_SMOB

# This should be kept at the end.  It's needed so that a user just wishing to compile from
# sources doesn't need flex and Bison, as the GNU Coding Standards mandate.
EXTRA_DIST += \
  $(BUILT_SOURCES)
## ##################################################### GNU epsilon executables: end

## ####################################################### Profile-driven feedback: begin
profile-driven-feedback: clean
	echo Building for profile generation... && \
	$(MAKE) $(AM_MAKEFLAGS) CFLAGS="$(AM_CFLAGS) $(CFLAGS) -fprofile-generate" && \
	echo Running a benchmark to generate a profile... && \
	@builddir@/bin/epsilon-svm @srcdir@/svm/examples/euclid.s && \
	echo Cleaning compiled files, but not profile information... && \
	$(MAKE) $(AM_MAKEFLAGS) mostlyclean && \
	echo Re-building using profile generation... && \
	$(MAKE) $(AM_MAKEFLAGS) CFLAGS="$(AM_CFLAGS) $(CFLAGS) -fprofile-use" && \
	echo "Generated the profile-driven feedback version"
## ####################################################### Profile-driven feedback: end

## ##################################################### Data files: begin
# dist_pkgdata_DATA += \
#   shared/COPYING shared/README \
#   shared/default-client-settings.xml shared/defaultfont.font \
#   shared/epsilongc-icon-72x72.png shared/epsilongc-icon-editor.xpm \
#   shared/epsilongc-icon-new.png shared/epsilongc-icon.png \
#   shared/scheme-model-format.scm shared/server-side.scm \
#   shared/utility.scm
## ##################################################### Data files: end


## ####################################################### Documentation: begin
info_TEXINFOS = doc/epsilon.texi
doc_epsilon_TEXINFOS = \
  @srcdir@/doc/gpl.texi \
  @srcdir@/doc/fdl.texi
text: doc/epsilon.text
doc/epsilon.text: @srcdir@/doc/epsilon.texi
	$(MAKEINFO) --footnote-style=end --no-split --no-headers \
	            --number-sections -o $@ @srcdir@/doc/epsilon.texi
# The info file is generated under @srcdir@ because GNU standards dictate it should
# be distributed in source archives.
all-doc: doc/epsilon.info doc/epsilon.html doc/epsilon.text doc/epsilon.dvi doc/epsilon.ps doc/epsilon.pdf
#all-doc: @srcdir@/doc/epsilon.info doc/epsilon.html doc/epsilon.text doc/epsilon.dvi doc/epsilon.ps doc/epsilon.pdf
clean-doc:
#	rm -f @srcdir@/doc/epsilon.info
	rm -f doc/epsilon.info
	rm -f doc/epsilon.html doc/epsilon.ps doc/epsilon.pdf doc/epsilon.text doc/epsilon.dvi
	rm -f doc/epsilon.text
# This remains in the root build directory, possibly because of Automake limitations
# relative to paths, or just as an "intermediate" file for ps or pdf. I don't know
# the exact reason, but this workaround works and allows us to pass distcheck.
	rm -rf *.dvi

# # I want to invoke texi2pdf with -E, to workaround some strange macro behavior under TeX:
# TEXI2DVI = texi2dvi -E
# TEXI2PDF = $(TEXI2DVI) --pdf --batch

## ####################################################### Documentation: end


## ####################################################### cleaning: begin
clean-profile:
	rm -rf `find . -name '*~'`  `find . -name '#*#'`       \
	       `find . -name '*.gcda'` `find . -name '*.gcno'` \
	       `find . -name '*.gcov'` `find . -name 'gmon.out'`
clean-local: clean-profile
	rm -rf `find . -name '*~'`  `find . -name '#*#'`       \
	       `find . -name '*.s' | grep -v /examples/ | grep -v /runtime/backend-specific` \
	       `find . -name '*.i'` \
	       `find . -name '*.o'`  `find . -name '*.a'`      \
	       `find . -name '*.so'` `find . -name '*.la'`     \
	       `find . -name '*.lo'`
#	       `find . -name '*.deps'` `find . -name '*.libs'`
	rm -f "dumps/repl-from-guile.u" "dumps/repl-from-epsilon1.u"
	rm -f "@abs_top_builddir@/dumps/unexec-repl-from-guile.u"
	rm -f epsilon-*.shar.gz epsilon-*.tar.gz epsilon-*.tar.bz2 epsilon-*.tar.lzma epsilon-*.zip
#	rm -rf `find $(srcdir)/ -name prefixes.h`
maintainer-clean-local:
	rm -f "bootstrap/scheme/fixed-point-tables.e"
	rm -f "@abs_top_builddir@/bootstrap/scheme/fixed-point-tables.e"
# We can afford to delete @abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u , as
# the compressed version is always @abs_top_srcdir@/bootstrap-dumps/ .
distclean-local: clean-doc clean-profile
	rm -f $(BUILT_SOURCES) $(nodist_BUILT_SOURCES)
	rm -f $(FIXED_POINT_TABLES)
	rm -f "@abs_top_builddir@/dumps/"*
	rm -f "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u"*
	rm -rf autom4te.cache config.log
## ####################################################### cleaning: end

## ####################################################### distribution: begin
edit: clean
	emacs @srcdir@/{Makefile.am,configure.ac,TODO} \
	      @srcdir@/config.h \
	      @srcdir@/*/*.[chly]

strip: all
	strip $(bin_PROGRAMS)

## ####################################################### interactive targets: begin
GUILE2_STACK_SIZE = 10000000000
@abs_top_builddir@/dumps/unexec-repl-from-guile.u: $(bin_PROGRAMS) $(lib_LIBRARIES) $(EPSILON_EARLY_DUMPED_SOURCE_FILES)
	echo rm -f "$@" # Remove the file we're about to generate, just in case we fail later.
	mkdir @abs_top_builddir@/dumps 2> /dev/null || true
	export GUILE_STACK_SIZE=$(GUILE2_STACK_SIZE); ulimit -s unlimited; EPSILON_BUILD_PATH="@abs_top_builddir@" \
	  "@abs_top_builddir@/bin/guile+whatever" \
	  @GUILE_NO_AUTO_COMPILE@ \
	  -l "@abs_top_srcdir@/bootstrap/scheme/bootstrap-from-guile.scm" \
	  -c '(exit 0)'
## FIXME: add the relevant epsilon1 source dependencies, and only them.  This
## will avoid bootstrapping from Guile in many cases, making builds faster.
dumps/repl-from-guile.u: bin/epsilon-image-interpreter-tagged @abs_top_builddir@/dumps/unexec-repl-from-guile.u $(EPSILON_LATE_DUMPED_SOURCE_FILES)
	mkdir @abs_top_builddir@/dumps 2> /dev/null || true
	echo rm -f "$@" # Remove the file we're about to generate, just in case we fail later.
	export GUILE_STACK_SIZE=$(GUILE2_STACK_SIZE); ulimit -s unlimited; "@abs_top_builddir@/bin/epsilon-image-interpreter-tagged" \
	  "@abs_top_builddir@/dumps/unexec-repl-from-guile.u"

dumps/repl-from-epsilon1.u: $(bin_PROGRAMS) $(lib_LIBRARIES) $(EPSILON_DUMPED_SOURCE_FILES) @abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u
	echo rm -f "$@" # Remove the file we're about to generate, just in case we fail later.
	mkdir @abs_top_builddir@/dumps 2> /dev/null || true
	ulimit -s unlimited; echo '(e1:load "@top_builddir@/bootstrap/scheme/configuration.e") (e1:load (string:append configuration:abs_top_srcdir "/bootstrap/scheme/bootstrap-from-epsilon1.e"))' \
	  | "@abs_top_builddir@/bin/epsilon-image-interpreter-tagged" \
	    "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u"

repl-from-guile: dumps/repl-from-guile.u
	export GUILE_STACK_SIZE=$(GUILE2_STACK_SIZE); ulimit -s unlimited; "@abs_top_builddir@/bin/epsilon-image-interpreter-tagged" \
	  "$<"
repl-from-epsilon1: dumps/repl-from-epsilon1.u
	ulimit -s unlimited; "@abs_top_builddir@/bin/epsilon-image-interpreter-tagged" \
	  "$<"

# Make a backup of the previous bootstrap REPL, and replace it with the
# one generated from epsilon1.
bootstrap-repl-from-epsilon1: dumps/repl-from-epsilon1.u
	cp "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u.old" \
	  "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u.old.1" || true
	cp "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u" \
	  "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u.old" || true
	cp "dumps/repl-from-epsilon1.u" \
	  "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u" || true
	echo "Bootstrapped."

# Regenerate a bootstrap REPL from Guile.  As long as the sources are
# correct and newer than the bootstrap REPL, this also works when the
# bootstrap REPL is broken.
bootstrap-repl-from-guile: dumps/repl-from-guile.u
	cp "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u.old" \
	  "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u.old.1" || true
	cp "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u" \
	  "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u.old" || true
	cp "dumps/repl-from-guile.u" \
	  "@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u" || true
	echo "Replaced the bootstrap REPL."

bootstrap-repl: bootstrap-repl-from-epsilon1

# Get the bootstrap REPL by uncompressing the one which is distributed.  File
# modification times will ensure that the uncompressed file isn't overwritten
# when it's been intentionally replaced by a newer version -- the compressed
# version will be older in that case.
@abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u: @abs_top_srcdir@/bootstrap-dumps/bootstrap-repl.u.xz
	mkdir @abs_top_builddir@/bootstrap-dumps/ || true
	unxz - < "$<" > "$@"

## In order to avoid a dependency loop we don't say that the compressed REPL loop
## is obtained from the uncompressed REPL: we only do that at dist time,
## when the compressed version is used.  This is also useful for preparing a new
## compressed file to be committed to git.
## There's no dependency from the uncompressed file to the compressed file, to
## avoid a loop.  The compressed file is only built on demand, and on dist.
## The sleep/touch hack serves to keep the uncompressed image newer than the
## compressed version; without that dependencies would force another useless
## bootstrap from the new compressed image.
compress-bootstrap-repl: @abs_top_builddir@/bootstrap-dumps/bootstrap-repl.u
	if test @abs_top_srcdir@/bootstrap-dumps/bootstrap-repl.u.xz -ot "$<"; then \
	  xz -9 - < "$<" > @abs_top_srcdir@/bootstrap-dumps/bootstrap-repl.u.xz; \
	  sleep 2; \
	  touch "$<"; \
	else \
	  echo "No need to compress the bootstrap REPL again."; \
	fi
EXTRA_DIST += \
  bootstrap-dumps/bootstrap-repl.u.xz
dist-hook: compress-bootstrap-repl

## This is convenient to run the REPL from the build directory.
repl: repl-from-epsilon1

## Run an existing REPL without rebuilding it, even when dependencies would require
## a rebuild.
quick-repl: bin/epsilon-image-interpreter-tagged
	ulimit -s unlimited; "@abs_top_builddir@/bin/epsilon-image-interpreter-tagged" \
	  "dumps/repl-from-epsilon1.u"

## The REPL should be built as a depencency of the default target, but not distributed
## pre-built.  Notice that we don't build the REPL bootstrapped from Guile by default.
edump_pkgdata_DATA += \
  dumps/repl-from-epsilon1.u
esrc_pkgdata_DATA += \
  $(EPSILON_SOURCE_FILES)
## ####################################################### interactive targets: end

## ####################################################### versioning: begin
EXTRA_DIST += \
	$(top_srcdir)/build-aux/get-version-from-git \
	$(top_srcdir)/.tarball-version
# backslash
#	$(top_srcdir)/.version $(top_srcdir)/.tarball-version
# BUILT_SOURCES += \
# 	$(top_srcdir)/.version
# $(top_srcdir)/.version:
# 	echo $(VERSION) > $@-t && mv $@-t $@

# BUILT_SOURCES += \
# 	.version
# dist-hook: .tarball-version
# 	echo $(VERSION) > $(distdir)/.tarball-version

# Fail if .tarball-version, which is supposed to be manually written,
# doesn't exist.
dist-hook: $(top_srcdir)/.tarball-version

# dist-hook: .tarball-version
# 	echo $(VERSION) > $(distdir)/.tarball-version
## ####################################################### versioning: end

## Scratch
built-sources: $(BUILT_SOURCES)
