## Process this file with automake to produce Makefile.in -*-Makefile-*-

## This file is part of GNU epsilon
## Copyright (C) 2006, 2007, 2008, 2013 Luca Saiu
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


################## To do: this seems ugly. Is it still needed? Yes, it is.
#hdir_includedir = @prefix@
################## To do: this seems ugly. Is it still needed? Yes, it is.

EXTRA_DIST =
AM_LDFLAGS =
AUTOMAKE_OPTIONS = subdir-objects

AM_YFLAGS = -d
AM_LFLAGS = -s

AM_CFLAGS = -Wall
AM_CFLAGS += @WERROR_OR_NOTHING@
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
  runtime/backend-specific/x86/backend-specific-c.c \
  runtime/backend-specific/x86/backend-specific-s.S \
  runtime/backend-specific/x86_64/backend-specific-c.c \
  runtime/backend-specific/x86_64/backend-specific-s.S \
  runtime/backend-specific/powerpc/backend-specific-c.c \
  runtime/backend-specific/powerpc/backend-specific-s.S \
  runtime/backend-specific/mips/backend-specific-c.c \
  runtime/backend-specific/mips/backend-specific-s.S \
  runtime/backend-specific/always-fail runtime/backend-specific/i386 runtime/backend-specific/i486 runtime/backend-specific/i586 runtime/backend-specific/i686 runtime/backend-specific/mips runtime/backend-specific/mips64 runtime/backend-specific/mips64el runtime/backend-specific/mipsel runtime/backend-specific/powerpc runtime/backend-specific/ppc runtime/backend-specific/svm runtime/backend-specific/x86 runtime/backend-specific/x86_64 runtime/backend-specific/x86-64

EXTRA_DIST += \
  bootstrap/scheme/epsilon0-in-scheme.scm \
  bootstrap/scheme/core.e \
  bootstrap/scheme/fill-reflective-structures.scm \
  bootstrap/scheme/epsilon1.scm \
  bootstrap/scheme/debugging-stuff-for-guile.scm \
  bootstrap/scheme/toplevel-in-scheme.scm \
  bootstrap/scheme/export-toplevel-forms-to-guile.scm \
  bootstrap/scheme/conversion.scm \
  bootstrap/scheme/unexec.e \
  bootstrap/scheme/scratch.scm \
  bootstrap/scheme/bootstrap.scm \
  bootstrap/scheme/quick-start.scm

## Some files are only useful to manually re-generate some code in core.e:
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
BUILT_SOURCES =
dist_noinst_SCRIPTS =
dist_pkgdata_DATA =

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

lib_LIBRARIES += lib/libepsilondriver-c-untagged.a
lib_libepsilondriver_c_untagged_a_SOURCES = \
  runtime/backend-specific/c/backend-specific-c.c \
  driver/driver.c
lib_libepsilondriver_c_untagged_a_LIBADD = \
  lib/libepsilonruntime-smob.a lib/libepsilonutility.a

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
## ####################################################### libraries: end


## ##################################################### GNU epsilon executables: begin
# One of these is conditionally #include'd by runtime/data.c:
EXTRA_DIST += \
  runtime/data-tagged.c \
  runtime/data-untagged.c \
  runtime/data-smob.c

# A little sanity test for the utility functions:
bin_PROGRAMS += \
  bin/epsilon-test-utility
bin_epsilon_test_utility_SOURCES = \
  utility/test.c
bin_epsilon_test_utility_LDADD = \
  lib/libepsilonutility.a
#bin_epsilon_test_utility_CPPFLAGS = -DEPSILON_RUNTIME_UNTAGGED

# Currently disabled
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
	rm -f translator/eamlscanner.[ch] translator/eamlparser.[ch]
	rm -f epsilon-*.shar.gz epsilon-*.tar.gz epsilon-*.tar.bz2 epsilon-*.tar.lzma epsilon-*.zip
#	rm -rf `find $(srcdir)/ -name prefixes.h`
distclean-local: clean-doc clean-profile
	rm -f $(BUILT_SOURCES)
	rm -rf autom4te.cache config.log
## ####################################################### cleaning: end

## ####################################################### distribution: begin
edit: clean
	emacs @srcdir@/{Makefile.am,configure.ac,TODO} \
	      @srcdir@/config.h \
	      @srcdir@/*/*.[chly]

strip: all
	strip $(bin_PROGRAMS)
