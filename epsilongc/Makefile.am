## Process this file with automake to produce Makefile.in -*-Makefile-*-

## This file is part of GNU epsilon
## Copyright (C) 2006, 2007 Luca Saiu
## (built on the eAM and NAUSEA Makefile.am, by Luca Saiu)
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


## This Automake template follows the "Alternative Approach to Subdirectories".
## See the GNU Automake manual for more information.


################# To do: this seems ugly. Is it still needed? Yes, it is.
hdir_includedir = @prefix@
################# To do: this seems ugly. Is it still needed? Yes, it is.

AM_LDFLAGS =
AUTOMAKE_OPTIONS = subdir-objects

AM_YFLAGS = -d
AM_LFLAGS = -s

AM_CFLAGS = -Wall
AM_CFLAGS += @WERROR_OR_NOTHING@
AM_CFLAGS += -pthread
AM_CFLAGS += -static
#AM_CFLAGS += -static-libgcc
#AM_LDFLAGS += /usr/lib/libc_nonshared.a

# Optimization:
#AM_CFLAGS += -fomit-frame-pointer
#AM_CFLAGS += -O3 -funroll-all-loops -foptimize-sibling-calls
#AM_CFLAGS += -finline-functions

#AM_CFLAGS += -fno-inline-functions

#AM_CFLAGS += -O0 -g

AM_CFLAGS += -I @srcdir@/include/epsilongc -I .

# GCov support:
#AM_CFLAGS += -fprofile-arcs -ftest-coverage

# GProf support:
#AM_CFLAGS += -fPIC -fpic
#AM_CFLAGS += -pg
#AM_LDFLAGS += /usr/lib/libdl_p.a /usr/lib/libpthread_p.a /usr/lib/libc_p.a
#AM_CFLAGS += -static
#AM_CFLAGS += -O2 -fno-unroll-loops -fno-inline-functions -fno-optimize-sibling-calls

AM_CFLAGS += -g

#AM_CFLAGS += -save-temps -fverbose-asm

#AM_CFLAGS = -Wall -g -ffast-math -finline-functions -funroll-loops @GUILE_CFLAGS@
#AM_CFLAGS += -p -pg
#AM_CFLAGS += -O3 -funroll-loops -finline-functions
#AM_CFLAGS += -fno-strict-aliasing
#AM_CFLAGS += -rdynamic

#AM_LDFLAGS = -export-dynamic -rdynamic -dlopen self @GUILE_LDFLAGS@

EXTRA_DIST = BUGS NO_WARRANTY PLATFORMS

## Yes, no subdirs. Both easier and faster; and also more correct with respect
## to dependencies. Ha.
SUBDIRS =

## Let's give an initial name to some variables, so that we can safely use
## += later:
noinst_PROGRAMS =
bin_PROGRAMS =
lib_LIBRARIES =
lib_LTLIBRARIES =
noinst_LTLIBRARIES =
nobase_dist_hdir_include_HEADERS = \
  @srcdir@/include/epsilongc/allocator.h \
  @srcdir@/include/epsilongc/compile_time_parameters.h \
  @srcdir@/include/epsilongc/epsilongc_threads.h \
  @srcdir@/include/epsilongc/global_structures.h \
  @srcdir@/include/epsilongc/epsilongc_debug.h \
  @srcdir@/include/epsilongc/epsilongc_debugging_hack.h \
  @srcdir@/include/epsilongc/epsilongc_features.h \
  @srcdir@/include/epsilongc/heuristics.h \
  @srcdir@/include/epsilongc/run_time_settings.h \
  @srcdir@/include/epsilongc/kind.h \
  @srcdir@/include/epsilongc/page.h \
  @srcdir@/include/epsilongc/pool.h \
  @srcdir@/include/epsilongc/roots.h \
  @srcdir@/include/epsilongc/set_of_pages.h \
  @srcdir@/include/epsilongc/trace.h \
  @srcdir@/include/epsilongc/epsilongc_macros.h \
  @srcdir@/include/epsilongc/epsilongc_types.h \
  @srcdir@/include/epsilongc/doubly_linked_list_macros.h \
  @srcdir@/include/epsilongc/fatal.h \
  @srcdir@/include/epsilongc/malloc.h \
  @srcdir@/include/epsilongc/time.h \
  @srcdir@/include/epsilongc/epsilongc.h \
  @srcdir@/include/epsilongc/large.h \
  @srcdir@/include/epsilongc/myrand.h \
  @srcdir@/include/epsilongc/declarationstoinline.h \
  @srcdir@/include/epsilongc/definitionstoinline.h \
  include/epsilongc/config.h

#nobase_nodist_hdir_include_HEADERS =
BUILT_SOURCES =
dist_noinst_SCRIPTS =
dist_pkgdata_DATA =

## ##################################################### commodity scripts: begin
#dist_noinst_SCRIPTS += clean-everything regenerate-stuff
## ##################################################### commodity scripts: end

## ##################################################### not for the user: begin
# Regenerate Emacs tags:
TAGS:
	etags `find -name \*.c` `find -name \*.h` \
	      `find -name \*.scm` \
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
	@cat */*.[chly] */*.scm */Makefile Makefile* | wc -l
	@echo -n "Non-empty source lines:"
#       This is quite fun:
	@echo " " | (cat */*.[chly]* */*.scm */Makefile Makefile* -) \
	          | nl --body-numbering=t | tail --lines=1
	@echo "=============================================================="
## ##################################################### not for the user: begin


## ####################################################### libraries: begin
## Configuration info:
#nobase_nodist_hdir_include_HEADERS += \
#  config.h

# ## The config program:
bin_PROGRAMS += \
  bin/epsilongc-config
bin_epsilongc_config_SOURCES = \
  src/epsilongc_config.c
bin_epsilongc_config_LDADD = \
  lib/libepsilongc.a

# ## The collector library:
lib_LIBRARIES += \
  lib/libepsilongc.a
lib_libepsilongc_a_SOURCES = \
  src/epsilongc_combined.c

BUILT_SOURCES += \
  src/epsilongc_print_enable_macros.c

#	grep ' ENABLE_' include/epsilongc/config.h | grep '^\#define' | awk '{print "printf(\""$$2":   yes\\n\");"}' > $$x && \
#	grep ' ENABLE_' include/epsilongc/config.h | grep '^/\* \#undef' | awk '{print "printf(\""$$3":   \*NO\*\\n\");"}' >> $$x && 

src/epsilongc_print_enable_macros.c: include/epsilongc/config.h
# 	grep ' HAVE_' include/epsilongc/config.h | \
# 	  grep '^\#define' | \
# 	  awk '{print "printf(\"%-40s: %s\\n\", \""$$2"\", \"yes\");"}' >> $$x && \
# 	grep ' HAVE_' include/epsilongc/config.h | \
# 	  grep '^/\* \#undef' | \
# 	  awk '{print "printf(\"%-40s: %s\\n\", \""$$3"\", \"no\");"}' >> $$x && 
	x=`mktemp` && \
	grep ' ENABLE_' include/epsilongc/config.h | \
	  grep '^\#define' | \
	  awk '{print "printf(\"%-40s: %s\\n\", \""$$2"\", \"yes\");"}' > $$x && \
	grep ' ENABLE_' include/epsilongc/config.h | \
	  grep '^/\* \#undef' | \
	  awk '{print "printf(\"%-40s: %s\\n\", \""$$3"\", \"no\");"}' >> $$x && \
	echo ' // This file is automatically generated. Do not edit it.' > $@ && \
	sort $$x >> $@ && \
	rm $$x

# Distribute all sources, even if we use the combined version
# (which simply #include's them) for building:
EXTRA_DIST += src/allocator.c src/benchmark1.c src/benchmark2.c \
  src/debug.c src/epsilongc.c src/epsilongc_combined.c \
  src/epsilongc_debug.c src/epsilongc_threads.c src/fatal.c \
  src/global_structures.c src/heuristics.c src/kind.c src/malloc.c \
  src/page.c src/pool.c src/roots.c src/run_time_settings.c \
  src/set_of_pages.c src/test.c src/test2-mccarthy.c src/test2.c \
  src/test3.c src/test_doubly_linked_list.c src/test_set.c src/time.c \
  src/trace.c include/epsilongc/allocator.h include/epsilongc/compile_time_parameters.h \
  src\large.c src/myrand.c \
  include/epsilongc/doubly_linked_list_macros.h include/epsilongc/epsilongc_debug.h \
  include/epsilongc/epsilongc_debugging_hack.h include/epsilongc/epsilongc_features.h \
  include/epsilongc/epsilongc_threads.h include/epsilongc/epsilongc_types.h include/epsilongc/fatal.h \
  include/epsilongc/filesystem.h include/epsilongc/global_structures.h include/epsilongc/heuristics.h \
  include/epsilongc/kind.h include/epsilongc/malloc.h include/epsilongc/page.h include/epsilongc/pool.h \
  include/epsilongc/roots.h include/epsilongc/run_time_settings.h include/epsilongc/set_of_pages.h \
  include/epsilongc/stack.h include/epsilongc/time.h include/epsilongc/trace.h include/epsilongc/large.h

# lib_libepsilongc_la_SOURCES = \
#   src/types.h \
#   src/epsilongc.h src/epsilongc.c \
#   src/allocator.h src/allocator.c \
#   src/global_structures.h src/global_structures.c \
#   src/kind.h src/kind.c \
#   src/page.h src/page.c \
#   src/pool.h src/pool.c \
#   src/heuristics.h src/heuristics.c \
#   src/set_of_pages.h src/set_of_pages.c \
#   src/trace.h src/trace.c \
#   src/roots.h src/roots.c \
#   src/sweep.h src/sweep.c \
#   common/fatal.h common/fatal.c \
#   common/time.h common/time.c \
#   common/malloc.h common/malloc.c \
#   common/doubly_linked_list.h common/doubly_linked_list.c \
#   common/set.h common/set.c


#BUILT_SOURCES += \
#  common/GLFontScanner.cc common/GLFontParser.cc common/GLFontParser.hh \
#  common/XMLScanner.cc common/XMLParser.cc common/XMLParser.hh
#common/GLFontScanner.cc: common/GLFontScanner.ll
#	$(LEX) -Pglfont_ -o$@ $(AM_LFLAGS) $^
#common/GLFontParser.cc: common/GLFontParser.yy
#	$(YACC) -p glfont_ -o $@ $(AM_YFLAGS) $^
#common/GLFontParser.hh: common/GLFontParser.cc
#	true # Do nothing: this is generated together with its companion .cc file
#common/XMLScanner.cc: common/XMLScanner.ll
#	$(LEX) -Pxml_ -o$@ $(AM_LFLAGS) $^
#common/XMLParser.cc: common/XMLParser.yy
#	$(YACC) -p xml_ -o $@ $(AM_YFLAGS) $^
#common/XMLParser.hh: common/XMLParser.cc
#	true # Do nothing: this is generated together its companion .cc file

# ## Lex and Yacc sources are not distributed by default unless we include them
# ## in a _SOURCES variable, but we currently can't do it because default rules
# ## for Lex and Yacc don't cope well with the 'single Makefile' style we're
# ## adopting. This slightly raw solution forces distribution:
# EXTRA_DIST += \
#   common/GLFontScanner.ll common/GLFontParser.yy \
#   common/XMLScanner.ll common/XMLParser.yy
#lib_libepsilongc_la_LIBADD = \
#  @GUILE_LDFLAGS@

#noinst_PROGRAMS += \
#  tests/test-scheme

#tests_test_scheme_SOURCES = common/TestScheme.cc
#EXTRA_DIST += tests/foo.scm
## ####################################################### libraries: end


## ##################################################### GNU epsilon executables: begin
## Utilities:
bin_PROGRAMS += \
  bin/epsilongc-test \
  bin/epsilongc-test2 \
  bin/epsilongc-test-set \
  bin/epsilongc-benchmark1 \
  bin/epsilongc-benchmark2 \
  bin/epsilongc-test-doubly-linked-list
bin_epsilongc_test_doubly_linked_list_SOURCES = \
  src/test_doubly_linked_list.c private/doubly_linked_list_macros.h
bin_epsilongc_test_doubly_linked_list_LDADD = lib/libepsilongc.a
bin_epsilongc_test_SOURCES = src/test.c
bin_epsilongc_test_LDADD = lib/libepsilongc.a
bin_epsilongc_test2_SOURCES = src/test2.c
bin_epsilongc_test2_LDADD = lib/libepsilongc.a
bin_epsilongc_test_set_SOURCES = src/test_set.c
bin_epsilongc_test_set_LDADD = lib/libepsilongc.a
bin_epsilongc_benchmark1_SOURCES = src/benchmark1.c
bin_epsilongc_benchmark1_LDADD = lib/libepsilongc.a
bin_epsilongc_benchmark2_SOURCES = src/benchmark2.c
bin_epsilongc_benchmark2_LDADD = lib/libepsilongc.a
## Icons should be included in distributions, but not installed (they are
## xpm files, #included in C++ sources); so they end up in EXTRA_DIST instead
## of, for example, dist_pkgdata_DATA:
#EXTRA_DIST += editor/images/README editor/images/editcopy.xpm editor/images/editcut.xpm
## ##################################################### GNU epsilon executables: end


## ##################################################### Data files: begin
# dist_pkgdata_DATA += \
#   shared/COPYING shared/README \
#   shared/default-client-settings.xml shared/defaultfont.font \
#   shared/epsilongc-icon-72x72.png shared/epsilongc-icon-editor.xpm \
#   shared/epsilongc-icon-new.png shared/epsilongc-icon.png \
#   shared/scheme-model-format.scm shared/server-side.scm \
#   shared/utility.scm
## ##################################################### Data files: end




## ####################################################### cleaning: begin
#clean-local: clean-doc
clean-local:
	rm -rf `find . -name '*~'`  `find . -name '#*#'`       \
	       `find . -name '*.s'` `find . -name '*.i'`       \
	       `find . -name '*.o'`  `find . -name '*.a'`      \
	       `find . -name '*.so'` `find . -name '*.la'`     \
	       `find . -name '*.lo'` `find . -name '*.gcda'`   \
	       `find . -name '*.gcno'`                         \
	       `find . -name 'gmon.out'`
#	       `find . -name '*.deps'` `find . -name '*.libs'`
	rm -f translator/eamlscanner.[ch] translator/eamlparser.[ch]
	rm -f $(BUILT_SOURCES)
	rm -f epsilongc-*.tar.gz
#	rm -rf `find $(top_srcdir)/ -name prefixes.h`
distclean-local:
	rm -rf autom4te.cache config.log
## ####################################################### cleaning: end

## ####################################################### distribution: begin
edit:
	emacs @srcdir@/{src,include/epsilongc}/*.[ch] \
	      @srcdir@/{Makefile.am,configure.ac,TODO} include/epsilongc/config.h&

backup: clean
	date=`date +"%Y-%m-%d"`; \
	filename="/tmp/epsilongc-$$date.tar.bz2"; \
	tar c ../epsilongc | bzip2 -9 - > $$filename && scp $$filename saiu@lipn.fr:nanolisp-snapshots && \
	scp $$filename positron@fencepost.gnu.org:backups/ && \
	mv $$filename ..

# To do: is this needed?
# ## Remove CVS files from source tarballs
# dist-hook:
# #	rm -rf `find $(distdir)/ -name CVS`
## ####################################################### distribution: end
