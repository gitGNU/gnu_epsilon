#!/bin/bash -e

while ! make maintainer-clean; do
    ./autogen.sh
    ./configure
done
rm -f configure Makefile.in aclocal.m4 config.h.in INSTALL
rm -rf build-aux libltdl
