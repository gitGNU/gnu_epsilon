#!/bin/bash -e
## This file is part of GNU epsilon.

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


mkdir build-aux &> /dev/null || true
autoreconf --verbose --install --force
#libtoolize --verbose --force --install --copy
#pushd libltdl
#autoreconf --verbose --install --force
#popd
echo 'Regenerated stuff with success.'
