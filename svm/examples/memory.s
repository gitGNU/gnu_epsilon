## [one-line description]

## This file is part of GNU epsilon
## Copyright (C) 2012 Université Paris 13

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


;; #! /home/luca/thesis/implementation/well-packaged-sources/bin/slow-virtual-machine
        ldi pointer_to_x + 0 %r1
        ld (%r1) %r0
loop:   ld (%r0) %r3
        add %r3 1 %r4
        st %r4 (%r0)
        dumpr %r4
        b loop

initialized pointer_to_x ( x )
initialized x ( -100000 )
