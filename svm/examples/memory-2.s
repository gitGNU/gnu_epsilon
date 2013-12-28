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
        ldi pointer_to_x %r3
        dumpr %r3
        dumpr %r0
        ld (%r3)     %r0        ; %r0 contains the pointer to x
loop:   ld (%r0 + 1) %r1        ; During the first iteration %r1 contains 22, the second element of x
        ld (%r0 + 3) %r2        ; During the first iteration %r2 contains 4444, the fourth element of x
        dumpr %r1
        dumpr %r2
        ;;  Swap the second and fourth element of x in memory:
        st %r2 (%r0 + 1)
        st %r1 (%r0 + 3)
        b  loop
        exit

initialized pointer_to_x ( x )
initialized useless ( 91 92 93 94 95 96 97 98 99 )
initialized x ( 1 22 333 4444 55555 666666 7777777 88888888 )
