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

        
        ldi 3333333333 %r1     ; This fits in 32 bit only if it's unsigned
        ldi 12  %r2

        dumpr %r1
        dumpr %r2
        
euclid: cmp %r1 %r2 %c0
        be  %c0 out
#        cmp %r1 %r2 %c0         ; not needed, of course
        blt  %c0 less
        sub %r1 %r2 %r1
        b euclid
less:   sub %r2 %r1 %r2
        b euclid
out:    dumpr %r1
        exit
