## An SVM example using a list

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


### Scan the list (1 2 3 4 5), printing each element.

        ldi 0 %r0
        ldi cons-1 %r1
## Is the list null?  If so, branch out:
loop:   cmp %r1 %r0 %c0
        be %c0 out
## The list is not null: load its car and print it:
        ld (%r1 + 0) %r2
        dumpr %r2
## The new list to examine becomes the rest of the list.  Continue:
        ld (%r1 + 1) %r1
        b  loop
out:
        exit

initialized cons-1 ( 1 cons-2 )
initialized cons-2 ( 2 cons-3 )
initialized cons-3 ( 3 cons-4 )
initialized cons-4 ( 4 cons-5 )
initialized cons-5 ( 5 0 )
