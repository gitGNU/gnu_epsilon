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


        nop
        ldi 10 %r0
        ldi 0xe %r1
        ldi 0b1110 %r2
        ldi 010 %r3
        ldi 0b0 %r4
        ;; ldi 0 %r2
        ;; ldi 0 %r3
        ;; ldi 0 %r4
        ;; ldi 0 %r5
        ;; ldi 0 %r6
        ;; ldi 0 %r7
        
        ;; ldi 1000 %sp
        ;; ldi 1001 %lr
        ;; ldi 1002 %cp
        ;; ldi 1003 %cr
        ldi 1 %r5
        ldi 0 %r6
        ldi 1 %r7
#        ld (initial_value) %r5
#        ldi initial_value %r5
#        ld (%r5) %r5            
L1:
#        add %r5 %r5 %r6
#        mov %r6 %r5
        add %r5 %r5 %r5
#        add %r6 %r7 %r6
        add %r6 1 %r6
        
        dump
#        hcf
        b L1

initialized initial_value ( 1 )
initialized irrilevante ( 42 43 )
uninitialized non_inizializzato 3
initialized a ( L1: non_inizializzato )

#         nop
#         ldi 1 %r0
#         ldi 2 %r1
#         ldi 3 %r2
#         ldi 4 %r3
#  L1:
#         dump
#         add %r0 %r1 %r2
#         add %r1 %r2 %r3
# #        hcf
#         jmpi L1

# initialized irrilevante ( 42 43 )
# uninitialized 3 non_inizializzato
# initialized a ( L1: non_inizializzato )
