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


# Approximate the fix-point of cosine, starting from 0.0
        ldi iterations %r1
        ld (%r1) %r0
        dumpr %r0
        ldi initial_float_value %r1
        ldf (%r1) %fcp0
        dumpr %fcp0
loop:   
        callc-f-f libm.so$cosf
        dumpr %fcr
        add %r0 -1 %r0
        cmp %r0 0 %c0
        bne %c0 loop
        ldi 0 %rcp0
        callc-v-i $exit
        
initialized iterations          ( 30 )
initialized initial_float_value ( 0.0 )
