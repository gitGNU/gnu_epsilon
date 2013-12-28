## Process this file with automake to produce Makefile.in -*-Makefile-*-

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


# This is the backend-specific part of the runtime for the x86
# (the part implemented in assembly)

        .text

# To do: alignment
	.globl epsilon_jump_to_epsilon_context
	.type	epsilon_jump_to_epsilon_context, @function
epsilon_jump_to_epsilon_context:
        nop
        # To do: write this
