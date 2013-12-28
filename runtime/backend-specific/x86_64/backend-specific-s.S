## x86_64-specific runtime: assembly part.

## This file is part of GNU epsilon
## Copyright (C) 2013 Luca Saiu

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


# This is the backend-specific part of the runtime for the x86_64
# (the part implemented in assembly)

#include "config.h"

        .text

	.p2align 4,,15
.globl epsilon_run_thread_context
	.type	epsilon_run_thread_context, @function
epsilon_run_thread_context:
        # Save registers:
        pushq %rax
        pushq %r11
        pushq %rbx
        pushq %r12
        pushq %r13

        # Copy the thread context address where we will keep it:
        movq %rdi, %r13

        # Extract the (low) epsilon stack pointer from the context:
        movq 8(%r13), %rbx

        # Keep epsilon_c_primitive_functions in a register:
	movq $epsilon_c_primitive_functions, %r12

        # Call:
        callq *%rsi

        # Restore registers:
        popq %r13
        popq %r12
        popq %rbx
        popq %r11
        popq %rax

        # We're done:
        retq
        .size	epsilon_run_thread_context, .-epsilon_run_thread_context
