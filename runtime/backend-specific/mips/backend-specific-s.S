## MIPS-specific runtime: assembly part.

## Copyright (C) 2013 Luca Saiu

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

#include "config.h" // for SIZEOF_LONG

        .text
	.set noreorder
	.set macro
	.set nomips16
#        .option pic2
#	.gnu_attribute 4, 1
#	.abicalls
#	.option	pic2

	.align 2
	.globl epsilon_run_thread_context
	.ent epsilon_run_thread_context
	.type	epsilon_run_thread_context, @function
epsilon_run_thread_context:
        # Save callee-save registers:
        addiu $sp, $sp, (-6 * SIZEOF_LONG)
        sw $16, (1 * SIZEOF_LONG)($sp)
        sw $21, (2 * SIZEOF_LONG)($sp)
        sw $22, (3 * SIZEOF_LONG)($sp)
        sw $23, (4 * SIZEOF_LONG)($sp)
        sw $28, (5 * SIZEOF_LONG)($sp)
        sw $31, (6 * SIZEOF_LONG)($sp)

        # Load the thread context address into $23.  We will always leave it there.
        move $23, $4

        # Load the address of epsilon_c_primitive_functions into $22.  We will
        # always leave it there.
	lui $28,%hi(__gnu_local_gp)
	addiu $28,$28,%lo(__gnu_local_gp)
	lw $22,%got(epsilon_c_primitive_functions)($28)

        # Keep a copy of $gp, which is to say $28, in $21: the o32 ABI allows C functions
        # to trash it, so we always keep a copy in a callee-save register just to be able
        # to quickly restore it.
        move $21, $28

        # Call:
        lw $16, (SIZEOF_LONG * 1)($23) # load (low) stack pointer
        move $25, $5 # the procedure is our second parameter
        jal $25
#        sw $6, 0($16) # delay slot: copy the third parameter to the stack
        nop # empty delay slot

#        # Load the result before we have to overwrite the epsilon stack pointer
#        lw $2, 0($16) # delay slot: result: load (low) stack pointer

        # Restore callee-save registers:
        lw $16, (1 * SIZEOF_LONG)($sp)
        lw $21, (2 * SIZEOF_LONG)($sp)
        lw $22, (3 * SIZEOF_LONG)($sp)
        lw $23, (4 * SIZEOF_LONG)($sp)
        lw $28, (5 * SIZEOF_LONG)($sp)
        lw $31, (6 * SIZEOF_LONG)($sp)
        addiu $sp, $sp, (6 * SIZEOF_LONG)

        # Return
        j $31
        nop # delay slot
        .end epsilon_run_thread_context
        .size epsilon_run_thread_context, .-epsilon_run_thread_context
