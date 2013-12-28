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


# This is the backend-specific part of the runtime for the PowerPC
# (the part implemented in assembly)

	.align 2
	.p2align 4,,15
	.globl epsilon_jump_to_epsilon_context
	.type	epsilon_jump_to_epsilon_context, @function
epsilon_jump_to_epsilon_context:
        # %r3 contains the context pointer at the beginning of the
        # routine.  But we need to work with the register image, which
        # is pointed by the first field of the struct pointer by %r3:
	lwz %r31,0(%r3) # it's the first field in the struct
        # Now %r31 contains the register image pointer.
        
        # Restore general registers, except %r31 and %r0, which I use
        # as scratch.
#        lwz %r0,(4+0*4)(%r31)
        lwz %r1,(4+1*4)(%r31)
#        lwz %r2,(4+2*4)(%r31)     # %r2 seems to have some special meaning...
        lwz %r3,(4+3*4)(%r31)
        lwz %r4,(4+4*4)(%r31)
        lwz %r5,(4+5*4)(%r31)
        lwz %r6,(4+6*4)(%r31)
        lwz %r7,(4+7*4)(%r31)
        lwz %r8,(4+8*4)(%r31)
        lwz %r9,(4+9*4)(%r31)
        lwz %r10,(4+10*4)(%r31)
        lwz %r11,(4+11*4)(%r31)
        lwz %r12,(4+12*4)(%r31)
        lwz %r13,(4+13*4)(%r31)
        lwz %r14,(4+14*4)(%r31)
        lwz %r15,(4+15*4)(%r31)
        lwz %r16,(4+16*4)(%r31)
        lwz %r17,(4+17*4)(%r31)
        lwz %r18,(4+18*4)(%r31)
        lwz %r19,(4+19*4)(%r31)
        lwz %r20,(4+20*4)(%r31)
        lwz %r21,(4+21*4)(%r31)
        lwz %r22,(4+22*4)(%r31)
        lwz %r23,(4+23*4)(%r31)
        lwz %r24,(4+24*4)(%r31)
        lwz %r25,(4+25*4)(%r31)
        lwz %r26,(4+26*4)(%r31)
        lwz %r27,(4+27*4)(%r31)
        lwz %r28,(4+28*4)(%r31)
        lwz %r29,(4+29*4)(%r31)
        lwz %r30,(4+30*4)(%r31)
#        lwz %r31,(4+31*4)(%r31)

        # Restore floating-point registers, which come next in the struct:
        lfd %f0,(4+32*4+0*8)(%r31)
        lfd %f1,(4+32*4+1*8)(%r31)
        lfd %f2,(4+32*4+2*8)(%r31)
        lfd %f3,(4+32*4+3*8)(%r31)
        lfd %f4,(4+32*4+4*8)(%r31)
        lfd %f5,(4+32*4+5*8)(%r31)
        lfd %f6,(4+32*4+6*8)(%r31)
        lfd %f7,(4+32*4+7*8)(%r31)
        lfd %f8,(4+32*4+8*8)(%r31)
        lfd %f9,(4+32*4+9*8)(%r31)
        lfd %f10,(4+32*4+10*8)(%r31)
        lfd %f11,(4+32*4+11*8)(%r31)
        lfd %f12,(4+32*4+12*8)(%r31)
        lfd %f13,(4+32*4+13*8)(%r31)
        lfd %f14,(4+32*4+14*8)(%r31)
        lfd %f15,(4+32*4+15*8)(%r31)
        lfd %f16,(4+32*4+16*8)(%r31)
        lfd %f17,(4+32*4+17*8)(%r31)
        lfd %f18,(4+32*4+18*8)(%r31)
        lfd %f19,(4+32*4+19*8)(%r31)
        lfd %f20,(4+32*4+20*8)(%r31)
        lfd %f21,(4+32*4+21*8)(%r31)
        lfd %f22,(4+32*4+22*8)(%r31)
        lfd %f23,(4+32*4+23*8)(%r31)
        lfd %f24,(4+32*4+24*8)(%r31)
        lfd %f25,(4+32*4+25*8)(%r31)
        lfd %f26,(4+32*4+26*8)(%r31)
        lfd %f27,(4+32*4+27*8)(%r31)
        lfd %f28,(4+32*4+28*8)(%r31)
        lfd %f29,(4+32*4+29*8)(%r31)
        lfd %f30,(4+32*4+30*8)(%r31)
        lfd %f31,(4+32*4+31*8)(%r31)
        
        # Restore condition registers, using %r0 as scratch:
        lwz %r0,(4+32*4+32*8)(%r31) # Saved conditions -> %r0
        mtcr %r0
        
        # Restore the count register:
        lwz %r0,(4+32*4+32*8+4+0*4)(%r31) # Saved %cr -> %r0
        mtctr %r0
        
        # Push the saved value of %r0, %r31 and %lr, onto the (already restored) stack:
        addi %r1,%r1,-12       # Make place on the stack:
        lwz %r0,(4+32*4+32*8+4+1*4)(%r31) # saved %lr-> %r0
        stw %r0,8(%r1)          # I'm not sure. Should I use another offset?
        lwz %r0,(4+0*4)(%r31)   # saved %r0 -> %r0
        stw %r0,4(%r1)          # I'm not sure. Should I use another offset?
        lwz %r0,(4+32*4)(%r31)  # saved %r31 -> %r0
        stw %r0,0(%r1)          # I'm not sure. Should I use another offset?
        
        # Load the instruction pointer and jump to the final address:
	lwz %r0,0(%r31)
	mtlr %r0
	blr
        
        # To do: well, we should restore %r0, %r31 and %lr from the stack, and
        # update the stack pointer *after returning*.

        # To do: discover what the fuck I should do with %r2. Is it reserved
        # for some strange usage I don't know about? GCC doesn't seem to use it.

# Finish restoring the part of state that we have saved on the stack
# but we had to temporarily trash in order to jump.  We have to restore
# %r0, %r31 and %lr, and to pop three words. Notice that we need a scratch
# register for setting %lr, and we use %r0.
#define EPSILON_FINISH_RESTORING_THE_CONTEXT \
        lwz %r0,8(%r1) \
        mtlr %r0 \
        lwz %r0,4(%r1) \
        lwz %r31,0(%r1) \
        addi %r1,%r1,+12

# # To do: this last macro is still very preliminar.
# # Save to the stack some registers that we need to trash when dumping, then
# # dump by calling a routine which does most of the work, and jump where the
# # caller asked to jump.  When the context is eventually restored by a call to
# # epsilon_jump_to_epsilon_context, the control will return to the last part
# # of this macro (another macro call to EPSILON_FINISH_RESTORING_THE_CONTEXT),
# # restoring what epsilon_jump_to_epsilon_context could not directly restore.
# # From then on, control goes on sequentially.
# #define EPSILON_DUMP_CONTEXT(context_name, where_to_jump) \
#         addi %r1,%r1,-8
#         stw %r0, 0(%r1)
#         mfcr %r0
#         stw %r0, 4(%r1)
#         bl epsilon_dump_context
# context_name # _label:  
