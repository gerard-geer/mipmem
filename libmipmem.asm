# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# File: libmipmem.asm
# Author: Gerard Geer
# Purpose: A quick and easy dynamic allocation system written in MARS mips.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# STATIC DATA DEFINITIONS =====================================================
	.data

# Okay, so the emulator doesn't emulate having a system pagesize when calling
# sbrk. Therefore we create our own and use it. This does introduce incompat
# with systems that do implement one; however, it is an easy fix.
mipmem_pagesize: .word 64

# The initialization flag. Set during mipmem_init.
mipmem_initflag: .word 0 # Are we initialized?

# This is the address of the start of the reference metadata table. We don't
# use a hash since memory is continuous. (In the MARS emulator at least.)
# The metadata table stores two words per reference:
# - The procedurally generated name of the reference.
# - The address of the reference.
# Names start at 1, and simply increment to the end of time. If you have 4
# billion variables, it's your problem, not mine.
mipmem_reflist: .space 4

# This will point to the last available reference slot. It's typically 8 bytes
# before the end of the reference table.
mipmem_lastref: .space 4

# This is a pointer to the next available slot in the reference metadata table.
# It increments by 8 bytes each allocation, and every time a reference is
# deleted and the system defrag'd it is retracted 8 bytes.
mipmem_nextref: .space 4

# A quick counter that's also the procedural variable name value.
# The defragmentation routine looks for references named 0x0000, and if it
# finds one, it assumes the reference has been deleted and cleans up.
# Hence, this starts at 1.
# Also, this guarantees that the reference table is never sparse, as deleted
# references still have an address. 
mipmem_procvarname: .word 1 # Current variable "name"

# The start of the data segment.
mipmem_datastart: .space 4 # Pointer to first page.

# The last byte of the data segment, so we can know when we've gone too far.
mipmem_lastbyte: .space 4 # Pointer to the last viable address.

# The address of the next free byte.
mipmem_nextbyte: .space 4 # Pointer to the start of free memory.

# PROCEDURE DEFINITION ========================================================
	.text 

#------------------------------------------------------------------------------
# Function: main
# The entry point function into the program.
# Parameters:
#	None
# Returns:
#	None
#------------------------------------------------------------------------------
main:	
	# Init the memory subsystem.
	jal mipmem_initmem

	li $a0, 16
	jal mipmem_alloc
	li $a0, 16
	jal mipmem_alloc
	li $a0, 16
	jal mipmem_alloc
	li $a0, 16
	jal mipmem_alloc
	li $a0, 16
	jal mipmem_alloc
	li $a0, 16
	jal mipmem_alloc
	li $a0, 16
	jal mipmem_alloc
	li $a0, 16
	jal mipmem_alloc
	li $a0, 16
	jal mipmem_alloc

	li $a0 4
	jal mipmem_dlloc
	
	jal mipmem_defrag


	# Exit.
	li $v0, 10
	syscall

#------------------------------------------------------------------------------
# Function: mipmem_initmem
# Initializes memory management subsystem.
# Parameters:
#	None
# Returns:
#	None.
#------------------------------------------------------------------------------
mipmem_initmem:
	lw $a0, mipmem_pagesize # Get the page size.
	li $v0, 9 # Sbrk that much memory for the reference list.
	syscall

	sw $v0, mipmem_reflist # Store that address in reference metadata table.
	sw $v0, mipmem_nextref # That first long in the table is our first ref.
	add $t0, $v0, $a0      # Compute the startaddress of the last available
	addi $t1, $t0, -8      # reference metadata entry and store it.
	sw $t1, mipmem_lastref 

	# Go ahead and get a first page.
	li $v0, 9
	syscall
	sw $v0, mipmem_datastart  # Store the adress of the first page.
	sw $v0, mipmem_nextbyte   # Our first usable byte is pretty easy to figure out.
	add $t0, $v0, $a0        # Compute the end of the data segment, and since
	sw $t0, mipmem_lastbyte  # we can allocate single bytes, we want the last byte.

	# Now that we're done initializing we can say so by updating the init flag.
	li $t0, 1
	sw $t0, mipmem_initflag
	
	# Okay we can go home now. Party's over.
	jr $ra

#------------------------------------------------------------------------------
# Function: mipmem_expreflist
# Expands the reference metadata table by one page.
# Parameters:
#	None
# Returns:
#	None
#------------------------------------------------------------------------------
mipmem_expreflist:

	# Since the metadata table and data segment are adjacent in memory, in
	# order to expand the metadata table we have to expand everything.
	# Hence first we need to call mipmem_exprange.
	addi $sp, $sp, -4 
	sw $ra, ($sp)
	li $a0, 1
	jal mipmem_exprange
	lw $ra, ($sp)
	addi $sp, $sp, 4

	# Now that we've done that, let's go ahead and move everything up one page.
	# To do that, we need to know the old last page.
	lw $t0, mipmem_pagesize
	lw $t1, mipmem_lastbyte
	lw $t2, mipmem_datastart
	sub $t3, $t1, $t0  # There we go.

	# Loop until $t3 < $t2 (old last addr < first addr), going through and moving
	# every word in the data segment up one page.
	mipmem_moving_things_for_reflist:
	blt $t3, $t2, mipmem_done_moving_things_for_reflist
		lw $t0, ($t3)
		sw $t0, ($t1)

		# Don't forget to clean up!
		li $t9, 0
		sw $t9    ($t3)

		addi $t1, $t1, -4 # Increment our indices.
		addi $t3, $t3, -4
	j mipmem_moving_things_for_reflist
	mipmem_done_moving_things_for_reflist:


	# Now that we're done moving things, we just need to increment the
	# end of the reference list.
	lw $t0, mipmem_lastref
	lw $t1, mipmem_pagesize
	add $t2, $t0, $t1
	sw $t2, mipmem_lastref

	# Oh, we should move the start of the data segment up too.
	lw $t0, mipmem_datastart
	add $t2, $t0, $t1
	sw $t2, mipmem_datastart

	# And dangit we need to go through each of the addresses and increase
	# them by one page.
	lw $t0, mipmem_reflist
	lw $t1, mipmem_pagesize
	lw $t2, 4($t0)
	mipmem_updating_addrs_to_exp_reflist:
	beqz $t2, mipmem_done_updating_addrs_to_exp_reflist
		add $t2, $t1, $t2 # Increment by the page size
		sw $t2 4($t0)	  # Store the new address
		addi $t0, $t0, 8  # Increment the address we're looking at.
		lw $t2, 4($t0)	  # Load that next one.
	j mipmem_updating_addrs_to_exp_reflist
	mipmem_done_updating_addrs_to_exp_reflist:

	# phew!
	jr $ra		
	
#------------------------------------------------------------------------------
# Function: mipmem_exprange
# Takes $a0 more pages of memory from the OS.
# Parameters:
#	$a0: Number of pages to take.
# Returns:
#	None
#------------------------------------------------------------------------------
mipmem_exprange:
	# First we need to calculate the number of bytes from the pages.
	lw $t0, mipmem_pagesize
	mult $t0, $a0
	mflo $a0 # I don't think writing to a recently used reg causes stalls.

	# Now we can get that memory.
	li $v0, 9
	syscall 

	# Increment the last addr.
	lw $t0, mipmem_lastbyte
	add $t0, $t0, $a0
	sw $t0, mipmem_lastbyte
	
	# Our work here is done.
	jr $ra

#------------------------------------------------------------------------------
# Function: mipmem_removegap
# Translates all data past address $a1 down to $a0.
# Parameters:
#	$a0: Address of first byte to crunch.
#   $a1: Address of byte to put there.
# Returns:
#	None
#------------------------------------------------------------------------------
mipmem_removegap:
	# Get all required things.
	lw $t0, mipmem_lastbyte # So we know when we should stop.
	lw $t1, mipmem_nextbyte  # For later use.
	sub $t2, $a1, $a0 # Store the difference between the offsets.
	sub $t3, $t0, $t2 # Store where the new mipmem_lastbyte will be.
	addi $t4, $t0, -32	# Since we're looking ahead we need to stop early.

	# TODO: bytes % 4 check.
	mipmem_removing_data_gap:
	bgt $a1, $t4, mipmem_done_removing_data_gap

		# Read some values.
		lw $s0,   ($a1)	# This is properly chunked to avoid pipeline stalls.
		lw $s1,  4($a1)
		lw $s2,  8($a1)
		lw $s3, 12($a1)

		# Store some values.
		sw $s0,   ($a0)
		sw $s1,  4($a0)
		sw $s2,  8($a0)
		sw $s3, 12($a0)
	
		# Increment our indices.
		addi $a0, $a0, 16
		addi $a1, $a1, 16

	j mipmem_removing_data_gap
	mipmem_done_removing_data_gap:

	# Now we sterilize the area by padding with zeros.
	mipmem_crunch_erasing_old_data:
	ble $t0, $t3 mipmem_crunch_done_erasing_old_data
		sw $zero ($t0)
		addi $t0, $t0, -4
	j mipmem_crunch_erasing_old_data
	mipmem_crunch_done_erasing_old_data:

	# And now that we're safe, we can move the last and cur addrs.
	sw $t0, mipmem_lastbyte
	sub $t0, $t1, $t2
	sw $t0, mipmem_nextbyte

	# and done!
	jr $ra
	
#------------------------------------------------------------------------------
# Function: mipmem_delref
# Removes a reference from the reference table.
# Parameters:
#	$a0: Address of reference.
# Returns:
#	None.
#------------------------------------------------------------------------------
mipmem_delref:
	lw $t0, mipmem_nextref
	
	mipmem_moving_refs_to_delete:
	bge $a0, $t0, mipmem_done_moving_refs_to_delete
		lw $t1, 8($a0)	  # We can only be safe double chunking, since each
		lw $t2, 12($a0)   # ref takes up two words.
		sw $zero, 8($a0)  # Since we have some cycles before $t1 is ready we
		sw $zero, 12($a0) # can go ahead and clean.
		sw $t1,  ($a0)	  # Store the "next" ref in the current one.
		sw $t2, 4($a0)    # The classic overwrite.
		addi $a0, $a0, 8  # Increment.
	j mipmem_moving_refs_to_delete
	mipmem_done_moving_refs_to_delete:

	# Now that all refs are moved, and since the current ref is always at the
	# end, we can decrement mipmem_nextref without checking where it is.
	lw $t0, mipmem_nextref
	addi $t0, $t0, -8
	sw $t0, mipmem_nextref 
	
	jr $ra
	
#------------------------------------------------------------------------------
# Function: mipmem_defrag
# Defrags memory.
# Parameters:
#	None.
# Returns:
#	None.
#------------------------------------------------------------------------------
mipmem_defrag:
	# First we get the first and next (aka next after the last)  refs.
	lw $t0, mipmem_reflist
	lw $t1, mipmem_nextref
	addi $t1, $t1, -4  # Since we have to look ahead...
	
	# Now we loop through each reference metadata entry.
	mipmem_checking_for_fragmentation:
	bge $t0, $t1, mipmem_done_checking_for_fragmentation

		# Get the name assigned to this reference. If it's zero, that means
		# it's been demipmem_allocated. That means....
		lw $t2, ($t0) 
		bnez $t2, mipmem_defrag_next_reference_check
		
			# First we need to crunch.
			addi $sp, $sp, -12 
			sw $t0,  ($sp)    
			sw $t1, 4($sp)     
			sw $ra, 8($sp)     
			
			# Now we get the address associated with that blank reference,
			# As well as the address associated with the reference after it.
			# This is our crunch region.
			lw $a0, 4($t0)  
			lw $a1, 12($t0) 
			jal mipmem_removegap	# Actually do the crunch.

			# Restore everything
			lw $t0,  ($sp)    
			lw $t1, 4($sp)     
			lw $ra, 8($sp)    
			addi $sp, $sp, 12

			# Once we've crunched, we can remove the reference from our
			# meta-data table.
			addi $sp, $sp, -12 
			sw $t0,  ($sp)    
			sw $t1, 4($sp)     
			sw $ra, 8($sp)    

			# As we're iterating our loaded copy of mipmem_reflist, we pass in
			# our current value for it as the address of the ref we need.
			add $a0, $zero, $t0
			jal mipmem_delref

			# Restore everything again.
			lw $t0,  ($sp)    
			lw $t1, 4($sp)     
			lw $ra, 8($sp)    
			addi $sp, $sp, 12
			
			# Now that the reference is deleted we have one fewer reference
			# and we need to reflect that change in mipmem_nextref.
			addi $t1, $t1, -8
			
		mipmem_defrag_next_reference_check:
		# Now that we're past crunching and scrunching, we need to increment.
		addi $t0, $t0, 8
		
	j mipmem_checking_for_fragmentation
	mipmem_done_checking_for_fragmentation:
	
	# That was intensive.
	jr $ra
			

#------------------------------------------------------------------------------
# Function: mipmem_alloc
# Allocates memory!
# Parameters:
#	$a0: Number of bytes to mipmem_allocate.
# Returns:
#	$v0: The address of this freshly mipmem_allocated chunk.
#------------------------------------------------------------------------------
mipmem_alloc:
	# First we need to make sure the memory subsystem is initialized.
	lw $t0, mipmem_initflag	# Load our phat boolean.
	bgtz $t0, mipmem_noinit_alloc  # Jump past all the stack nonsense if we don't need it.
		addi $sp, $sp, -8 # Store everything:
		sw $ra, 4($sp)	  #  * Return address.
		sw $a0, ($sp)     #  * arguments to this function
		jal mipmem_initmem       # Go initialize the memory.
		lw $ra, 4($sp)    # Restore everything.
		lw $a0, ($sp)     #
		addi $sp, $sp, 8  #
	mipmem_noinit_alloc:

	# First let's find out how much memory we have available.
	lw $t0, mipmem_lastbyte
	lw $t1, mipmem_nextbyte
	sub $t2, $t0, $t1 # Use a third reg to avoid pipeline stalls.

	# If we do need to expand...
	bgt $t2, $a0 mipmem_alloc_no_expansion
		addi $sp, $sp, -8 # Store everything:
		sw $ra, 4($sp)	  #  * Return address.
		sw $a0, ($sp)     #  * arguments to this function
		# Calculate how many pages we need.
		lw $t0, mipmem_pagesize  # Get the page size once again.
		div $a0, $t0	  # Bytes / mipmem_pagesize
		mflo $t1		  # Get the result
		addi $a0, $t1, 1  # One extra, just in case.
		# Actually go do our thing
		jal mipmem_exprange      # Go expand our memory.
		lw $ra, 4($sp)    # Restore everything.
		lw $a0, ($sp)     #
		addi $sp, $sp, 8  #
	mipmem_alloc_no_expansion:
	
	# Now that we know we have enough space we need to do two things.
	# First, we must add an entry for this variable into the mipmem_reflist.
	# Then we can increment our data pointer.
	# TODO: Expand mipmem_reflist if needed.
	
 	lw $t0, mipmem_nextref		# Get the address of our entry location
	lw $t1, mipmem_lastref     # Get the address of the last entry location.
	blt $t0, $t1, mipmem_no_ref_resize
		addi $sp, $sp, -12
		sw $t0, -8($sp)
		sw $a0, -4($sp)
		sw $ra,   ($sp)
		jal mipmem_expreflist
		lw $t0, -8($sp)
		lw $a0, -4($sp)
		lw $ra,   ($sp)
		addi $sp, $sp, 12
	mipmem_no_ref_resize:

	lw $t2, mipmem_procvarname  # Get the next-up var name.
	lw $t3, mipmem_nextbyte     # Also get our current address.
	sw $t2, ($t0)	    # Store the variable name.
	sw $t3,4($t0)		# Store the address where its data lies.
	addi $t0, $t0, 8    # Increment the address of the next reference.
	sw $t0, mipmem_nextref     # Save that new reference address.
	addi $t2, $t2, 1    # Increment the current var name.
	sw $t2, mipmem_procvarname  # and save it too.
	
	# Secondly, we need to update the current address.
	add $v0, $zero, $t3 # Not before we store the current address for return.
	add $t3, $t3, $a0
	sw $t3, mipmem_nextbyte

	#...that's all we need to do I think.
	jr $ra  
	

#------------------------------------------------------------------------------
# Function: mipmem_alloc
# Frees memory.
# Parameters:
#	$a0: Variable address to free. 
# Returns:
#	1 if successful, 0 if not.
#------------------------------------------------------------------------------
mipmem_dlloc:
	# We need to make sure the system is initialized before we free too.
	lw $t0, mipmem_initflag	# Load our init flag.
	bgtz $t0, mipmem_noinit_dlloc  # Jump past all the stack nonsense if unnecessary.
		addi $sp, $sp, -4 	# Hear me out on this. Since there is guaranteed to
		sw $ra, 4($sp)	    # be no memory allocated if the system isn't
		jal mipmem_initmem  # initialized, we can shortcut out at this point.
		lw $ra, 4($sp)      # That also means the only thing we need to cache
		addi $sp, $sp, 4    # is $ra.
		jr $ra				# So we can shortcut out right here.
	mipmem_noinit_dlloc:
	
	# First let's get a handle to the reference metadata table and the current
	# reference pointer.
	lw $t0, mipmem_reflist
	lw $t1, mipmem_nextref
	li $t2, 0  # The return flag.
	
	# Now we have to go find this darn reference.
	mipmem_searching_for_ref_to_dealloc:
	bge $t0, $t1, mipmem_done_searching_for_ref_to_dealloc
		
		# Get the address of the current reference.
		lw $t3, 4($t0) # It's great how that 4 is all we neeed to switch to addr hunting.
		
		# If it's the reference we're looking for, we
		# set it's name to 0x0000, and the mipmem_defrag will take care of the
		# rest when it's called.
		bne $a0, $t3, this_is_not_the_ref_we_are_looking_for
			 sw $zero, ($t0)
		this_is_not_the_ref_we_are_looking_for:
		
		# Increment our address.
		addi $t0, $t0, 8
	j  mipmem_searching_for_ref_to_dealloc
	mipmem_done_searching_for_ref_to_dealloc:
	
	# TODO: Should we mipmem_defrag every time?
	jr $ra
	
