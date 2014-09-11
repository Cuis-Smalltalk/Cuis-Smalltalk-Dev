'From Cuis 4.2 of 25 July 2013 [latest update: #2079] on 8 September 2014 at 10:23:26.528 pm'!

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 9/8/2014 22:12'!
maxExternalSemaphores
	"The size of table where external semaphores are registered. Only in Cog.
	nil elsewhere"

	^self vmParameterAt: 49! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 9/8/2014 22:17'!
vmParameterAt: parameterIndex
	"parameterIndex is a positive integer corresponding to one of the VM's internal
	parameter/metric registers.  Answer with the current value of that register.
	Answer nil if the VM doesn't provide the register, and the primitive fails.
	VM parameters are numbered as follows:
		1	end of old-space (0-based, read-only)
		2	end of young-space (read-only)
		3	end of memory (read-only)
		4	allocationCount (read-only; nil in Cog VMs)
		5	allocations between GCs (read-write; nil in Cog VMs)
		6	survivor count tenuring threshold (read-write)
		7	full GCs since startup (read-only)
		8	total milliseconds in full GCs since startup (read-only)
		9	incremental GCs since startup (read-only)
		10	total milliseconds in incremental GCs since startup (read-only)
		11	tenures of surving objects since startup (read-only)
		12-20 specific to the translating VM
		21	root table size (read-only)
		22	root table overflows since startup (read-only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc.
		24	memory threshold above whichto shrink object memory (read-write)
		25	memory headroom when growing object memory (read-write)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
		27	number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking
		28	number of times sweep loop iterated for current IGC/FGC (read-only)
		29	number of times make forward loop iterated for current IGC/FGC (read-only)
		30	number of times compact move loop iterated for current IGC/FGC (read-only)
		31	number of grow memory requests (read-only)
		32	number of shrink memory requests (read-only)
		33	number of root table entries used for current IGC/FGC (read-only)
		34	number of allocations done before current IGC/FGC (read-only)
		35	number of survivor objects after current IGC/FGC (read-only)
		36	millisecond clock when current IGC/FGC completed (read-only)
		37	number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only)
		38	milliseconds taken by current IGC (read-only)
		39	Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only)
		40	BytesPerWord for this image
		41	imageFormatVersion for the VM
		42	number of stack pages in use (Cog Stack VM only, otherwise nil)
		43	desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
		44	size of eden, in bytes (Cog VMs only, otherwise nil)
		45	desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
		46	size of machine code zone, in bytes (stored in image file header; Cog JIT VM only, otherwise nil)
		47	desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
		48	various properties of the Cog VM as an integer encoding an array of bit flags.
			 Bit 0: implies the image's Process class has threadId as its 3rd inst var (zero relative)
		49	the size of the external semaphore table (read-write; Cog VMs only)
		50-55 reserved for VM parameters that persist in the image (such as eden above)
		56	number of process switches since startup (read-only)
		57	number of ioProcessEvents calls since startup (read-only)
		58	number of ForceInterruptCheck (Cog VMs) or quickCheckInterruptCalls (non-Cog VMs) calls since startup (read-only)
		59	number of check event calls since startup (read-only)
		60	number of stack page overflows since startup (read-only; Cog VMs only)
		61	number of stack page divorces since startup (read-only; Cog VMs only)
		62	number of machine code zone compactions since startup (read-only; Cog VMs only)
		63	milliseconds taken by machine code zone compactions since startup (read-only; Cog VMs only)
		64	current number of machine code methods (read-only; Cog VMs only)
		65	true if the VM supports multiple bytecode sets;  (read-only; Cog VMs only; nil in older Cog VMs)
		70	the value of VM_PROXY_MAJOR (the interpreterProxy major version number)
		71	the value of VM_PROXY_MINOR (the interpreterProxy minor version number)"

	<primitive: 254>
	^nil! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'jmv 9/8/2014 22:10'!
isRunningCog
	"Returns true if we're running on a Cog VM (JIT or StackInterpreter)
	 (vmParameterAt: 42 is the number of stack pages)
	Smalltalk isRunningCog
	"

	^(self vmParameterAt: 42)
		ifNotNil: [ :stackPages | stackPages > 0 ]
		ifNil: [ false ]! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'jmv 9/8/2014 22:10'!
isRunningCogit
	"Returns true if we're running on the Cog JIT
	 (vmParameterAt: 46 is the size of the machine code zone)
	Smalltalk isRunningCog
	"

	^(self vmParameterAt: 46)
		ifNotNil: [ :machineCodeZoneSize  | machineCodeZoneSize > 0 ]
		ifNil: [ false ]! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'jmv 9/8/2014 22:15'!
processPreemptionYields
	"Answer whether the VM causes a process to yield on process preemption,
	 i.e. to put a preempted process at the back of its run queue.  If the parameter
	 is unavailable (non-Cog VMs) or bit 2 (4) is 0 then preemption yields.
	Smalltalk processPreemptionYields
	"

	^ (self vmParameterAt: 48)
		ifNil: [ true ]
		ifNotNil: [ :properties |
			(properties allMask: 4) not ]! !

!SystemDictionary methodsFor: 'image format' stamp: 'jmv 9/8/2014 22:07'!
imageFormatVersion
	"Answer an integer identifying the type of image in memory. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). This invokes an optional primitive
	that may not be available on all virtual machines.
	Answer nil if unknown."

	"
	Smalltalk imageFormatVersion
	"

	<primitive: 'primitiveImageFormatVersion'>

	"Cog provides a VM parameter"
	^Smalltalk vmParameterAt: 41! !

