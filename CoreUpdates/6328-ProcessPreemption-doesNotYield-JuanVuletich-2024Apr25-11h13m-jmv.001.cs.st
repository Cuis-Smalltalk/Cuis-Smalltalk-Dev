'From Cuis6.3 [latest update: #6327] on 25 April 2024 at 11:26:55 am'!

!SystemDictionary methodsFor: 'vm parameters' stamp: 'jmv 4/25/2024 11:24:24'!
doMixedArithmetic
	"If true, then primitives can handle the conversions:
	Float (Small or Boxed) arithmeticOp: SmallInteger
	Float (Small or Boxed) compareOp: SmallInteger
	Else, the primitive fail in case of mixed arithmetic, and conversion will be performed at image side.
	See doMixedArithmetic:
	
	Note:
		OpenSmalltalk VMs after March, 2019 can set the option and will honor it. The comparison operation behaves as if the Float was converted #asTrueFraction. This means that some rather big SmallIntegers in 64 bit systems, that can not be represented exactly as a Float will not be equal to any Float. Squeak adopted this critera. Cuis follows the more conventional, Smalltalk-80 tradition to always convert to Float if any operand is Float. Therefore Cuis needs to do 'Smalltalk doMixedArithmetic: false'.
		Previous VMs can not set the option, and will answer true when queried. But these VMs did the conversion to Float, and the requested operation in Floats. So, with these VMs, Cuis will also have the desired behavior.
		
		Smalltalk doMixedArithmetic
		SmallInteger maxVal asFloat = SmallInteger maxVal

		Smalltalk doMixedArithmetic: true
		SmallInteger maxVal asFloat = SmallInteger maxVal

		Smalltalk doMixedArithmetic: false
		SmallInteger maxVal asFloat = SmallInteger maxVal
		"

	^ ((self vmParameterAt: 48) allMask: 64) not! !

!SystemDictionary methodsFor: 'vm parameters' stamp: 'jmv 4/25/2024 11:26:11'!
vmParameterAt: parameterIndex
	"parameterIndex is a positive integer corresponding to one of the VM's internal parameter/metric registers.
	 Answer with the current value of that register.  Primitive fails if parameterIndex is invalid or has no corresponding register.
	Answer nil if the VM doesn't provide the register, and the primitive fails.
	
	Also see #getVMParameters and #vmParameterAt:put: These 3 methods call the 
	same primitive, whose behavior depends on argument count:
		0 args:	return an Array of VM parameter values;
  		1 arg:	return the indicated VM parameter;
  		2 args:	set the VM indicated parameter.

	VM parameters are numbered as follows:
		1	byte size of old-space (read-only)
		2	byte size of young-space (read-only)
		3	byte size of object memory (read-only)
		4	nil (was allocationCount (read-only))
		5	nil (was allocations between GCs (read-write)
		6	survivor count tenuring threshold (read-write)
		7	full GCs since startup (read-only)
		8	total milliseconds in full GCs since startup (read-only)
		9	Scavenging GCs
		10	total milliseconds in scavenging GCs since startup (read-only) since startup (read-only)
		11	tenures of surving objects since startup/last write (read-write)
		12	event trace mask; if 1 << eventType is set in the mask then event received by primGetNextEvent: will be printed to stderr.
		13	VM ticker start microseconds (Croquet/QwaqVM only)
		14	VM ticker count (Croquet/QwaqVM only)
		15	VM ticker call count (Croquet/QwaqVM only)
		16	total microseconds at idle since start-up (if non-zero)
		17	fraction of the code zone to use (Sista only; used to control code zone use to preserve sendAndBranchData on counter tripped callback)
		18	total milliseconds in full GC compaction since startup (a portion of parameter 8)
		19	scavenge threshold, the effective size of eden.  When eden fills to the threshold a scavenge is scheduled.
		20	utc microseconds at VM start-up (actually at time initialization, which precedes image load).
		21	root/remembered table size (occupancy) (read-only)
		22	root table overflows since startup (read-only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc (stored in image file header).
		24	memory threshold above which to shrink object memory (read-write)
		25	ammount to grow by when growing object memory (read-write)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
		27	number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking
		28	number of times sweep loop iterated for current IGC/FGC (read-only)
		29	number of times make forward loop iterated for current IGC/FGC (read-only)
		30	number of times compact move loop iterated for current IGC/FGC (read-only)
		31	number of grow memory requests (read-only)
		32	number of shrink memory requests (read-only)
		33	number of root table entries used for current IGC/FGC (read-only)
		34	bytes allocated in total since start-up or reset (read-write)
		35	number of survivor objects after current IGC/FGC (read-only)
		36	millisecond clock when current IGC/FGC completed (read-only)
		37	number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only)
		38	milliseconds taken by current IGC (read-only)
		39	Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only)
		40	BytesPerOop for this image
		41	imageFormatVersion for the VM
		42	number of stack pages in use (Cog Stack VM only, otherwise nil)
		43	desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
		44	size of eden, in bytes (Cog VMs only, otherwise nil)
		45	desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
		46	size of machine code zone, in bytes (stored in image file header; Cog JIT VM only, otherwise nil)
		47	desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
		48	various properties stored in the image header (that instruct the VM) as an integer encoding an array of bit flags. 
			Bit 0: if set, tells the VM that the image's Process class has threadId as its 5th inst var 'threadAffinity' 
					(after nextLink, suspendedContext, priority & myList)
			Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods. This is useful when profiling a system to set the number
					of literals beyond which CompiledMethods are not jitted.
			Bit 2: if set, preempting does not yield. If not set, preempting a process puts it at the back of the run queue, yielding to all other
					process of the same priority. This was the Smalltalk-80 behavior, but it makes sharing structures between processes of 
					the same priority unnecesarily more complex.
			Bit 3: in a VM that supports muilt-threaded FFI, if set, the Window system will only be accessed from the first VM thread:
					'noThreadingOfGUIThread'
			Bit 4: in a Spur vm, if set, new finalization enabled. Enable for images with Ephemeron support.
					causes weaklings and ephemerons to be queued individually for finalization
			Bit 5: If set, wheel events will be delivered as such and not mapped to arrow key events.
					If unset, create fake keyboard events for wheel events. 
			Bit 6: See #doMixedArithmetic:
				If set, arithmetic primitives won't perform conversion in case of mixed SmallInteger/Float, and fail, letting the image decide what to do.
				If unset, the VM will make the conversion, meaning that `SmallInteger maxVal asFloat = SmallInteger maxVal` could evaluate to false.
			Bit 7: if set use the unix epoch for dates and times in files (FilePlugin / FileAttributesPlugin) / or is it about UTC???- If set UTC seconds from epoch, rather than local seconds
			Bit 8: if set upscaleDisplayIfHighDPI
		49	the size of the external semaphore table (read-write; Cog VMs only)
		50-51 reserved for VM parameters that persist in the image (such as eden above)
		52	root (remembered) table maximum size (read-only)
		53	the number of oldSpace segments (Spur only, otherwise nil)
		54	total size of free old space (Spur only, otherwise nil)
		55	ratio of growth and image size at or above which a GC will be performed post scavenge
		56	number of process switches since startup (read-only)
		57	number of ioProcessEvents calls since startup (read-only)
		58	number of forceInterruptCheck (Cog VMs) or quickCheckInterruptCalls (non-Cog VMs) calls since startup (read-only)
		59	number of check event calls since startup (read-only)
		60	number of stack page overflows since startup (read-only; Cog VMs only)
		61	number of stack page divorces since startup (read-only; Cog VMs only)
		62	number of machine code zone compactions since startup (read-only; Cog VMs only)
		63	milliseconds taken by machine code zone compactions since startup (read-only; Cog VMs only)
		64	current number of machine code methods (read-only; Cog VMs only)		
		65	In newer Cog VMs a set of flags describing VM features,
				if non-zero bit 0 implies multiple bytecode set support;
				if non-zero bit 1 implies read-only object support;
				if non-zero bit 2 implies the VM suffers from using an ITIMER heartbeat (if 0 it has a thread that provides the heartbeat)
			(read-only; Cog VMs only; nil in older Cog VMs, a boolean answering multiple bytecode support in not so old Cog VMs)
				if non-zero bit 3 implies the VM supports cross-platform BIT_IDENTICAL_FLOATING_POINT arithmetic
				if non-zero bit 4 implies the VM can catch exceptions in FFI calls and answer them as primitive failures
				if non-zero bit 5 implies the VM's suspend primitive backs up the process to before the wait if it was waiting on a condition variable
		66	the byte size of a stack page in the stack zone  (read-only; Cog VMs only)
		67	the maximum allowed size of old space in bytes, 0 implies no internal limit (Spur VMs only).
		68	the average number of live stack pages when scanned by GC (at scavenge/gc/become et al)
		69	the maximum number of live stack pages when scanned by GC (at scavenge/gc/become et al)
		70	the value of VM_PROXY_MAJOR (the interpreterProxy major version number)
		71	the value of VM_PROXY_MINOR (the interpreterProxy minor version number)
		72	total milliseconds in full GCs Mark phase since startup (read-only)
		73	total milliseconds in full GCs Sweep phase since startup (read-only, can be 0 depending on compactors)
		74	maximum pause time due to segment allocation
		75	whether arithmetic primitives will do mixed type arithmetic; if false they fail for different receiver and argument types
		76	the minimum unused headroom in all stack pages; Cog VMs only"

	<primitive: 254>
	(parameterIndex isInteger
	 and: [parameterIndex > 0]) ifTrue:
		[^nil].
	self primitiveFailed! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk processPreemptionYields: false.!

