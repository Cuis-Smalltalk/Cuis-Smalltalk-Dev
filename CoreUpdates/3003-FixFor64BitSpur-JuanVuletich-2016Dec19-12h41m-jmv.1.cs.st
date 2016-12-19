'From Cuis 5.0 of 7 November 2016 [latest update: #3002] on 19 December 2016 at 12:44:03 pm'!

!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 12/17/2016 22:38:52'!
initialPC
	"Answer the program counter for the receiver's first bytecode."
	^ (self numLiterals + 1) * Smalltalk wordSize + 1! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 12/17/2016 22:37:27'!
lowSpaceThreshold 
	"Answer the low space threshold. When the amount of free memory (after garbage collection)
	 falls below this limit, the system is in serious danger of completely exhausting memory and
	 crashing. This limit should be made high enough to allow the user open a debugger to diagnose
	 a problem or to save the image.  In a stack-based VM such as Cog contexts for activations in
	 the stack zone will have to be created as the debugger opens, requiring additional headroom."

	| slotsForDebugger slotsForContextsOnStackPages |
	slotsForDebugger := 65536. "Arbitrary guess"
	slotsForContextsOnStackPages :=
		(self vmParameterAt: 42)
			ifNil: [0]
			ifNotNil:
				[:numStackPages| | headerSize numActivationsPerPage maxContextSize |
				numActivationsPerPage := 40. "Design goal of the Cog VM"
				headerSize := 2. "64-bytes for Spur"
				maxContextSize := MethodContext instSize + CompiledMethod fullFrameSize + headerSize.
				numStackPages * numActivationsPerPage * maxContextSize].
	^slotsForDebugger + slotsForContextsOnStackPages * self wordSize! !

