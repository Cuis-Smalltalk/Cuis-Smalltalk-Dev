'From Cuis 4.0 of 21 April 2012 [latest update: #1495] on 4 December 2012 at 11:06:23 pm'!

!CompiledMethod methodsFor: 'scanning' stamp: 'eliot 10/8/2012 11:24'!
scanFor: byteOrClosure
	"Answer whether the receiver contains the argument as a bytecode,
	 if it is a number, or evaluates to true if a block."
	^ (InstructionStream on: self) scanFor: (byteOrClosure isBlock
													ifTrue: [byteOrClosure]
													ifFalse: [[:instr | instr = byteOrClosure]])
"
Smalltalk browseAllSelect: [:m | m scanFor: 134]
"! !

