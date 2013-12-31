'From Cuis 4.0 of 21 April 2012 [latest update: #1504] on 4 December 2012 at 11:37:01 pm'!
!classDefinition: #ClosureExtractor category: #'Kernel-Methods'!
InstructionClient subclass: #ClosureExtractor
	instanceVariableNames: 'action scanner currentContext '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

!ClosureExtractor methodsFor: 'instruction decoding' stamp: 'eem 10/24/2012 14:08'!
blockReturnTop
	currentContext := currentContext sender! !


!ClosureExtractor methodsFor: 'accessing' stamp: 'eem 10/24/2012 14:09'!
scanner: anInstructionStream
	scanner := anInstructionStream.
	currentContext := MethodContext
							sender: nil
							receiver: self
							method: scanner method
							arguments: (Array new: scanner method numArgs)! !

!ClosureExtractor methodsFor: 'instruction decoding' stamp: 'eem 10/24/2012 14:11'!
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Create a BlockClosure corresponding to the closure bytecode
	 and execute the action block with it. The created BlockClosure is only a pseudo value,
	 it is not populated with meaningful context and argument information."
	| block |
	block := BlockClosure
				outerContext: currentContext
				startpc: scanner pc
				numArgs: numArgs
				copiedValues: (Array new: numCopied)..
	currentContext := block asContextWithSender: currentContext.
	action value: block! !

!classDefinition: #ClosureExtractor category: #'Kernel-Methods'!
InstructionClient subclass: #ClosureExtractor
	instanceVariableNames: 'action scanner currentContext'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!
