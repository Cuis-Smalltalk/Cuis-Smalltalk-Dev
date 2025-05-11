'From Cuis7.3 [latest update: #7122] on 11 May 2025 at 7:15:34 pm'!

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 6/13/2024 14:18:52'!
stepIntoBlock
	"Send messages until you return to the present method context.
	 Used to step into a block in the method."

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].

	self 
		handleLabelUpdatesIn: [interruptedProcess stepToHome: self selectedContext]
		whenExecuting: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn! !

