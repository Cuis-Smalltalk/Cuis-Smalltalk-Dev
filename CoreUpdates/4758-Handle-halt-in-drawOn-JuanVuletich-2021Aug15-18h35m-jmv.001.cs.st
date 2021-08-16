'From Cuis 5.0 [latest update: #4757] on 15 August 2021 at 6:37:12 pm'!

!BlockClosure methodsFor: 'evaluating' stamp: 'jmv 8/15/2021 18:36:43'!
ifErrorOrHalt: errorHandlerBlock
	"Evaluate the block represented by the receiver, and normally return it's value.  If an error occurs, the errorHandlerBlock is evaluated, and it's value is instead returned.  The errorHandlerBlock must accept zero, one, or two parameters (the error message and the receiver)."
	"Examples:
		[1 halt] ifErrorOrHalt: [:err :rcvr | 'huh?'].
		[1 whatsUpDoc] ifErrorOrHalt: [:err :rcvr | 'huh?'].
		[1 / 0] ifErrorOrHalt: [:err :rcvr |
			'ZeroDivide' = err
				ifTrue: [Float infinity]
				ifFalse: [self error: err]]
"

	^ self on: Error, Halt do: [ :ex |
		errorHandlerBlock valueWithPossibleArgument: ex description and: ex receiver ]! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 8/15/2021 17:30:09'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[self displayWorld] ifErrorOrHalt: [ :err :rcvr |
		"Handle a drawing error"
		canvas currentMorphDrawingFails.
		self setCanvas.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	]! !

