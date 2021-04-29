'From Cuis 5.0 [latest update: #4567] on 29 April 2021 at 2:52:18 pm'!

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 4/29/2021 14:52:10'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[self displayWorld] ifError: [ :err :rcvr |
		"Handle a drawing error"
		canvas currentMorphDrawingFails.
		self setCanvas.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	]! !

