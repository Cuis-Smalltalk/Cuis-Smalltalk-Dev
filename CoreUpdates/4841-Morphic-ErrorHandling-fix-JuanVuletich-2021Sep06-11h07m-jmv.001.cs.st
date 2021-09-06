'From Cuis 5.0 [latest update: #4839] on 6 September 2021 at 11:07:34 am'!

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/6/2021 11:07:11'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[self displayWorld] on: Error, Halt do: [ :ex |
		"Handle a drawing error"
		canvas currentMorphDrawingFails.
		"Creating a new canvas here could be dangerous, as code signaling the exception will be resumed."
		self resetCanvas.
		"Install the old error handler, so we can re-raise the error"
		ex receiver error: ex description.
	]! !

