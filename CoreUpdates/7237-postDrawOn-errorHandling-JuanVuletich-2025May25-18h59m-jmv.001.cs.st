'From Cuis7.3 [latest update: #7236] on 25 May 2025 at 7:04:13 pm'!

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 5/25/2025 19:03:29'!
mainLoop

	[
		self displayWorldOn: canvas.
		[
			"Here canvas and waitDelay are passed as arguments.
			This is because when debugging Morphic, a new UI process,
			these ivars are updated, but in the debugger process we still want the original ones."
			self doOneCycleOn: canvas delay: waitDelay.
			UISupervisor isUIProcessRunning ]
				whileTrue: []
	] on: Error, Halt do: [ :ex |
		"Handle a drawing error"
		(ex signalerContext findContextSuchThat: [ :context |
				#(drawOn: postDrawOn:) statePointsTo: context selector ])
			ifNotNil: [ :context |
				context receiver drawingFails.
				"Install the old error handler, so we can re-raise the error.
				This means, open a Debugger.
				This will also create a new Morphic process. if the user closes the debugger."
				ex receiver error: ex description.
				"Create a new UI process, if the user proceeds in the debugger.
				This is to use a new, clean Canvas."
				UISupervisor newUIProcess ]
			ifNil: [ ex pass ]
	].! !

