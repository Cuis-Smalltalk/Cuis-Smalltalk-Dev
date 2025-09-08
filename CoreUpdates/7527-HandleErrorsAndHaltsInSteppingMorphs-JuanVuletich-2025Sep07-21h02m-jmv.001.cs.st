'From Cuis7.5 [latest update: #7526] on 7 September 2025 at 9:08:40 pm'!

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 9/7/2025 21:07:35'!
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
				#(drawOn: postDrawOn: fullDraw: step stepAt:) statePointsTo: context selector ])
			ifNotNil: [ :context | | failingMorph selector |
				failingMorph := context selector = #fullDraw:
					ifTrue: [ (context at: 1) ]
					ifFalse: [ context receiver ].
				selector := context selector.
				(#(drawOn: postDrawOn: fullDraw:) statePointsTo: selector)
					ifTrue: [ failingMorph drawingFails ].
				(#(step stepAt:) statePointsTo: selector)
					ifTrue: [ failingMorph stopStepping ].
				"Install the old error handler, so we can re-raise the error.
				This means, open a Debugger.
				This will also create a new Morphic process. if the user closes the debugger."
				ex receiver error: ex description.
				"Create a new UI process, if the user proceeds in the debugger.
				This is to use a new, clean Canvas."
				UISupervisor newUIProcess ]
			ifNil: [
				"Debugging aid: Uncomment. Set Transcript to log to StdOut and File.
				Be ready to need to kill Cuis from the OS."
				"'-----' print.
				{Time now. Processor activeProcess. ex} print.
				thisContext printStack: 50.
				[UISupervisor newUIProcess] fork."
				"ex return"
				ex pass ]
	].! !

