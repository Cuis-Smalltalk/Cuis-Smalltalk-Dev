'From Cuis7.5 [latest update: #7370] on 27 July 2025 at 12:27:25 pm'!

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 7/27/2025 12:26:17'!
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
				#(drawOn: postDrawOn: fullDraw:) statePointsTo: context selector ])
			ifNotNil: [ :context |
				context selector = #fullDraw:
					ifTrue: [ (context at: 1) drawingFails ]
					ifFalse: [ context receiver drawingFails ].
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
				ex return ]
	].! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/27/2025 12:22:02'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"
	aMorph visible ifFalse: [^ self].

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	self into: aMorph.

	currentMorph isKnownFailing ifTrue: [
		self canvasToUse drawCurrentAsError.
		self outOfMorph.
		^ self].

	currentMorph layoutSubmorphsIfNeeded.

	(currentMorph isOwnedByHand and: [
			(Preferences at: #cheapWindowReframe) and: [currentMorph is: #SystemWindow]]) ifTrue: [
		self canvasToUse drawCurrentAsOutline ifTrue: [
			self outOfMorph.
			^ self]].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph.! !

