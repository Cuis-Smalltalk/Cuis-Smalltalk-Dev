'From Cuis7.1 [latest update: #6656] on 29 August 2024 at 5:35:13 pm'!

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 8/29/2024 12:21:07'!
mainCanvas: aMorphicCanvas
	"Set the main canvas"

	canvas := aMorphicCanvas.
	damageRecorder := DamageRecorder new.
	damageRecorder recordInvalidRect: self localBounds for: nil.
	waitDelay := Delay forMilliseconds: 50.! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 8/29/2024 12:24:11'!
mainLoop

	self position: `0@0` extent: Display extent.

        "If this platform supports deferred updates
		- Set Display to deferred update mode
		- Use a Canvas on Display
	otherwise
		- Set a non-Display canvas"
	(Display deferUpdates: true)
		ifNotNil: [
			"But then, they need to flush updates periodically."
			DisplayScreen installScreenUpdater.
			"Both Linux and Windows VM do support deferUpdates:, so Morphic Canvas target is Display."
			self mainCanvas: Display getMainCanvas.
			canvas world: self.
			]
		ifNil: [
			"Mac (at least intel) doesn't support deferUpdates:, so Morphic Canvas target is this aux Form."
			self mainCanvas: (Form extent: extent depth: Display depth) getMainCanvas.
			canvas world: self ].
	self restoreDisplay.

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime := Time localMillisecondClock.
	lastAlarmTime := 0.

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
				context  selector = #drawOn: ])
			ifNotNil: [ :context |
				context receiver drawingFails.
				"Install the old error handler, so we can re-raise the error.
				This means, open a Debugger.
				This will also create a new Morphic process. if the user closes the debugger."
				ex receiver error: ex description.
				"Create a new UI process, if the user proceeds in the debugger."
				UISupervisor newUIProcess ]
			ifNil: [ ex pass ]
	].! !

