'From Cuis7.1 [latest update: #6596] on 17 August 2024 at 5:27:17 pm'!

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 8/16/2024 16:58:35'!
setupDisplay
	"
	DisplayScreen setupDisplay.
	Display forceToScreen.
	"

	self terminateScreenUpdater.
	Display ifNil: [
		Smalltalk at: #Display put: DisplayScreen new.
		"See #recreateSpecialObjectsArray"
		Smalltalk specialObjectsArray at: 15 put: Display ].
	Display setExtent: self actualScreenSize depth: Display nativeDepth.! !

!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 8/16/2024 17:05:06'!
screenUpdateRequired: aRectangle

	ScreenUpdaterProcess ifNotNil: [
		Damage := aRectangle
			ifNotNil: [ aRectangle quickMerge: Damage ]
			ifNil: [ Display boundingBox ].
		ScreenUpdaterSemaphore signal ].! !

!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 8/16/2024 17:06:36'!
terminateScreenUpdater

	ScreenUpdaterProcess ifNotNil: [
		ScreenUpdaterProcess terminate ].
	ScreenUpdaterProcess := nil.
	ScreenUpdaterSemaphore := nil.
	Damage := nil.! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 8/16/2024 17:03:16'!
mainLoop

	| morphicCanvas morphicDamageRecorder delay |
	self morphPosition: `0@0` extent: Display extent.

        "If this platform supports deferred updates
		- Set Display to deferred update mode
		- Use a Canvas on Display
	otherwise
		- Set a non-Display canvas"
	(Display deferUpdates: true)
		ifNotNil: [
			"Both Linux and Windows VM do support deferUpdates:, so Morphic Canvas target is Display."
			morphicCanvas := Display getMainCanvas.
			"But then, they need to flush updates periodically."
			DisplayScreen installScreenUpdater ]
		ifNil: [
			"Mac (at least intel) doesn't support deferUpdates:, so Morphic Canvas target is this aux Form."
			morphicCanvas := (Form extent: extent depth: Display depth) getCanvas ].

	morphicCanvas ownerProcess: Processor activeProcess.
	morphicCanvas world: self.
	morphicDamageRecorder := DamageRecorder new.
	"Set this as the main canvas"
	canvas := morphicCanvas.
	damageRecorder := morphicDamageRecorder.
	damageRecorder recordInvalidRect: self morphLocalBounds for: nil.
	self restoreDisplay.

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime := Time localMillisecondClock.
	lastAlarmTime := 0.

	delay := Delay forMilliseconds: 50.
	[
		self displayWorldOn: morphicCanvas.
		[
			"Set the main canvas on every iteration.
			This is needed if UI processes are interrupted, resumed, etc."
			canvas := morphicCanvas.
			damageRecorder := morphicDamageRecorder.
			self doOneCycleOn: morphicCanvas delay: delay.
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

