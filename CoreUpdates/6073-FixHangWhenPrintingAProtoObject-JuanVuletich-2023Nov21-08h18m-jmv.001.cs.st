'From Cuis 6.0 [latest update: #6072] on 21 November 2023 at 8:20:10 am'!

!Debugger methodsFor: 'private' stamp: 'jmv 11/21/2023 08:17:07'!
fixReceiverInspector
	"Make receiver inspector work on current context receiver.
	Create a new inspector if needed"

	| currentReceiver requiredInspectorClass oldInspectorClass |
	currentReceiver := self receiver.
	requiredInspectorClass := currentReceiver isObject
		ifTrue: [ currentReceiver inspectorClass ]
		ifFalse: [ Inspector ].
	oldInspectorClass := receiverInspector class.
	
	oldInspectorClass ~= requiredInspectorClass ifTrue: [
		oldInspectorClass format = requiredInspectorClass format
			ifTrue: [receiverInspector primitiveChangeClassTo: requiredInspectorClass basicNew]
			ifFalse: [receiverInspector becomeForward: (requiredInspectorClass basicNew copyFrom: receiverInspector)]].
	
	receiverInspector object: currentReceiver! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 11/21/2023 08:06:14'!
mainLoop

	| morphicCanvas d |
        "If this platform supports deferred updates
		- Set Display to deferred update mode
		- Use a Canvas on Display
	otherwise
		- Set a non-Display canvas"
	self morphPosition: `0@0` extent: Display extent.
	morphicCanvas := (Display deferUpdates: true)
		ifNotNil: [ Display getMainCanvas ]
		ifNil: [ (Form extent: extent depth: Display depth) getCanvas ].
	morphicCanvas world: self.
	"Set this as the main canvas"
	canvas := morphicCanvas.
	damageRecorder := DamageRecorder new.
	damageRecorder recordInvalidRect: self morphLocalBounds for: nil.
	self restoreDisplay.

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime := Time localMillisecondClock.
	lastAlarmTime := 0.

	d := Delay forMilliseconds: 50.
	[
		self displayWorldOn: morphicCanvas.
		[
			"Set the main canvas on every iteration.
			This is needed if UI processes are interrupted, resumed, etc."
			canvas := morphicCanvas.
			self doOneCycleOn: morphicCanvas delay: d.
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

!methodRemoval: WorldMorph #handleFatalDrawingError: stamp: 'jmv 11/21/2023 08:19:42'!
WorldMorph removeSelector: #handleFatalDrawingError:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
UISupervisor newUIProcess.!

