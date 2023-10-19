'From Cuis 6.0 [latest update: #6044] on 19 October 2023 at 7:15:27 pm'!

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 10/19/2023 17:29:59'!
canBeInterrupted: aProcess
	"Answer nil if aProcess can be interrupted.
	Answer an error message if not."

	aProcess isTerminated ifTrue: [
		^('Process {', aProcess printString, '} isTerminated. Can not interrupt it.') ].

	"The background process can't be interrupted, or Cuis will likely crash."
	aProcess == Processor backgroundProcess ifTrue: [
		^ 'Can not interrupt backgroundProcess' ].

	(aProcess name beginsWith: '[system]') ifTrue: [
		^ ('Process {', aProcess printString, '} is critical for system stability. Can not interrupt it.') ].

	^nil! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 10/19/2023 17:30:09'!
handleUserInterrupt
	| processToInterrupt oldUIProcess |
	"If the UI process is not ready to run, the UI was not responsive, and most likely that's why the user pressed
	the interrupt key. Make GUI responsive again, and let them debug the process that made UI not responsive.
	'The four Andreas tests for interruptability':
	http://lists.squeakfoundation.org/pipermail/squeak-dev/2022-July/221727.html
		[true] whileTrue
		[[true] whileTrue] forkAt: Processor userSchedulingPriority + 1
		Smalltalk createStackOverflow
		[Smalltalk createStackOverflow] forkAt: Processor userSchedulingPriority + 1
	"
	oldUIProcess := UISupervisor newUIProcessIfCurrentCantRun.
	"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
	processToInterrupt := Sensor shiftPressed | (Preferences at: #cmdDotInterruptTakesStatistics)
		ifTrue: [Utilities reportCPUandRAM]
		ifFalse: [Utilities processTakingMostCPU].
	(Preferences at: #cmdDotEnabled) ifFalse: [
		^self ].
	(processToInterrupt isNil or: [ (self canBeInterrupted: processToInterrupt) notNil]) ifTrue: [
		processToInterrupt := oldUIProcess ifNil: [ Processor preemptedProcess ]].
	processToInterrupt isNil ifTrue: [
		'No process to interrupt found' print.
		^self ].
	(self canBeInterrupted: processToInterrupt) ifNotNil: [ :msg |
		msg print.
		^self ].
	[
		EventSensor install.
		Debugger openProcess: processToInterrupt context: processToInterrupt suspendedContext label: 'User Interrupt'
	] fork "Forked so the #userInterruptWatcher process continues."! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 10/19/2023 19:14:06'!
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
				(context receiver is: #Morph) and: [ context  selector = #drawOn: ]])
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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
UISupervisor newUIProcess!

