'From Cuis 5.0 [latest update: #4523] on 15 January 2021 at 5:05:05 pm'!

!HandMorph methodsFor: 'double click support' stamp: 'jmv 1/14/2021 18:49:09'!
dontWaitForMoreClicks
	"Reset the double-click detection state to normal (i.e., not waiting for a double-click).
	This happens after timeout, regardless of multiple clicks having been detected or not."

	mouseClickState _ nil.! !

!HandMorph methodsFor: 'event handling' stamp: 'jmv 1/15/2021 16:51:21'!
processEventQueue
	"Process user input events from the local input devices.
	Answer true if any event was handled (but ignore MouseMove)"

	| evt evtBuf type hadAny mcs hadAnyMouseEvent |
	mcs _ mouseClickState.
	hadAny := false.
	hadAnyMouseEvent := false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		type := evtBuf first.
		evt := self createEventFrom: evtBuf ofType: type.
		evt ifNotNil: [
			"Finally, handle it"
			self startEventDispatch: evt.
			hadAny := true.
			evt isMouse ifTrue: [
				hadAnyMouseEvent := true ]]].
	hadAnyMouseEvent ifFalse: [
		mcs 
			ifNotNil: [ 
				"No mouse events during this cycle. Make sure click states time out accordingly"
				mcs
					handleEvent: (self lastMouseEvent asMouseMove: (Time localMillisecondClock - self lastMouseEventTime max: 0))
					from: self ]].
	^hadAny! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 1/15/2021 16:14:27'!
mainLoop

	
	self clearWaitDelay.
	self setCanvas.
	[
		self doOneCycle.
		true ]
			whileTrue: []! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 1/15/2021 17:04:27'!
doOneCycle
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil a |
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	(lastCycleHadAnyEvent or: [ deferredUIMessages isEmpty not ])
		ifTrue: [
			pause _ 20.				"This value will only be used later, when there are no more events to serve or deferred UI messages to process."
			wait _ 0.				"Don't wait this time"]
		ifFalse: [
			"wait between 20 and 200 milliseconds"
			(hands anySatisfy: [ :h | h waitingForMoreClicks ])
				ifTrue: [ pause _ 20 ]
				ifFalse: [ pause < 200 ifTrue: [ pause _ pause * 21//20 ] ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].
			wait _ waitUntil - Time localMillisecondClock max: 0 ].
	Preferences serverMode
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait = 0
		ifTrue: [ Processor yield ]
		ifFalse: [
			waitDelay beingWaitedOn
				ifFalse: [ waitDelay setDelay: wait; wait ]
				ifTrue: [
					"If we are called from a different process than that of the main UI, we might be called in the main
					interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
					(Delay forMilliseconds: wait) wait ]].

	"Record start time of this cycle, and do cycle"
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNow.! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 1/15/2021 16:51:48'!
doOneCycleNow
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| hadAny |
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	DisplayScreen checkForNewScreenSize.
	self displayWorldSafely.
	"Run steps, alarms and deferred UI messages"
	self runStepMethods.
	"Process user input events. Run all event triggered code."
	hadAny _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAny _ hadAny | h processEventQueue.
		activeHand _ nil ].
	"The default is the primary hand"
	activeHand _ self hands first.
	^ hadAny.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| guiRootObject |
Utilities authorInitialsPerSe ifNil: [ Utilities setAuthor ].
(nil confirm: 'We need to restart the User Interface process.
You''ll need to do [Install New Updates] again, to install later updates.') ifFalse: [ self halt ].
[
	guiRootObject _ UISupervisor ui.
	UISupervisor stopUIProcess.
	UISupervisor spawnNewMorphicProcessFor: guiRootObject.
	(Delay forSeconds: 1) wait.
	ChangeSet installing: '4524-MorphicEventsCleanup-JuanVuletich-2021Jan15-16h08m-jmv.003.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '4524') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Done updating Morphic ui process code.' print.
	'Installed ChangeSet: 4524-MorphicEventsCleanup-JuanVuletich-2021Jan15-16h08m-jmv.003.cs.st' print.
	'Please do [Install New Updates] again.' print.
] forkAt: 41!

