'From Cuis 6.0 [latest update: #5396] on 14 July 2022 at 8:01:17 pm'!

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 7/14/2022 18:27:35'!
runDeferredUIMessages
	"Processing the queue until empty is wrong if a block in it calls #addDeferredUIMessage: itself, because this loop will never end.
	Instead, process no more than entries already in queue befor we start iterating!!"
	| readyToProcess |
	readyToProcess _ deferredUIMessages size.
	readyToProcess timesRepeat: [
		deferredUIMessages nextOrNil ifNotNil: [ :block |
			block value ]].! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/14/2022 19:50:25'!
newUIProcessIfNeeded
	"If the system needs a UIProcess (we know because UIProcess is not nil),
	then ensure that the UIProcess is ready to run, in order to hava a responsive UI.
	If we needed to create a new UI process, answer the old one, as it is most likely the
	process the user is interested in debugging. See senders."

	UIProcess ifNotNil: [ :oldUIProcess |
		oldUIProcess isBlocked | oldUIProcess isSuspended | oldUIProcess isTerminated ifTrue: [
			self spawnNewMorphicProcessFor: UI.
			^oldUIProcess ]].
	^nil! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/14/2022 19:14:50'!
spawnNewMorphicProcessFor: guiRootObject
	"
	Safe to run anytime, as the old Morphic process will end by itself.
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.
	"
	"If the old one gets resumed, don't step on each other feet"
	UIProcess ifNotNil: [UIProcess priority: UIProcess priority-1].
	UI _ guiRootObject.
	UIProcess _ guiRootObject runProcess.
	UIProcess resume! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 7/14/2022 18:45:46'!
doOneCycle: delay
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	"
	'Debugging Aid. Declare Delta either as a class variable or as a global variable. Declare delta and r as locals'.
	delta _ Time localMillisecondClock - lastCycleTime.
	r _ 15@1515 extent: 60@30.
	Delta _ Delta ifNil: [ delta ] ifNotNil: [ Delta * 0.9 + (delta * 0.1) ].
	Random next > 0.9 ifTrue: [
		Display fill: r fillColor: Color white.
		(Delta printStringFractionDigits: 1) displayAt: 20@1520.
		Display forceToScreen: r ].
	"
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
	(Preferences at: #serverMode)
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait = 0
		ifTrue: [ Processor yield ]
		ifFalse: [
			delay setDelay: wait; wait ].

	self doOneCycleNow.! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 7/14/2022 19:22:12'!
doOneCycleNow
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| hadAny |
	lastCycleTime _ Time localMillisecondClock.
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	self checkForNewScreenSize.
	canvas ensureCurrentMorphIsWorld.
	self displayWorldSafely.
	"Run steps, alarms and deferred UI messages"
	self runDeferredUIMessages.
	self triggerAlarmsBefore: lastCycleTime.
	self runLocalStepMethods: lastCycleTime.
	"Process user input events. Run all event triggered code."
	hadAny _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAny _ hadAny | h processEventQueue ].
	"The default is the primary hand"
	activeHand _ self hands first.
	"Run any deferred UI messages that were enqueued during this cycle before the inter cycle pause."
	self runDeferredUIMessages.
	lastCycleHadAnyEvent _ hadAny.
	^hadAny! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/14/2022 20:00:31'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"The main canvas should only be drawn by the UI process.
	If some other process has been ser as the UI process, don't continue, as breakage is likely."
	world ifNotNil: [
		world canvas == self ifTrue: [
			UISupervisor isUIProcessRunning ifFalse: [^self]]].
	aMorph visible ifFalse: [^ self].

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self canvasToUse drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [
			(Preferences at: #cheapWindowReframe) and: [currentMorph is: #SystemWindow]]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph! !

!methodRemoval: WorldMorph #runStepMethods stamp: 'jmv 7/14/2022 19:48:56'!
WorldMorph removeSelector: #runStepMethods!
