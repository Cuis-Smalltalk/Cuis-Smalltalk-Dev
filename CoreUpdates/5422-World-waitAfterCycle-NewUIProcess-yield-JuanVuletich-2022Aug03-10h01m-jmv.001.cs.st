'From Cuis 6.0 [latest update: #5421] on 3 August 2022 at 10:10:18 am'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/2/2022 17:44:37'!
openDebugging: aBlock to: aCompiledMethod label: aString
	"Create a new process to debug aBlock, and step into it."

	| processToDebug debugger |
	debugger _ Debugger new.
	processToDebug _ aBlock newProcess name: 'debugIt'.
	debugger process: processToDebug context: processToDebug suspendedContext.
	debugger openFullMorphicLabel: aString.
	[debugger interruptedContext method == aCompiledMethod]
		whileFalse: [debugger send]! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 8/2/2022 20:32:21'!
spawnNewMorphicProcessFor: guiRootObject
	"
	Safe to run anytime, as the old Morphic process will end by itself.
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.
	"
	"If the old one gets resumed, don't step on each other feet"
	UIProcess ifNotNil: [
		UIProcess
			name: 'Formerly: ', UIProcess name;
			priority: UIProcess priority-1.
		UIProcess isRunning ifTrue: [ Processor yield ]].
	UI _ guiRootObject.
	UIProcess _ guiRootObject runProcess.
	UIProcess resume! !

!UISupervisor class methodsFor: 'accessing' stamp: 'jmv 8/2/2022 18:00:40'!
animatedUIOf: aProcess
	"
	UISupervisor animatedUIOf: UIProcess
	[ (UISupervisor animatedUIOf: Processor activeProcess) print ] fork
	"
	^aProcess == UIProcess ifTrue: [ UI ]! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 8/2/2022 20:23:20'!
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
	self doOneCycleNow.

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
			delay setDelay: wait; wait ].! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 8/2/2022 20:19:56'!
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
	lastCycleHadAnyEvent _ hadAny.! !

