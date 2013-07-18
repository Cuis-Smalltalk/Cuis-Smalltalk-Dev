'From Cuis 4.1 of 12 December 2012 [latest update: #1681] on 17 April 2013 at 12:55:33 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 4/17/2013 12:45'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."
	aMouseButtonEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		^self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition ].

	self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue:[
		self startStepping: #processMouseStillDown:localPosition:
			at: Time localMillisecondClock + self mouseStillDownThreshold
			arguments: {aMouseButtonEvent copy resetHandlerFields . localEventPosition}
			stepTime: self mouseStillDownStepRate ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 4/17/2013 12:45'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"System level event handling."

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self ]) ifFalse: [^self].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time localMillisecondClock
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1]! !

!Morph methodsFor: 'stepping and presenter' stamp: 'jmv 4/17/2013 12:45'!
startStepping
	"Start getting sent the 'step' message."
	self startStepping: #stepAt: at: Time localMillisecondClock arguments: nil stepTime: nil! !

!Morph methodsFor: 'stepping and presenter' stamp: 'jmv 4/17/2013 12:45'!
startSteppingSelector: aSelector
	"Start getting sent the 'step' message."
	self startStepping: aSelector at: Time localMillisecondClock arguments: nil stepTime: nil! !


!HierarchicalListMorph methodsFor: 'events-processing' stamp: 'jmv 4/17/2013 12:45'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time localMillisecondClock
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1]! !


!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 4/17/2013 12:45'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Re-implemented to allow for mouse-up move events"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time localMillisecondClock
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1]! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 4/17/2013 12:45'!
startBlinking
	"And show the cursor"
	pauseBlinking _ true.
	"Start blinking in a short while"
	paragraph ifNotNil: [ paragraph showCaret: true ].
	self
		startStepping: #onBlinkCursor
		at: Time localMillisecondClock
		arguments: nil
		stepTime: 500! !


!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 4/17/2013 12:44'!
monitor: anObjectExplorerWrapper
	"Start stepping and watching the given wrapper for changes."
	anObjectExplorerWrapper ifNil: [ ^self ].
	self monitorList at: anObjectExplorerWrapper put: anObjectExplorerWrapper asString.
	self runningWorld startStepping: self at: Time localMillisecondClock selector: #step arguments: #() stepTime: 200! !


!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 4/17/2013 12:45'!
startBlinking
	"And show the cursor"
	pauseBlinking _ true.
	"Start blinking in a short while"
	showCaret _ true.
	self
		startStepping: #onBlinkCursor
		at: Time localMillisecondClock
		arguments: nil
		stepTime: 500! !


!PasteUpMorph methodsFor: 'stepping' stamp: 'jmv 4/17/2013 12:44'!
startStepping: aMorph
	"Add the given morph to the step list. Do nothing if it is already being stepped."
	^self startStepping: aMorph at: Time localMillisecondClock selector: #stepAt: arguments: nil stepTime: nil! !


!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 4/17/2013 12:46'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time localMillisecondClock
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition }
			stepTime: 1]! !


!WorldState methodsFor: 'alarms' stamp: 'jmv 4/17/2013 12:47'!
adjustAlarmTimes: nowTime
	"Adjust the alarm times after some clock weirdness (such as image-startup etc)"
	| deltaTime |
	deltaTime _ nowTime - lastAlarmTime.
	self alarms do:[:alarm| alarm scheduledTime: alarm scheduledTime + deltaTime]! !

!WorldState methodsFor: 'alarms' stamp: 'jmv 4/17/2013 12:46'!
triggerAlarmsBefore: nowTime
	"Trigger all pending alarms that are to be executed before nowTime."
	| pending |
	lastAlarmTime ifNil: [ lastAlarmTime _ nowTime ].
	nowTime - lastAlarmTime > 10000
		ifTrue: [ self adjustAlarmTimes: nowTime ].
	pending _ self alarms.
	[pending isEmpty not and: [pending first scheduledTime < nowTime]]
		whileTrue: [ pending removeFirst value: nowTime ].
	lastAlarmTime _ nowTime! !

!WorldState methodsFor: 'initialization' stamp: 'jmv 4/17/2013 12:53'!
initialize

	hands _ #().
	damageRecorder _ DamageRecorder new.
	stepList _ Heap sortBlock: self stepListSortBlock.
	lastStepTime _ 0.
	lastAlarmTime _ 0.
	drawingFailingMorphs _ WeakIdentitySet new.
	pause _ 20.
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ false! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 4/17/2013 12:42'!
runLocalStepMethodsIn: aWorld 
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world."

	| now morphToStep stepTime |
	now _ Time localMillisecondClock.
	self triggerAlarmsBefore: now.
	stepList isEmpty 
		ifTrue: [ ^self].
	[ stepList isEmpty not and: [ stepList first scheduledTime < now ]] 
		whileTrue: [
			lastStepMessage _ stepList removeFirst.
			morphToStep _ lastStepMessage receiver.
			(morphToStep shouldGetStepsFrom: aWorld) 
				ifTrue: [
					lastStepMessage value: now.
					lastStepMessage ifNotNil: [
							stepTime _ lastStepMessage stepTime ifNil: [ morphToStep stepTime ].
							lastStepMessage scheduledTime: now + (stepTime max: 1).
							stepList add: lastStepMessage ]].
			lastStepMessage _ nil ].
	lastStepTime _ now! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 4/17/2013 12:42'!
startStepping: aMorph at: scheduledTime selector: aSelector arguments: args stepTime: stepTime
	"Add the given morph to the step list. Do nothing if it is already being stepped."

	self stopStepping: aMorph selector: aSelector.
	stepList add:(
		StepMessage 
			scheduledAt: scheduledTime
			stepTime: stepTime
			receiver: aMorph
			selector: aSelector
			arguments: args)! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 4/17/2013 12:52'!
doOneCycleFor: aWorld
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a MaxCycleLapse delay before the ui gets responsive again."

	| wait waitUntil |
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	lastCycleHadAnyEvent
		ifTrue: [
			pause _ 20.				"This value will only be used when there are no more events to serve."
			wait _ 0 ]					"Don't wait"
		ifFalse: [
			pause < MaxCycleLapse		"No events processed? Start saving CPU!!"
				ifTrue: [ pause _ pause * 21//20 ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].

			wait _ waitUntil - Time localMillisecondClock ].
	Preferences serverMode
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait > 0 ifTrue: [
		waitDelay beingWaitedOn
			ifFalse: [ waitDelay setDelay: wait; wait ]
			ifTrue: [
				"If we are called from a different process than that of the main UI, we might be called in the main
				interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
				(Delay forMilliseconds: wait) wait ]].

	"Record start time of this cycle, and do cycle"
	lastCycleTime _  Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNowFor: aWorld! !

!methodRemoval: WorldState #adjustWakeupTimes:!
WorldState removeSelector: #adjustWakeupTimes:!
!methodRemoval: WorldState #adjustWakeupTimesIfNecessary!
WorldState removeSelector: #adjustWakeupTimesIfNecessary!
