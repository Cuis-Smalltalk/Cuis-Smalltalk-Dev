'From Cuis 4.2 of 25 July 2013 [latest update: #2555] on 25 October 2015 at 8:25:14.729366 pm'!

!StepMessage methodsFor: 'testing' stamp: 'jmv 10/24/2015 23:30'!
stepTime
	"Return the step time for this message. If nil, the receiver of the message will be asked for its #stepTime."
	^stepTime ifNil: [ receiver stepTime ]! !


!WorldState methodsFor: 'initialization' stamp: 'jmv 10/25/2015 19:26'!
initialize

	hands _ #().
	damageRecorder _ DamageRecorder new.
	stepList _ Heap sortBlock: self stepListSortBlock.
	lastAlarmTime _ 0.
	drawingFailingMorphs _ WeakIdentitySet new.
	pause _ 20.
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ false! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 10/25/2015 20:12'!
runLocalStepMethodsIn: aWorld 
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world."

	| now morphToStep scheduledTime |
	now _ lastCycleTime.
	self triggerAlarmsBefore: now.
	stepList isEmpty 
		ifTrue: [ ^self].
	[ stepList isEmpty not and: [ stepList first scheduledTime <= now ]] 
		whileTrue: [
			lastStepMessage _ stepList removeFirst.
			morphToStep _ lastStepMessage receiver.
			(morphToStep shouldGetStepsFrom: aWorld) 
				ifTrue: [
					scheduledTime _ lastStepMessage scheduledTime.
					lastStepMessage valueAtTime: now.
					lastStepMessage ifNotNil: [
							lastStepMessage scheduledTime: (scheduledTime + lastStepMessage stepTime max: now + 1).
							stepList add: lastStepMessage ]].
			lastStepMessage _ nil ]! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 10/25/2015 20:09'!
doOneCycleFor: aWorld
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	lastCycleHadAnyEvent
		ifTrue: [
			pause _ 20.				"This value will only be used when there are no more events to serve."
			wait _ 0 ]					"Don't wait"
		ifFalse: [
			(alarms notNil and: [alarms notEmpty])
				ifTrue: [
					pause _ 20 ] 		"don' t give more than 20ms resolution to alarms"
				ifFalse: [
					stepList notEmpty
						ifTrue: [	"This can be optimized!! We don' t need to do it each time"
							pause _ stepList inject: 1000 into: [ :p :e | p min: e stepTime ].
							]
						ifFalse: [
							pause < 200		"Start saving CPU!! But never make the user wait more than 200ms for being responsive again."
								ifTrue: [ pause _ pause * 21//20 ].
							].
					].
			waitUntil _ lastCycleTime + pause.
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
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNowFor: aWorld! !

