'From Cuis 4.2 of 25 July 2013 [latest update: #2284] on 1 May 2015 at 10:13:23.508699 pm'!
!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent activeHand world '
	classVariableNames: 'DeferredUIMessages MaxCycleLapse '
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!WorldState class methodsFor: 'class initialization' stamp: 'jmv 5/1/2015 20:27'!
releaseClassCachedState
	"
	WorldState releaseClassCachedState
	"

	DeferredUIMessages _ nil! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 5/1/2015 20:24'!
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
			pause < 200				"No events processed? Start saving CPU!! But never make the user wait more than 200ms for being responsive again."
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

!methodRemoval: WorldState class #initialize!
WorldState class removeSelector: #initialize!
WorldState initialize!
!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent activeHand world'
	classVariableNames: 'DeferredUIMessages'
	poolDictionaries: ''
	category: 'Morphic-Worlds'!
