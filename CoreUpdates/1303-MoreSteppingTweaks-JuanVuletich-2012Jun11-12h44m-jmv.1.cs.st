'From Cuis 4.0 of 21 April 2012 [latest update: #1302] on 11 June 2012 at 12:46:49 pm'!
!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent '
	classVariableNames: 'DeferredUIMessages MaxCycleLapse MinCycleLapse '
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!WorldState methodsFor: 'initialization' stamp: 'jmv 6/11/2012 11:32'!
initialize

	hands _ #().
	damageRecorder_ DamageRecorder new.
	stepList _ Heap sortBlock: self stepListSortBlock.
	lastStepTime _ 0.
	lastAlarmTime _ 0.
	drawingFailingMorphs _ IdentitySet new.
	lastCycleHadAnyEvent _ false! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 6/11/2012 12:46'!
doOneCycleFor: aWorld
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a MaxCycleLapse delay before the ui gets responsive again."

	| thisPause now wait |
	thisPause _ MinCycleLapse.
	"fix in a postcript and remove"
	lastCycleHadAnyEvent ifNil: [ lastCycleHadAnyEvent _ false ].
	lastCycleHadAnyEvent
		ifTrue: [  thisPause _ MinCycleLapse ]
		ifFalse: [
			thisPause < MaxCycleLapse		"No events processed? Start saving CPU!!"
				ifTrue: [
					thisPause _ thisPause * 21//20 ]].
	"Check for unnecessary calls to #millisecondClockValue"
	now _ Time millisecondClockValue.
	alarms ifNotNil: [
		alarms isEmpty not ifTrue: [
			thisPause _ thisPause min: (alarms first scheduledTime - now) ]].
	stepList isEmpty not ifTrue: [
		thisPause _ thisPause min: stepList first scheduledTime - now ].

	"thisPause is set to very short if we have events already enqueued, so they are processed quickly."
	(Preferences higherPerformance or: [ Sensor eventQueue isEmpty not ]) 
		ifTrue: [ thisPause _ 1 ].


""
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	wait _ lastCycleTime notNil
		ifTrue: [ lastCycleTime + thisPause - Time millisecondClockValue ]
		ifFalse: [ 0 ].
	Preferences serverMode
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	(wait > 0 and: [ wait <= thisPause ])
		ifTrue: [
			waitDelay beingWaitedOn
				ifFalse: [ waitDelay setDelay: wait; wait ]
				ifTrue: [
					"If we are called from a different process than that of the main UI, we might be called in the main
					interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
					(Delay forMilliseconds: wait) wait ]].

	lastCycleTime _  Time millisecondClockValue.
""


	lastCycleHadAnyEvent _ self doOneCycleNowFor: aWorld.! !

!methodRemoval: WorldState #interCyclePause:!
WorldState removeSelector: #interCyclePause:!
!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent'
	classVariableNames: 'DeferredUIMessages MaxCycleLapse MinCycleLapse'
	poolDictionaries: ''
	category: 'Morphic-Worlds'!
