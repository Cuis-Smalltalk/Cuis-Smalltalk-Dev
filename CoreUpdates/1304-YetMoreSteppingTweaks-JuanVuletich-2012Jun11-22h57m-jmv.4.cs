'From Cuis 4.0 of 21 April 2012 [latest update: #1303] on 11 June 2012 at 11:42:12 pm'!
!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent '
	classVariableNames: 'DeferredUIMessages MaxCycleLapse MinCycleLapse '
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!WorldState commentStamp: '<historical>' prior: 0!
The state of a Morphic world.  (This needs some serious commenting!!!!)!


!FrameRateMorph methodsFor: 'drawing' stamp: 'jmv 6/11/2012 23:27'!
drawOn: aCanvas
	super drawOn: aCanvas.
	meanStepDelta ifNotNil: [
		aCanvas drawString: lastStepDelta rounded printString at: bounds topLeft font: StrikeFont default color: Color black.
		aCanvas drawString: meanStepDelta rounded printString at: bounds topLeft + (0@14) font: StrikeFont default color: Color black.
		"aCanvas drawString: lastStepStamp printString at: bounds topLeft + (0@28) font: StrikeFont default color: Color black "
		]! !

!FrameRateMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 23:30'!
stepAt: millisecondClockValue

	| n |
	lastStepStamp ifNil: [ lastStepStamp _ millisecondClockValue ].
	lastStepDelta _ millisecondClockValue - lastStepStamp.
	lastStepStamp _ millisecondClockValue.
	"This factor is a damper, to show a sort of mean of the n latest step deltas"
	meanStepDelta
		ifNil: [ meanStepDelta _ 0. n _ 0 ]
		ifNotNil: [
"			n _ (meanStepDelta / lastStepDelta between: 0.5 and: 2)
				ifTrue: [ 10 ]
				ifFalse: [10 ]."
			n _ 20 ].
	meanStepDelta _ meanStepDelta * n + lastStepDelta / (n+1).
	self redrawNeeded! !

!FrameRateMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 23:34'!
stepTime

	^20! !


!HaloMorph methodsFor: 'testing' stamp: 'jmv 6/11/2012 23:32'!
stepTime
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!!"
	^ 20! !


!HandleMorph methodsFor: 'testing' stamp: 'jmv 6/11/2012 23:32'!
stepTime
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!!"
	^ 20! !


!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 23:32'!
stepTime
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!!"
	^ 20! !


!MagnifierMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 23:32'!
stepTime
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!!"
	^ 20! !


!Taskbar methodsFor: 'stepping' stamp: 'jmv 6/11/2012 23:33'!
stepTime
	"Update often."
	^ 50! !


!WorldState methodsFor: 'initialization' stamp: 'jmv 6/11/2012 23:28'!
initialize

	hands _ #().
	damageRecorder_ DamageRecorder new.
	stepList _ Heap sortBlock: self stepListSortBlock.
	lastStepTime _ 0.
	lastAlarmTime _ 0.
	drawingFailingMorphs _ IdentitySet new.
	pause _ 20.
	lastCycleTime _ Time millisecondClockValue.
	lastCycleHadAnyEvent _ false! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 6/11/2012 23:41'!
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

			wait _ waitUntil - Time millisecondClockValue ].
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
	lastCycleTime _  Time millisecondClockValue.
	lastCycleHadAnyEvent _ self doOneCycleNowFor: aWorld! !


!WorldState class methodsFor: 'class initialization' stamp: 'jmv 6/11/2012 23:39'!
initialize
	"WorldState initialize"

	MaxCycleLapse _ 200.		"never wait more than this to react to user events"
	DeferredUIMessages _ SharedQueue new.! !

!methodRemoval: WorldState class #minCycleLapse:!
WorldState class removeSelector: #minCycleLapse:!
WorldState initialize!
!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent'
	classVariableNames: 'DeferredUIMessages MaxCycleLapse'
	poolDictionaries: ''
	category: 'Morphic-Worlds'!
!methodRemoval: Preferences class #higherPerformance!
Preferences class removeSelector: #higherPerformance!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
((World instVarNamed: 'worldState') instVarNamed: 'lastCycleTime') ifNil: [
	(World instVarNamed: 'worldState') instVarNamed: 'lastCycleTime' put: Time millisecondClockValue ].
((World instVarNamed: 'worldState') instVarNamed: 'lastCycleHadAnyEvent') ifNil: [
	(World instVarNamed: 'worldState') instVarNamed: 'lastCycleHadAnyEvent' put: false ].!

