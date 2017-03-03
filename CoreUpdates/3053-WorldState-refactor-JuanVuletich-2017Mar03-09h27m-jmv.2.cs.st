'From Cuis 5.0 of 7 November 2016 [latest update: #3049] on 3 March 2017 at 9:49:08 am'!

!WorldState methodsFor: 'accessing' stamp: 'jmv 3/3/2017 09:47:13'!
runLocalStepMethods: nowTime
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world."

	| morphToStep scheduledTime |
	[ stepList notEmpty and: [ stepList first scheduledTime <= nowTime ]] 
		whileTrue: [
			lastStepMessage _ stepList removeFirst.
			morphToStep _ lastStepMessage receiver.
			(morphToStep shouldGetStepsFrom: world) 
				ifTrue: [
					scheduledTime _ lastStepMessage scheduledTime.
					lastStepMessage valueAtTime: nowTime - scheduledTime + lastStepMessage stepTime.
					lastStepMessage ifNotNil: [
							lastStepMessage scheduledTime: (scheduledTime + lastStepMessage stepTime max: nowTime + 1).
							stepList add: lastStepMessage ]].
			lastStepMessage _ nil ]! !


!WorldState methodsFor: 'alarms' stamp: 'jmv 3/3/2017 09:33:59'!
addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime
	"Add a new alarm with the given set of parameters"

	alarms add: 
		(MorphicAlarm 
			receiver: aTarget
			selector: aSelector
			arguments: argArray
			at: scheduledTime)! !

!WorldState methodsFor: 'alarms' stamp: 'jmv 3/3/2017 09:34:03'!
adjustAlarmTimes: nowTime
	"Adjust the alarm times after some clock weirdness (such as image-startup etc)"
	| deltaTime |
	deltaTime _ nowTime - lastAlarmTime.
	alarms do: [ :alarm |
		alarm scheduledTime: alarm scheduledTime + deltaTime ]! !

!WorldState methodsFor: 'alarms' stamp: 'jmv 3/3/2017 09:34:21'!
removeAlarm: aSelector for: aTarget 
	"Remove the alarm with the given selector"

	| alarm |
	alarm := alarms 
				detect: [ :any | any receiver == aTarget and: [any selector == aSelector]]
				ifNone: nil.
	alarm ifNotNil: [ alarms remove: alarm ]! !

!WorldState methodsFor: 'alarms' stamp: 'jmv 3/3/2017 09:35:03'!
triggerAlarmsBefore: nowTime
	"Trigger all pending alarms that are to be executed before nowTime."

	nowTime - lastAlarmTime > 10000
		ifTrue: [ self adjustAlarmTimes: nowTime ].
	[ alarms notEmpty and: [ alarms first scheduledTime < nowTime ]]
		whileTrue: [ alarms removeFirst valueAtTime: nowTime ].
	lastAlarmTime _ nowTime! !

!WorldState methodsFor: 'initialization' stamp: 'jmv 3/3/2017 09:33:53'!
initialize

	activeHand _ HandMorph new.
	hands _ { activeHand }.
	damageRecorder _ DamageRecorder new.
	stepList _ Heap sortBlock: self stepListSortBlock.
	alarms _ Heap sortBlock: self alarmSortBlock.
	lastAlarmTime _ 0.
	drawingFailingMorphs _ WeakIdentitySet new.
	pause _ 20.
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ false! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 3/3/2017 09:39:18'!
cleanseStepList
	"Remove morphs from the step list that are not in this World."

	| deletions morphToStep |
	deletions _ nil.
	stepList do: [:entry |
		morphToStep _ entry receiver.
		morphToStep world == world ifFalse:[
			deletions ifNil: [deletions _ OrderedCollection new].
			deletions addLast: entry]].

	deletions ifNotNil:[
		deletions do: [:entry|
			self stopSteppingMorph: entry receiver]].

	alarms copy do: [ :entry |
		morphToStep _ entry receiver.
		((morphToStep is: #Morph) and: [ morphToStep world == world ]) 
			ifFalse: [ self removeAlarm: entry selector for: entry receiver ]]! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 3/3/2017 09:46:37'!
runStepMethods
	"Perform periodic activity inbetween event cycles"
	| queue msg |

	queue _ self class deferredUIMessages.
	[ (msg _ queue nextOrNil) isNil ] whileFalse: [
		msg value
	].
	self triggerAlarmsBefore: lastCycleTime.
	self runLocalStepMethods: lastCycleTime.

	"we are using a normal #step for these now"
	"aWorld allLowerWorldsDo: [ :each | each runLocalStepMethods ]."
! !

!methodRemoval: WorldState #alarms!
WorldState removeSelector: #alarms!
!methodRemoval: WorldState #runLocalStepMethods!
WorldState removeSelector: #runLocalStepMethods!
