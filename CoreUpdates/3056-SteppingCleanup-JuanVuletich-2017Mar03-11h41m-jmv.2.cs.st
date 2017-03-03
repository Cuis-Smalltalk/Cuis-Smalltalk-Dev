'From Cuis 5.0 of 7 November 2016 [latest update: #3053] on 3 March 2017 at 11:55:19 am'!

!StepMessage methodsFor: 'evaluating' stamp: 'jmv 3/3/2017 11:55:15'!
valueAtTime: millisecondClock

	|  nArgs millisecondsSinceLast |
	numArgs ifNil: [numArgs _ selector numArgs].
	nArgs _ arguments ifNil: [0] ifNotNil: [arguments size].
	lastEvaluationTime ifNil: [ lastEvaluationTime _ millisecondClock ].
	millisecondsSinceLast _ millisecondClock - lastEvaluationTime.
	lastEvaluationTime _ millisecondClock.
	nArgs = numArgs ifTrue: [
		"Ignore extra argument"
		^self value ].
	^arguments
		ifNil: [ receiver perform: selector with: millisecondsSinceLast]
		ifNotNil: [ receiver perform: selector withArguments: (arguments copyWith: millisecondsSinceLast) ]! !


!PasteUpMorph methodsFor: 'caching' stamp: 'jmv 3/3/2017 11:42:44'!
releaseCachedState
	super releaseCachedState.
	backgroundImage _ nil.
	self isWorldMorph ifTrue: [
		worldState cleanseStepList.
		worldState clearCanvas ]! !


!WorldState methodsFor: 'alarms' stamp: 'jmv 3/3/2017 11:54:00'!
removeAlarm: aSelector for: aTarget 
	"Remove the alarm with the given selector"

	| alarm |
	alarm _ alarms 
				detect: [ :any | any receiver == aTarget and: [any selector == aSelector]]
				ifNone: nil.
	alarm ifNotNil: [ alarms remove: alarm ]! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 3/3/2017 11:51:16'!
cleanseStepList
	"Remove morphs from the step list that are not in this World."

	| deletions |
	deletions _ OrderedCollection new.
	stepList do: [ :entry |
		entry receiver world == world ifFalse: [
			deletions add: entry]].
	deletions do: [ :entry|
		stepList remove: entry ].

	deletions _ OrderedCollection new.
	alarms do: [ :entry |
		((entry receiver is: #Morph) and: [ entry receiver world == world ]) ifFalse: [
			deletions add: entry]].
	deletions do: [ :entry|
		alarms remove: entry ]! !

!methodRemoval: PasteUpMorph #cleanseStepList!
PasteUpMorph removeSelector: #cleanseStepList!
