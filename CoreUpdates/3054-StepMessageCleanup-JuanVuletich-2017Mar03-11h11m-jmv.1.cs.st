'From Cuis 5.0 of 7 November 2016 [latest update: #3053] on 3 March 2017 at 11:17:47 am'!
!classDefinition: #StepMessage category: #'Morphic-Events'!
MorphicAlarm subclass: #StepMessage
	instanceVariableNames: 'stepTime lastEvaluationTime '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!StepMessage methodsFor: 'evaluating' stamp: 'jmv 3/3/2017 11:14:28'!
valueAtTime: millisecondClock

	|  nArgs millisecondsSinceLast |
	numArgs ifNil: [numArgs _ selector numArgs].
	nArgs _ arguments ifNil: [0] ifNotNil: [arguments size].
"OJO!!"
lastEvaluationTime ifNil: [ lastEvaluationTime _ millisecondClock ].
	millisecondsSinceLast _ millisecondClock - lastEvaluationTime.
	lastEvaluationTime _ millisecondClock.
	nArgs = numArgs ifTrue: [
		"Ignore extra argument"
		^self value ].
	^arguments
		ifNil: [ receiver perform: selector with: millisecondsSinceLast]
		ifNotNil: [ receiver perform: selector withArguments: (arguments copyWith: millisecondsSinceLast) ]! !


!WorldState methodsFor: 'accessing' stamp: 'jmv 3/3/2017 11:16:08'!
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
					lastStepMessage valueAtTime: nowTime.
					lastStepMessage ifNotNil: [
						lastStepMessage scheduledTime: (scheduledTime + lastStepMessage stepTime max: nowTime + 1).
						stepList add: lastStepMessage ]].
			lastStepMessage _ nil ]! !

!classDefinition: #StepMessage category: #'Morphic-Events'!
MorphicAlarm subclass: #StepMessage
	instanceVariableNames: 'stepTime lastEvaluationTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!StepMessage reorganize!
('accessing' stepTime:)
('printing' printOn:)
('testing' stepTime)
('evaluating' valueAtTime:)
!

