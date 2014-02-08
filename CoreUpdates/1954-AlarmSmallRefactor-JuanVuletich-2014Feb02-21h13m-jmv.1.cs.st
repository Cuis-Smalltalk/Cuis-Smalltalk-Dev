'From Cuis 4.2 of 25 July 2013 [latest update: #1952] on 2 February 2014 at 9:26:31.793635 pm'!

!MorphicAlarm methodsFor: 'evaluating' stamp: 'jmv 2/2/2014 21:21'!
valueAtTime: millisecondClock
"aca esta la sanata fea para ponerle el argumento a #stepAt:"
"al reconvertir, intentar evitar el falso polimorfismo. Llamar quizas #valueAt: o algo asi..."
"creo que en todos los casos va a ser nil..."
	| nArgs |
	numArgs ifNil:[numArgs _ selector numArgs].
	nArgs _ arguments ifNil:[0] ifNotNil:[arguments size].
	nArgs = numArgs ifTrue:[
		"Ignore extra argument"
		^self value].
	^arguments
		ifNil: [ receiver perform: selector with: millisecondClock]
		ifNotNil: [ receiver perform: selector withArguments: (arguments copyWith: millisecondClock) ]! !


!WorldState methodsFor: 'alarms' stamp: 'jmv 2/2/2014 21:17'!
triggerAlarmsBefore: nowTime
	"Trigger all pending alarms that are to be executed before nowTime."
	| pending |
	lastAlarmTime ifNil: [ lastAlarmTime _ nowTime ].
	nowTime - lastAlarmTime > 10000
		ifTrue: [ self adjustAlarmTimes: nowTime ].
	pending _ self alarms.
	[ pending isEmpty not and: [ pending first scheduledTime < nowTime ]]
		whileTrue: [ pending removeFirst valueAtTime: nowTime ].
	lastAlarmTime _ nowTime! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:16'!
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
					lastStepMessage valueAtTime: now.
					lastStepMessage ifNotNil: [
							stepTime _ lastStepMessage stepTime ifNil: [ morphToStep stepTime ].
							lastStepMessage scheduledTime: now + (stepTime max: 1).
							stepList add: lastStepMessage ]].
			lastStepMessage _ nil ].
	lastStepTime _ now! !

!methodRemoval: MorphicAlarm #value:!
MorphicAlarm removeSelector: #value:!
