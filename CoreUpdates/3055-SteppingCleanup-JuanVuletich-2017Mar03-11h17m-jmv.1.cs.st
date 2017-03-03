'From Cuis 5.0 of 7 November 2016 [latest update: #3053] on 3 March 2017 at 11:41:31 am'!

!StepMessage methodsFor: 'evaluating' stamp: 'jmv 3/3/2017 11:28:02'!
rescheduleAfter: millisecondTimer
	"Schedule next run"
	scheduledTime _ scheduledTime + self stepTime max: millisecondTimer + 1! !


!WorldState methodsFor: 'stepping' stamp: 'jmv 3/3/2017 11:39:34'!
stopStepping: aMorph selector: aSelector
	"Remove the given morph from the step list."
	stepList removeAll: (stepList select:[:stepMsg| stepMsg receiver == aMorph and: [ stepMsg selector == aSelector ]])! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 3/3/2017 11:39:22'!
stopSteppingMorph: aMorph
	"Remove the given morph from the step list."
	stepList removeAll: (stepList select: [ :stepMsg | stepMsg receiver == aMorph])! !

!WorldState methodsFor: 'accessing' stamp: 'jmv 3/3/2017 11:39:10'!
runLocalStepMethods: nowTime
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world."

	| stepMessage |
	[ stepList notEmpty and: [ stepList first scheduledTime <= nowTime ]] 
		whileTrue: [

			stepMessage _ stepList first.
			(stepMessage receiver shouldGetStepsFrom: world)
				ifTrue: [
					stepMessage valueAtTime: nowTime.
					"If it was not removed from the list during its own evaluation"
					stepMessage == stepList first ifTrue: [
						stepList removeFirst.
						stepMessage rescheduleAfter: nowTime.
						stepList add: stepMessage ]]

				ifFalse: [ stepList removeFirst ].
		]! !

