'From Cuis 5.0 of 7 November 2016 [latest update: #3426] on 19 August 2018 at 6:11:23 pm'!

!WorldState methodsFor: 'accessing' stamp: 'jmv 8/19/2018 18:11:12'!
runLocalStepMethods: nowTime
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world."

	| stepMessage |
	[ stepList notEmpty and: [ stepList first scheduledTime <= nowTime ]] 
		whileTrue: [
			stepMessage _ stepList first.
			(stepMessage receiver shouldGetStepsFrom: world)
				ifFalse: [ stepList removeFirst ]
				ifTrue: [
					stepMessage valueAtTime: nowTime.
					stepMessage rescheduleAfter: nowTime.
					"We've just updated the  scheduled time for stepMessage.
					It might have been that stepMessage was removed altogether from stepList.
					It also may be the case that stepList got added or removed other elements while on #valueAtTime:
					Just reSort. It will be ok in any case."
					stepList reSort.
					]
		]! !

