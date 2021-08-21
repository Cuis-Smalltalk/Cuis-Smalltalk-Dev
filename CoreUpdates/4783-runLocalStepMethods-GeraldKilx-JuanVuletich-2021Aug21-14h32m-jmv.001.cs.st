'From Cuis 5.0 [latest update: #4782] on 21 August 2021 at 2:35:24 pm'!

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 8/21/2021 14:33:52'!
runLocalStepMethods: nowTime
	"Run morph 'step' methods whose time has come. Purge any morphs that are no longer in this world."

	| stepMessage |
	[ stepList notEmpty and: [ (stepMessage _ stepList first) scheduledTime <= nowTime ]] 
		whileTrue: [
			(stepMessage receiver shouldGetStepsFrom: self)
				ifFalse: [
					stepList remove: stepMessage ifAbsent: []]
				ifTrue: [
					stepMessage valueAtTime: nowTime.
					stepMessage rescheduleAfter: nowTime.
					"We've just updated the  scheduled time for stepMessage.
					It might have been that stepMessage was removed altogether from stepList.
					It also may be the case that stepList got added or removed other elements while on #valueAtTime:
					Just reSort. It will be ok in any case."
					stepList reSort ]].! !

