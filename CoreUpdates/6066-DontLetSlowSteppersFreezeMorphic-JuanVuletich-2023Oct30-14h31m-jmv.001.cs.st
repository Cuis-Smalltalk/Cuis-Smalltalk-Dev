'From Cuis 6.0 [latest update: #6065] on 30 October 2023 at 2:31:52 pm'!

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 10/30/2023 14:11:13'!
runLocalStepMethods: nowTime
	"Run morph 'step' methods whose time has come. Purge any morphs that are no longer in this world."

	| n stepMessage |
	n := stepList size.
	[ n > 0 and: [stepList notEmpty and: [ (stepMessage := stepList first) scheduledTime <= nowTime ]]]
		whileTrue: [
			n := n - 1.
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

