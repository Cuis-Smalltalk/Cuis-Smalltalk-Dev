'From Cuis 6.0 [latest update: #6067] on 1 November 2023 at 10:42:04 am'!

!StepMessage methodsFor: 'accessing' stamp: 'jmv 11/1/2023 10:14:50'!
stepTime: aNumber
	"Set the step time interval for this message in milliseconds.
	If nil, the receiver of the message will be asked for its #stepTime."
	stepTime := aNumber! !

!StepMessage methodsFor: 'testing' stamp: 'jmv 11/1/2023 10:13:30'!
stepTime
	"Return the step time interval for this message in milliseconds.
	If nil, the receiver of the message will be asked for its #stepTime."
	^stepTime ifNil: [ receiver stepTime asInteger ]! !

!StepMessage methodsFor: 'evaluating' stamp: 'jmv 11/1/2023 10:40:33'!
rescheduleAfter: millisecondTimer
	"Schedule next run.
	Not before millisecondTimer"
	scheduledTime := scheduledTime + self stepTime max: millisecondTimer+1! !


!StepMessage class methodsFor: 'instance creation' stamp: 'jmv 11/1/2023 10:18:48'!
receiver: aTarget selector: aSelector at: scheduledTime stepTime: timeIntervalMilliseconds

	^ (self receiver: aTarget selector: aSelector arguments: nil)
		scheduledTime: scheduledTime;
		stepTime: timeIntervalMilliseconds! !


!Morph methodsFor: 'event handling' stamp: 'jmv 11/1/2023 10:20:15'!
mouseStillDownStepRate
	"At what rate do I want to receive #mouseStillDown notifications?
	This is actually the time interval between notifications, in milliseconds."
	^1! !

!Morph methodsFor: 'stepping' stamp: 'jmv 11/1/2023 10:22:05'!
startStepping: aSelector in: waitMilliseconds stepTime: timeIntervalMillisecondsOrNil
	"Start stepping the receiver"

	self world ifNotNil: [ :w |
		w
			startStepping: self
			at: Time localMillisecondClock+waitMilliseconds
			selector: aSelector
			stepTime: timeIntervalMillisecondsOrNil.
		"?"
		"self redrawNeeded" ]! !

!Morph methodsFor: 'stepping' stamp: 'jmv 11/1/2023 10:22:46'!
startStepping: aSelector stepTime: timeIntervalMillisecondsOrNil
	"Start stepping the receiver"

	self startStepping: aSelector in: 0 stepTime: timeIntervalMillisecondsOrNil! !

!Morph methodsFor: 'stepping' stamp: 'jmv 11/1/2023 10:22:52'!
startSteppingStepTime: timeIntervalMillisecondsOrNil
	"Start stepping the receiver"

	self startStepping: #stepAt: in: 0 stepTime: timeIntervalMillisecondsOrNil! !


!WorldMorph methodsFor: 'stepping' stamp: 'jmv 11/1/2023 10:40:48'!
runLocalStepMethods: nowTime
	"Run morph 'step' methods whose time has come. Purge any morphs that are no longer in this world."

	| timerBefore timerAfter stepMessage |
	timerAfter := nowTime.
	[ stepList notEmpty and: [ (stepMessage := stepList first) scheduledTime <= nowTime ]] 
		whileTrue: [
			(stepMessage receiver shouldGetStepsFrom: self)
				ifFalse: [
					stepList remove: stepMessage ifAbsent: []]
				ifTrue: [
					timerBefore := timerAfter.
					stepMessage valueAtTime: nowTime.
					timerAfter := Time localMillisecondClock.
					"No step message should take more than 50% CPU. 
					Reschedule it only after its required time is elapsed again."
					stepMessage rescheduleAfter: timerAfter + (timerAfter - timerBefore).
					"We've just updated the  scheduled time for stepMessage.
					It might have been that stepMessage was removed altogether from stepList.
					It also may be the case that stepList got added or removed other elements while on #valueAtTime:
					Just reSort. It will be ok in any case."
					stepList reSort ]].! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 11/1/2023 10:19:08'!
startStepping: aMorph at: scheduledTime selector: aSelector stepTime: timeIntervalMillisecondsOrNil
	"Add the given morph to the step list"

	self stopStepping: aMorph selector: aSelector.
	stepList add: (
		StepMessage 
			receiver: aMorph
			selector: aSelector
			at: scheduledTime
			stepTime: timeIntervalMillisecondsOrNil)! !


!PluggableButtonMorph methodsFor: 'event handling' stamp: 'jmv 11/1/2023 10:20:11'!
mouseStillDownStepRate
	"At what rate do I want to receive #mouseStillDown notifications?
	This is actually the time interval between notifications, in milliseconds."
	^40! !

