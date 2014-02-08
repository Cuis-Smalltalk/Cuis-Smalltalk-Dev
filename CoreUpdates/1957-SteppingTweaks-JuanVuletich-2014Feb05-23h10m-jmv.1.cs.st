'From Cuis 4.2 of 25 July 2013 [latest update: #1955] on 5 February 2014 at 11:14:38.576454 pm'!

!MorphicAlarm class methodsFor: 'instance creation' stamp: 'jmv 2/5/2014 23:11'!
receiver: aTarget selector: aSelector arguments: argArray at: scheduledTime

	^ (self receiver: aTarget selector: aSelector arguments: argArray)
		scheduledTime: scheduledTime! !


!StepMessage class methodsFor: 'instance creation' stamp: 'jmv 2/5/2014 23:11'!
receiver: aTarget selector: aSelector at: scheduledTime stepTime: stepTime

	^ (self receiver: aTarget selector: aSelector arguments: nil)
		scheduledTime: scheduledTime;
		stepTime: stepTime! !


!WorldState methodsFor: 'alarms' stamp: 'jmv 2/5/2014 23:11'!
addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime
	"Add a new alarm with the given set of parameters"

	self alarms add: 
		(MorphicAlarm 
			receiver: aTarget
			selector: aSelector
			arguments: argArray
			at: scheduledTime)! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 2/3/2014 21:15'!
startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTimeOrNil
	"Add the given morph to the step list"

	self stopStepping: aMorph selector: aSelector.
	stepList add: (
		StepMessage 
			receiver: aMorph
			selector: aSelector
			at: scheduledTime
			stepTime: stepTimeOrNil)! !

!methodRemoval: StepMessage class #scheduleReceiver:selector:at:stepTime:!
StepMessage class removeSelector: #scheduleReceiver:selector:at:stepTime:!
!methodRemoval: MorphicAlarm class #scheduleReceiver:selector:arguments:at:!
MorphicAlarm class removeSelector: #scheduleReceiver:selector:arguments:at:!
