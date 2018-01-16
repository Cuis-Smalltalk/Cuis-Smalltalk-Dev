'From Cuis 5.0 of 7 November 2016 [latest update: #3250] on 14 January 2018 at 11:26:34 am'!

!WorldState methodsFor: 'stepping' stamp: 'jmv 1/14/2018 11:26:09'!
runStepMethods
	"Perform periodic activity inbetween event cycles"
	| queue readyToProcess |

	queue _ self class deferredUIMessages.
	"Processing the queue until empty is wrong if a block in it calls #addDeferredUIMessage: itself, because this loop will never end.
	Instead, process no more than entries already in queue befor we start iterating!!"
	readyToProcess _ queue size.
	readyToProcess timesRepeat: [
		queue nextOrNil ifNotNil: [ :block |
			block value
		]
	].
	self triggerAlarmsBefore: lastCycleTime.
	self runLocalStepMethods: lastCycleTime.

	"we are using a normal #step for these now"
	"aWorld allLowerWorldsDo: [ :each | each runLocalStepMethods ]."
! !

