'From Cuis 4.2 of 25 July 2013 [latest update: #2558] on 28 October 2015 at 10:08:07.77478 am'!

!WorldState methodsFor: 'update cycle' stamp: 'jmv 10/28/2015 10:07'!
doOneCycleFor: aWorld
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	lastCycleHadAnyEvent
		ifTrue: [
			pause _ 20.				"This value will only be used when there are no more events to serve."
			wait _ 0 ]					"Don't wait"
		ifFalse: [
			pause < 200				"No events processed? Start saving CPU!! But never make the user wait more than 200ms for being responsive again."
				ifTrue: [ pause _ pause * 21//20 ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].

			wait _ waitUntil - Time localMillisecondClock ].
	Preferences serverMode
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait > 0 ifTrue: [
		waitDelay beingWaitedOn
			ifFalse: [ waitDelay setDelay: wait; wait ]
			ifTrue: [
				"If we are called from a different process than that of the main UI, we might be called in the main
				interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
				(Delay forMilliseconds: wait) wait ]].

	"Record start time of this cycle, and do cycle"
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNowFor: aWorld! !

