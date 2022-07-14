'From Cuis 6.0 [latest update: #5393] on 14 July 2022 at 1:16:26 pm'!

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 7/14/2022 13:09:54'!
doOneCycle: delay
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	"
	'Debugging Aid. Declare Delta either as a class variable or as a global variable. Declare delta and r as locals'.
	delta _ Time localMillisecondClock - lastCycleTime.
	r _ 15@1515 extent: 60@30.
	Delta _ Delta ifNil: [ delta ] ifNotNil: [ Delta * 0.9 + (delta * 0.1) ].
	Random next > 0.9 ifTrue: [
		Display fill: r fillColor: Color white.
		(Delta printStringFractionDigits: 1) displayAt: 20@1520.
		Display forceToScreen: r ].
	"
	(lastCycleHadAnyEvent or: [ deferredUIMessages isEmpty not ])
		ifTrue: [
			pause _ 20.				"This value will only be used later, when there are no more events to serve or deferred UI messages to process."
			wait _ 0.				"Don't wait this time"]
		ifFalse: [
			"wait between 20 and 200 milliseconds"
			(hands anySatisfy: [ :h | h waitingForMoreClicks ])
				ifTrue: [ pause _ 20 ]
				ifFalse: [ pause < 200 ifTrue: [ pause _ pause * 21//20 ] ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].
			wait _ waitUntil - Time localMillisecondClock max: 0 ].
	(Preferences at: #serverMode)
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait = 0
		ifTrue: [ Processor yield ]
		ifFalse: [
			delay setDelay: wait; wait ].

	"Record start time of this cycle, and do cycle"
	canvas ensureCurrentMorphIsWorld.
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNow.! !


!WorldMorph methodsFor: 'initialization' stamp: 'jmv 7/14/2022 13:15:35'!
clearWaitDelay
	waitDelay ifNotNil: [
		waitDelay unschedule.
		waitDelay _ nil ].! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 7/14/2022 13:15:33'!
mainLoop
	| d |

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime _ Time localMillisecondClock.
	lastAlarmTime _ 0.

	canvas isNil ifTrue: [
		self setMainCanvas ].
	self redrawNeeded.
	d _ Delay forMilliseconds: 50.
	[
		self doOneCycle: d.
		UISupervisor isUIProcessRunning ]
			whileTrue: []! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui clearWaitDelay.!

