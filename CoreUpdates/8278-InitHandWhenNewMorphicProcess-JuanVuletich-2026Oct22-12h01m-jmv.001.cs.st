'From Cuis7.9 [latest update: #8277] on 22 October 2026 at 12:01:10 pm'!

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 22/Oct/2026, 3:01:02 pm (UTC)'!
runProcess: createNewCanvas
	
	^[
		createNewCanvas ifTrue: [
			self position: `0@0` extent: Display extent.
			"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."
			self setupCanvas.
			self restoreDisplay ].
		"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
		Might happen, especially on RasPi or other systems without an RTC"
		lastCycleTime := Time localMillisecondClock.
		"Correct scheduled time for existing alarms (or their alarm time would be pushed far far into the future)."
		alarms do: [:alarm | alarm scheduledTime: alarm scheduledTime - lastAlarmTime ].
		lastAlarmTime := 0.
		activeHand initForEvents.
		self mainLoop
	] newProcess.! !

