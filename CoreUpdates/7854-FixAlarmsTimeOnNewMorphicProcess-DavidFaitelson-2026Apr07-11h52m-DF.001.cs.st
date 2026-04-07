'From Cuis7.7 [latest update: #7851] on 7 April 2026 at 11:56:15 am'!

!WorldMorph methodsFor: 'interaction loop' stamp: 'DF 7/Apr/2026 11:55:44'!
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
		self mainLoop
	] newProcess.! !

