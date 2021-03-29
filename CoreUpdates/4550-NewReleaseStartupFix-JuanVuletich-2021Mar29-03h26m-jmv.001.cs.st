'From Cuis 5.0 [latest update: #4549] on 29 March 2021 at 3:26:32 am'!

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 3/29/2021 03:26:19'!
mainLoop

	
	self clearWaitDelay.
	canvas isNil ifTrue: [
		self setCanvas: Display getCanvas.
		damageRecorder _ DamageRecorder new ].
	self redrawNeeded.
	[
		self doOneCycle.
		true ]
			whileTrue: []! !

