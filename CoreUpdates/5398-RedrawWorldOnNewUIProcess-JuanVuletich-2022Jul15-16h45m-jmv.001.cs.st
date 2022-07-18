'From Cuis 6.0 [latest update: #5397] on 15 July 2022 at 4:47:21 pm'!

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 7/15/2022 16:46:11'!
mainLoop
	| d |

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime _ Time localMillisecondClock.
	lastAlarmTime _ 0.

	canvas isNil ifTrue: [
		self setMainCanvas ].
	self displayWorld.
	d _ Delay forMilliseconds: 50.
	[
		self doOneCycle: d.
		UISupervisor isUIProcessRunning ]
			whileTrue: []! !

