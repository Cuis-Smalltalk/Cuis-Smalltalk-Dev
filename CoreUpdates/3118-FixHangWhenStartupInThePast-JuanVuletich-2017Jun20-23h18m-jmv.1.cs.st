'From Cuis 5.0 of 7 November 2016 [latest update: #3117] on 20 June 2017 at 11:19:24 pm'!

!WorldState methodsFor: 'initialization' stamp: 'jmv 6/20/2014 20:24:55'!
clearWaitDelay
	waitDelay ifNotNil: [
		waitDelay unschedule.
		waitDelay _ nil ].
	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime _ Time localMillisecondClock.
	lastAlarmTime _ 0.! !

