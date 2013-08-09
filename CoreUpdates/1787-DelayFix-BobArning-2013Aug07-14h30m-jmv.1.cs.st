'From Cuis 4.2 of 25 July 2013 [latest update: #1786] on 7 August 2013 at 2:32:14.364177 pm'!

!Delay class methodsFor: 'snapshotting' stamp: 'jmv 8/7/2013 14:30'!
restoreResumptionTimes
	"Private!! Restore the resumption times of all scheduled Delays after a snapshot or clock roll-over. This method should be called only while the AccessProtect semaphore is held."

	| newBaseTime |
	newBaseTime _ Time millisecondClockValue.
	SuspendedDelays
		do: [ :d | d adjustResumptionTimeOldBase: 0 newBase: newBaseTime].
	ActiveDelay
		ifNotNil: [ActiveDelay adjustResumptionTimeOldBase: 0 newBase: newBaseTime].
	ActiveDelayStartTime _ newBaseTime! !

