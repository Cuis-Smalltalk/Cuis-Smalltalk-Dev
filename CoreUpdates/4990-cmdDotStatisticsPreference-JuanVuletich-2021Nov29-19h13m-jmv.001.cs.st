'From Cuis 5.0 [latest update: #4989] on 29 November 2021 at 7:15:25 pm'!

!Preferences class methodsFor: 'standard queries'!
cmdDotInterruptTakesStatistics
	^ self
		valueOfFlag: #cmdDotInterruptTakesStatistics
		ifAbsent: [ true ].! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 11/29/2021 19:14:22'!
handleUserInterrupt
	| p |
	"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
	p _ Sensor shiftPressed | Preferences cmdDotInterruptTakesStatistics
		ifTrue: [Utilities reportCPUandRAM]
		ifFalse: [Processor preemptedProcess].
	Preferences cmdDotEnabled ifTrue: [
		[ UISupervisor userInterrupt: p ] fork]! !

