'From Cuis 5.0 [latest update: #4230] on 23 June 2020 at 12:29:05 pm'!

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 6/23/2020 10:48:01'!
handleUserInterrupt
	Utilities reportCPUandRAM.
	Preferences cmdDotEnabled ifTrue: [
		[ UISupervisor userInterrupt ] fork]! !

