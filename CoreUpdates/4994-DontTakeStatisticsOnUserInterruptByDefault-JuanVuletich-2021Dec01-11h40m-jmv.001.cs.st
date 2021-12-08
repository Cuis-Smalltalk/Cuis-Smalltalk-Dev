'From Cuis 5.0 [latest update: #4992] on 1 December 2021 at 11:41:58 am'!

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 12/1/2021 11:40:58'!
cmdDotInterruptTakesStatistics
	^ self
		valueOfFlag: #cmdDotInterruptTakesStatistics
		ifAbsent: [ false ].! !

