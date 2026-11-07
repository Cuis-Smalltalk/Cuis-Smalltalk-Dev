'From Cuis7.9 [latest update: #8336] on 7 November 2026 at 9:59:40 am'!

!FileIOAccessor methodsFor: 'testing' stamp: 'pb 25/May/2016, 3:32:00 am (UTC)'!
onUnix
	^self pathNameDelimiter == $/! !

!FileIOAccessor methodsFor: 'testing' stamp: 'pb 25/May/2016, 3:32:00 am (UTC)'!
onWindows
	^self pathNameDelimiter == $\! !

