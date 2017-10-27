'From Cuis 5.0 of 7 November 2016 [latest update: #3198] on 27 October 2017 at 9:25:00 am'!

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 10/27/2017 09:24:28'!
logDebuggerStackToFile
	^ self
		valueOfFlag: #logDebuggerStackToFile
		ifAbsent: [ false ]! !

