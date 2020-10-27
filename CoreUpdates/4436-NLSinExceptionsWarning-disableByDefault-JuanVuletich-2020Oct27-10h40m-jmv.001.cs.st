'From Cuis 5.0 [latest update: #4426] on 27 October 2020 at 10:41:33 am'!

!Preferences class methodsFor: 'exception handling' stamp: 'jmv 10/27/2020 10:41:23'!
warnAboutNonLocalReturnsInExceptionHandlers
	^ self
		valueOfFlag: #warnAboutNonLocalReturnsInExceptionHandlers
		ifAbsent: [ false ].! !

