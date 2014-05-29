'From Cuis 4.2 of 25 July 2013 [latest update: #1987] on 18 May 2014 at 4:41:13.702292 pm'!
!classDefinition: #Time category: #'Kernel-Chronology'!
Magnitude subclass: #Time
	instanceVariableNames: 'seconds nanos '
	classVariableNames: 'MillisecondClockMask MillisecondClockOffset '
	poolDictionaries: ''
	category: 'Kernel-Chronology'!

!Time class methodsFor: 'private' stamp: 'jmv 5/18/2014 16:40'!
clearMillisecondClockOffset
	MillisecondClockOffset _ 0! !


!DateAndTime class methodsFor: 'initialization' stamp: 'jmv 5/18/2014 16:37'!
initialize
	"
	self initialize
	"
	LastTickSemaphore := Semaphore forMutualExclusion.
	LastTick := 0! !

!methodRemoval: Time class #initializeMillisecondClockMask!
Time class removeSelector: #initializeMillisecondClockMask!
!classDefinition: #Time category: #'Kernel-Chronology'!
Magnitude subclass: #Time
	instanceVariableNames: 'seconds nanos'
	classVariableNames: 'MillisecondClockOffset'
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!methodRemoval: DateAndTime class #startUp:!
DateAndTime class removeSelector: #startUp:!
DateAndTime initialize!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk removeFromStartUpList: DateAndTime.
Time clearMillisecondClockOffset!

