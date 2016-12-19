'From Cuis 5.0 of 7 November 2016 [latest update: #3005] on 19 December 2016 at 1:12:20 pm'!

!SmallInteger class methodsFor: 'constants' stamp: 'jmv 12/19/2016 13:06:38'!
maxVal
	"Answer the maximum value for a SmallInteger."

	"Ensure word size is properly set. If so, maxVal is also set."
	Smalltalk wordSize.
	^maxVal! !

!SmallInteger class methodsFor: 'constants' stamp: 'jmv 12/19/2016 13:07:24'!
minVal
	"Answer the minimum value for a SmallInteger."

	"Ensure word size is properly set. If so, minVal is also set."
	Smalltalk wordSize.
	^minVal! !

