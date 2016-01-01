'From Cuis 4.2 of 25 July 2013 [latest update: #2649] on 28 December 2015 at 1:06:37.340719 pm'!

!Float64Array methodsFor: 'accessing' stamp: 'jmv 12/28/2015 12:31'!
floatAt: index put: aFloat
	"Store the argument (e.g., 64 bit Float) at the given index
	Use the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits.
	Allow subclasses to redefine #at:put:"

	self replaceWordsFrom: index * 2 - 1 to: index * 2  with: aFloat asFloat startingAt: 1.
	^aFloat! !

