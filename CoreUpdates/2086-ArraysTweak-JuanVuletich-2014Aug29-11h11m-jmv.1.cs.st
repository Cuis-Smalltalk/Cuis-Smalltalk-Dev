'From Cuis 4.2 of 25 July 2013 [latest update: #2084] on 29 August 2014 at 5:46:32.770376 pm'!

!Array2D class methodsFor: 'instance creation' stamp: 'jmv 8/28/2014 14:41'!
extent: aPoint
	^self basicNew initHeight: aPoint y width: aPoint x! !


!FloatArray methodsFor: 'math functions' stamp: 'jmv 8/29/2014 11:19'!
derivative
	| answer s |
	s _ self size.
	answer _ FloatArray new: s.
	answer replaceFrom: 1 to: s-1 with: self startingAt: 2.
	answer at: s put: (answer at: s-1).
	answer -= self.
	^answer! !

