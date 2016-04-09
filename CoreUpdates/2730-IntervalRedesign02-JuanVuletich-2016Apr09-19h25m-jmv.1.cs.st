'From Cuis 4.2 of 25 July 2013 [latest update: #2729] on 9 April 2016 at 7:25:52.558839 pm'!

!Interval class methodsFor: 'instance creation' stamp: 'jmv 4/9/2016 16:41'!
from: startInteger to: stopInteger 
	"Answer an instance of me, starting at startNumber, ending at 
	stopNumber, and with an interval increment of 1."

	^self basicNew
		setFrom: startInteger
		to: stopInteger
		count: (stopInteger-startInteger) abs + 1! !

!Interval class methodsFor: 'instance creation' stamp: 'jmv 4/9/2016 19:25'!
from: start to: stop count: n
	"Answer an instance of me, starting at start, ending at 
	stop, and with n elements."

	^self basicNew setFrom: start to: stop count: n! !

!Interval class methodsFor: 'instance creation' stamp: 'jmv 4/9/2016 19:25'!
integersFrom: start to: stop count: n
	"Answer an instance of me, starting at start, ending at 
	stop, and with n elements. All the elements will be integer numbers."

	| answer |
	answer _ self from: start asInteger to: stop asInteger count: n.
	(answer increment isKindOf: Fraction)
		ifTrue: [answer _ answer collect: [ :each | each rounded ]].
	^answer! !

!Interval class methodsFor: 'instance creation' stamp: 'jmv 4/9/2016 16:40'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

    | newInterval n |

    (n := aCollection size) <= 1 ifTrue: [
		n = 0 ifTrue: [^self from: 1 to: 0].
		^self from: aCollection first to: aCollection last].
    	newInterval := self from: aCollection first to: aCollection last by: (aCollection last - aCollection first) // (n - 1).
	aCollection ~= newInterval
		ifTrue: [self error: 'The argument is not an arithmetic progression'].
	^newInterval

"	Interval newFrom: {1. 2. 3}
	{33. 5. -23} as: Interval
	{33. 5. -22} as: Interval    (an error)
	(-4 to: -12 by: -1) as: Interval
"! !

!methodRemoval: Interval class #new!
Interval class removeSelector: #new!
