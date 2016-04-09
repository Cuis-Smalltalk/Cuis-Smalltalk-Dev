'From Cuis 4.2 of 25 July 2013 [latest update: #2729] on 9 April 2016 at 6:10:03.069976 pm'!

!Interval methodsFor: 'accessing' stamp: 'jmv 4/9/2016 16:18'!
at: i 
	"Answer the anInteger'th element."

	(i >= 1 and: [i <= count])
		ifTrue: [
			count=1 ifTrue: [ ^start ].
			^start + ((stop-start)/(count-1)*(i-1))]
		ifFalse: [self errorSubscriptBounds: i]! !

!Interval methodsFor: 'accessing' stamp: 'jmv 4/9/2016 16:00'!
increment
	"Answer the receiver's interval increment."

	^stop = start ifFalse: [stop-start / (count-1)] ifTrue:[1]! !

!Interval methodsFor: 'accessing' stamp: 'jmv 4/9/2016 15:59'!
last 
	"Refer to the comment in SequenceableCollection|last."

	^ stop! !

!Interval methodsFor: 'accessing' stamp: 'jmv 4/9/2016 16:03'!
rangeIncludes: aNumber
	"Return true if the number lies in the interval between start and stop."

	^stop >= start
		ifTrue: [ aNumber between: start and: stop ]
		ifFalse: [ aNumber between: stop and: start ]! !

!Interval methodsFor: 'accessing' stamp: 'jmv 4/9/2016 15:57'!
size

	^ count! !

!Interval methodsFor: 'comparing' stamp: 'jmv 4/9/2016 16:02'!
= otherCollection

	self == otherCollection ifTrue: [^ true].
	"(1 to: 3) = #( 1 2 3) -> false"
	otherCollection isInterval ifFalse: [ ^false ].
	^ start = otherCollection first
		and: [count = otherCollection size
			and: [self last = otherCollection last]]! !

!Interval methodsFor: 'comparing' stamp: 'jmv 4/9/2016 16:01'!
hash
	"Hash is reimplemented because = is implemented."

	^(((start hash bitShift: 2)
		bitOr: stop hash)
		bitShift: 1)
		bitOr: count! !

!Interval methodsFor: 'enumerating' stamp: 'jmv 4/9/2016 16:01'!
do: aBlock

	1 to: count do: [ :i |
		aBlock value: (self at: i) ]		! !

!Interval methodsFor: 'enumerating' stamp: 'jmv 4/9/2016 16:02'!
reverseDo: aBlock 
	"Evaluate aBlock for each element of my interval, in reverse order."
	
	count to: 1 by: -1 do: [ :i |
		aBlock value: (self at: i) ]	! !

!Interval methodsFor: 'printing' stamp: 'jmv 4/9/2016 16:23'!
printOn: aStream
	| s |
	aStream nextPut: $(;
	 print: start;
	 nextPutAll: ' to: ';
	 print: stop.
	s _ self increment.
	s ~= 1 ifTrue: [aStream nextPutAll: ' by: '; print: s].
	aStream nextPut: $)! !

!Interval methodsFor: 'private' stamp: 'jmv 4/9/2016 16:25'!
setFrom: startNumber to: stopNumber count: countInteger
	"start and stop can be integers, fractions, floats, whatever.
	Usually stop>start, but stop=start and stop<start are also possible."

	start _ startNumber.
	stop _ stopNumber.
	count _ countInteger! !

!Interval methodsFor: 'arithmetic' stamp: 'jmv 4/9/2016 16:25'!
+ number

	^ start + number to: stop + number count: count! !

!Interval methodsFor: 'arithmetic' stamp: 'jmv 4/9/2016 16:25'!
- number

	^ start - number to: stop - number count: count! !

!methodRemoval: Interval #collect:!
Interval removeSelector: #collect:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Interval allInstancesDo: [ :i | i instVarNamed: 'step' put: (i instVarNamed: 'count') ]!

