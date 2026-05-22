"
I represent a finite arithmetic progression.
"
Class {
	#name : 'Interval',
	#superclass : 'SequenceableCollection',
	#instVars : [
		'start',
		'stop',
		'count'
	],
	#category : 'Collections-Sequenceable',
	#package : 'Collections',
	#tag : 'Sequenceable'
}

{ #category : 'instance creation' }
Interval class >> from: startInteger to: stopInteger  [
	"Answer an instance of me, starting at startNumber, ending at 
	stopNumber, and with an interval increment of 1."

	^self basicNew
		setFrom: startInteger
		to: stopInteger
		count: (stopInteger-startInteger) abs + 1
]

{ #category : 'instance creation' }
Interval class >> from: start to: stop by: step [
	"Answer an instance of me, starting at start, ending at 
	stop, and with an interval increment of step.
	The actual interval creation uses start, stop and count, to avoid accumulation of rounding errors.
	We need to tell apart things like
		(0.0 to: 2.4 by: 0.1)	the caller wants to honor end
	from
		(0.0 to: 10.0 by: 3.0) 	the caller actually wants to end at 9.0.
		
	Before this,
		(0 to: 2.4 by: 0.1) last 
	used to answer 2.3"
	| count end |
	count := stop - start / step + 1.
	(count isFloat and: [count isWithin: 5 floatsFrom: count rounded asFloat])
		ifTrue: [
			count := count rounded.
			end := stop ]
		ifFalse: [
			count := count truncated.
			end := count-1 * step + start ].
	^self from: start to: end count: count
]

{ #category : 'instance creation' }
Interval class >> from: start to: stop count: n [
	"Answer an instance of me, starting at start, ending at 
	stop, and with n elements."

	^self basicNew setFrom: start to: stop count: n
]

{ #category : 'instance creation' }
Interval class >> integersFrom: start to: stop count: n [
	"Answer an instance of me, starting at start, ending at 
	stop, and with n elements. All the elements will be integer numbers."

	| answer |
	answer := self from: start asInteger to: stop asInteger count: n.
	answer increment isFraction
		ifTrue: [answer := answer collect: [ :each | each rounded ]].
	^answer
]

{ #category : 'instance creation' }
Interval class >> newFrom: aCollection  [
	"Answer an instance of me containing the same elements as aCollection."

    | newInterval n |

    (n := aCollection size) <= 1 ifTrue: [
		n = 0 ifTrue: [^self from: 1 to: 0].
		^self from: aCollection first to: aCollection last].
    	newInterval := self from: aCollection first to: aCollection last by: (aCollection last - aCollection first) // (n - 1).
	(newInterval hasEqualElements: aCollection)
		ifFalse: [ self error: 'The argument is not an arithmetic progression' ].
	^newInterval

"
	Interval newFrom: {1. 2. 3}
	{33. 5. -23} as: Interval
	{33. 5. -22} as: Interval.   ' (an error)'
	(-4 to: -12 by: -1) as: Interval
"
]

{ #category : 'arithmetic' }
Interval >> + number [

	^ start + number to: stop + number count: count
]

{ #category : 'arithmetic' }
Interval >> - number [

	^ start - number to: stop - number count: count
]

{ #category : 'comparing' }
Interval >> = otherCollection [

	self == otherCollection ifTrue: [^ true].
	"(1 to: 3) = #( 1 2 3) -> false"
	otherCollection isInterval ifFalse: [ ^false ].
	^ start = otherCollection first
		and: [count = otherCollection size
			and: [self last = otherCollection last]]
]

{ #category : 'adding' }
Interval >> add: newObject  [
	"Adding to an Interval is not allowed."
	self shouldNotImplement
]

{ #category : 'accessing' }
Interval >> anyOne [
 	"This message will fail for an empty Interval, super would not."
 	^self at: 1
]

{ #category : 'converting' }
Interval >> asSourceCodeInterval [

	^ SourceCodeInterval from: start to: stop
]

{ #category : 'accessing' }
Interval >> at: i  [
	"Answer the anInteger'th element."

	(i >= 1 and: [i <= count])
		ifTrue: [
			count=1 ifTrue: [ ^start ].
			^start + ((stop-start)/(count-1)*(i-1))]
		ifFalse: [self errorSubscriptBounds: i]
]

{ #category : 'accessing' }
Interval >> at: anInteger put: anObject  [
	"Storing into an Interval is not allowed."

	self error: 'you can not store into an interval'
]

{ #category : 'enumerating' }
Interval >> do: aBlock [

	1 to: count do: [ :i |
		aBlock value: (self at: i) ]		
]

{ #category : 'accessing' }
Interval >> extent  [
	"Answer the max - min of the receiver interval."
	"(10 to: 50) extent"

	^stop - start
]

{ #category : 'accessing' }
Interval >> first [
	^start
]

{ #category : 'comparing' }
Interval >> hash [
	"Hash is reimplemented because = is implemented."

	^ (start hash bitXor: stop hash) bitXor: count hash
]

{ #category : 'testing' }
Interval >> includes: aNumber [

	| index |
	
	aNumber isNumber ifFalse: [ ^ false ].
	
	^ start = stop 
		ifTrue: [ start = aNumber ]
		ifFalse: [ 
			index := (aNumber - start) / (stop-start) * (count-1) + 1.
			index isInteger and: [ index between: 1 and: count ]]
]

{ #category : 'testing' }
Interval >> includesAllOf: aCollection [

	^aCollection isInterval
		ifTrue: [ (self includes: aCollection first)
			and: [ (self includes: aCollection last)
			and: [ self increment = aCollection increment or: [ super includesAllOf:aCollection ]]]]
		ifFalse: [ super includesAllOf: aCollection ]
]

{ #category : 'accessing' }
Interval >> increment [
	"Answer the receiver's interval increment."

	^stop = start ifFalse: [stop-start / (count-1)] ifTrue:[1]
]

{ #category : 'testing' }
Interval >> intersectsRange: rightRange [
 
	^(self includes: rightRange first) 
		or: [ (self includes: rightRange last)
		or: [ rightRange includes: self first ]]
]

{ #category : 'accessing' }
Interval >> isEmpty [
	^self size = 0
]

{ #category : 'testing' }
Interval >> isInterval [

	^ true
]

{ #category : 'accessing' }
Interval >> last  [
	"Refer to the comment in SequenceableCollection|last."

	^ stop
]

{ #category : 'enumerating' }
Interval >> permutationsDo: aBlock [
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations.

	(1 to: 4) permutationsDo: [:each | Transcript newLine; show: each printString].
	"

	self asArray permutationsDo: aBlock

]

{ #category : 'printing' }
Interval >> printOn: aStream [
	| s |
	aStream nextPut: $(;
	 print: start;
	 nextPutAll: ' to: ';
	 print: stop.
	s := self increment.
	s ~= 1 ifTrue: [aStream nextPutAll: ' by: '; print: s].
	aStream nextPut: $)
]

{ #category : 'testing' }
Interval >> rangeIncludes: aNumber [
	"Return true if the number lies in the interval between start and stop."

	^stop >= start
		ifTrue: [ aNumber between: start and: stop ]
		ifFalse: [ aNumber between: stop and: start ]
]

{ #category : 'removing' }
Interval >> remove: newObject  [
	"Removing from an Interval is not allowed."

	self error: 'elements cannot be removed from an Interval'
]

{ #category : 'enumerating' }
Interval >> reverseDo: aBlock  [
	"Evaluate aBlock for each element of my interval, in reverse order."
	
	count to: 1 by: -1 do: [ :i |
		aBlock value: (self at: i) ]	
]

{ #category : 'private' }
Interval >> setFrom: startNumber to: stopNumber count: countInteger [
	"start and stop can be integers, fractions, floats, whatever.
	Usually stop>start, but stop=start and stop<start are also possible."

	start := startNumber.
	stop := stopNumber.
	count := countInteger
]

{ #category : 'accessing' }
Interval >> size [

	^ count
]

{ #category : 'private' }
Interval >> species [
	^Array
]

{ #category : 'printing' }
Interval >> storeOn: aStream  [
	"This is possible because we know numbers store and print the same"

	self printOn: aStream
]
