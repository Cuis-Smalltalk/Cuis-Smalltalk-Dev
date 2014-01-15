'From Cuis 4.2 of 25 July 2013 [latest update: #1933] on 10 January 2014 at 10:47:24.45138 am'!

!Float methodsFor: 'printing' stamp: 'jmv 1/10/2014 10:45'!
printOn: aStream fractionDigits: placesDesired
	"This implementation avoids any rounding error caused by rounded or roundTo:
	The approach is simple. Round to appropriate integer, take the digits, and just 
	add the decimal point in the appropriate place."

	| i s scaled |
	self isFinite ifFalse: [ ^self printOn: aStream ].

	placesDesired <= 0 ifTrue: [
		^self rounded printOn: aStream ].

	scaled _ self * (10 raisedTo: placesDesired).
	scaled isFinite ifFalse: [ ^self printOn: aStream ].
	i _ scaled rounded.
	i negative ifTrue: [
		aStream nextPut: $-.
		i _ i negated ].
	s _ i printString.
	placesDesired + 1 > s size
		ifTrue: [
			aStream nextPutAll: '0.'.
			placesDesired - s size timesRepeat: [ aStream nextPut: $0 ].
			aStream nextPutAll: s ]
		ifFalse: [
			aStream
				nextPutAll: (s copyFrom: 1 to: s size-placesDesired);
				nextPut: $.;
				nextPutAll: (s copyFrom: s size-placesDesired+1 to: s size) ]! !

