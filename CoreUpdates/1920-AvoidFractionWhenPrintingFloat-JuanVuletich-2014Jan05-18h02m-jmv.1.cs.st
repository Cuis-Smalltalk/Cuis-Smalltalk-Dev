'From Cuis 4.2 of 25 July 2013 [latest update: #1919] on 5 January 2014 at 6:11:25.673819 pm'!

!Float methodsFor: 'printing' stamp: 'jmv 1/5/2014 18:05'!
printOn: aStream showingDecimalPlaces: placesDesired
	"This implementation avoids any rounding error caused by rounded or roundTo:
	The approach is simple. Round to appropriate integer, take the digits, and just 
	add the decimal point in the appropriate place."

	| i s |
	self isFinite ifFalse: [ ^self printOn: aStream ].

	placesDesired <= 0 ifTrue: [
		^self rounded printOn: aStream ].

	i _ (self * (10 raisedTo: placesDesired)) rounded.
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

!methodRemoval: Float #printShowingDecimalPlaces:!
Float removeSelector: #printShowingDecimalPlaces:!
