'From Cuis 4.2 of 25 July 2013 [latest update: #2571] on 15 November 2015 at 4:26:45.34204 am'!

!SequenceableCollection methodsFor: 'testing' stamp: 'len 11/15/2015 04:14'!
beginsWith: sequence
	"Answer true if the receiver starts with the argument collection."
	
	| sequenceSize |
	self size < (sequenceSize _ sequence size) ifTrue: [ ^false ].
	1 to: sequenceSize do: [ :index |
		(sequence at: index) = (self at: index) ifFalse: [ ^false ] ].
	^true! !

!SequenceableCollection methodsFor: 'testing' stamp: 'len 11/15/2015 04:17'!
endsWith: sequence
	"Answer true if the receiver ends with the argument collection."
	
	| sequenceSize offset |
	sequenceSize _ sequence size.
	(offset _ self size - sequenceSize) < 0 ifTrue: [ ^false ].
	1 to: sequenceSize do: [ :index |
		(sequence at: index) = (self at: index + offset) ifFalse: [ ^false ] ].
	^true! !


!String methodsFor: 'comparing' stamp: 'len 11/15/2015 04:23'!
is: aString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."
	| sequenceSize |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ aString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self basicAt: index) = (aString basicAt: 1) ifFalse: [ ^false ].
	^(self findSubstring: aString in: self startingAt: index matchTable: CaseSensitiveOrder) = index! !

