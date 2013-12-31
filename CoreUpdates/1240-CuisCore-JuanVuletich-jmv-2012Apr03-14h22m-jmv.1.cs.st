'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 3 April 2012 at 2:26:39 pm'!

!String methodsFor: 'comparing' stamp: 'jmv 4/3/2012 12:22'!
      endsWith: suffix
	"Answer whether the tail end of the receiver is the same as suffix.
	The comparison is case-sensitive."

	^self is: suffix substingAt: self size - suffix size + 1
"
  'Elvis' endsWith: 'vis'
"! !

!String methodsFor: 'comparing' stamp: 'jmv 4/3/2012 14:25'!
            is: aString substingAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."
	| sequenceSize |
	sequenceSize _ aString size.
	sequenceSize = 0 ifTrue: [ ^false ].
	index < 1 ifTrue: [ ^false ].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self basicAt: index) = (aString basicAt: 1) ifFalse: [ ^false ].
	^(self findSubstring: aString in: self startingAt: index matchTable: CaseSensitiveOrder) = index! !

