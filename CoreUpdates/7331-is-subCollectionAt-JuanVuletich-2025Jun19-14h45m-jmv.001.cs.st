'From Cuis7.5 [latest update: #7279] on 19 June 2025 at 2:46:56 pm'!

!SequenceableCollection methodsFor: '[mk] comparing' stamp: 'jmv 6/19/2025 10:48:21'!
is: otherCollection subcollectionAt: index
	"Answer whether the receiver includes otherCollection as a subcollection at position index."

	| sequenceSize |
	index < 1 ifTrue: [ ^false ].
	sequenceSize := otherCollection size.
	sequenceSize = 0 ifTrue: [ ^true ].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	1 to: sequenceSize do: [ :i |
		(self at: index+i-1) = (otherCollection at: i) ifFalse: [ ^false ]].
	^true! !


!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 6/19/2025 10:48:50'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	^self is: subString subcollectionAt: index! !

