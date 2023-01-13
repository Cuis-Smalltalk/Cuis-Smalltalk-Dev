'From Cuis 6.0 [latest update: #5631] on 13 January 2023 at 3:37:40 pm'!

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 1/13/2023 14:27:48'!
findFirst: aBlock startingAt: firstIndex
	"Return the index of my first element for which aBlock evaluates as true.
	Start at firstIndex"

	| s index |
	s _ self size.
	index _ firstIndex.
	[
		index <= s ] whileTrue: [
			(aBlock value: (self at: index)) ifTrue: [^index ].
			index _ index + 1.
			].
	^ 0! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 1/13/2023 14:27:19'!
findFirst: aBlock
	"Return the index of my first element for which aBlock evaluates as true."

	^self findFirst: aBlock startingAt: 1! !

