'From Cuis 6.0 [latest update: #5620] on 4 January 2023 at 11:03:18 am'!

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 1/4/2023 10:58:49'!
indexOfAnyOf: aCharacterSet
	"returns the index of the first character in the given set, starting from 1."

	^ self indexOfAnyOf: aCharacterSet startingAt: 1 ifAbsent: 0! !


!UnicodeString methodsFor: 'accessing' stamp: 'jmv 1/4/2023 10:57:26'!
indexOfAnyOf: aCharacterSet startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start."

	"Fast lane."
	| index |
	(self isAscii or: [aCharacterSet isAscii]) ifTrue: [
		^ bytes indexOfAnyOf: aCharacterSet startingAt: start ifAbsent: aBlock ].

	index _ start.
	self do: [ :codePoint |
		(aCharacterSet includes: codePoint) ifTrue: [ ^index ].
		index _ index + 1].

	^aBlock value! !

!methodRemoval: UnicodeString #indexOfAnyOf: stamp: 'jmv 1/4/2023 10:58:41'!
UnicodeString removeSelector: #indexOfAnyOf:!
!methodRemoval: String #indexOfAnyOf: stamp: 'jmv 1/4/2023 10:58:44'!
String removeSelector: #indexOfAnyOf:!
