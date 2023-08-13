'From Cuis 6.0 [latest update: #5980] on 13 August 2023 at 6:46:57 pm'!

!UnicodeString methodsFor: 'accessing' stamp: 'jmv 8/13/2023 18:46:18'!
indexOfAnyOf: aCharacterSet startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start."

	"Fast lane."
	| index |
	self isAscii ifTrue: [
		^ bytes indexOfAnyOf: aCharacterSet startingAt: start ifAbsent: aBlock ].
	aCharacterSet isAscii ifTrue: [ |startBI foundBI|
		startBI := self byteIndexAt: start.
		foundBI := bytes indexOfAnyOf: aCharacterSet startingAt: startBI ifAbsent: aBlock.
		^ self codePointIndexAt: foundBI ].

	index := start.
	self do: [ :codePoint |
		(aCharacterSet includes: codePoint) ifTrue: [ ^index ].
		index := index + 1].

	^aBlock value! !

