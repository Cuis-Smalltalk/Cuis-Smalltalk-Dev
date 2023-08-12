'From Cuis 6.0 [latest update: #5979] on 12 August 2023 at 4:58:40 pm'!

!UnicodeString methodsFor: 'accessing' stamp: 'jmv 8/12/2023 16:54:23'!
indexOfAnyOf: aCharacterSet startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start."

	"Fast lane."
	| index |
	self isAscii ifTrue: [
		^ bytes indexOfAnyOf: aCharacterSet startingAt: start ifAbsent: aBlock ].
	aCharacterSet isAscii ifTrue: [
		^ self codePointIndexAt: (bytes indexOfAnyOf: aCharacterSet startingAt: start ifAbsent: aBlock) ].

	index := start.
	self do: [ :codePoint |
		(aCharacterSet includes: codePoint) ifTrue: [ ^index ].
		index := index + 1].

	^aBlock value! !

