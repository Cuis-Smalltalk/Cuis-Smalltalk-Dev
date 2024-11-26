'From Cuis7.1 [latest update: #6857] on 26 November 2024 at 4:16:59 pm'!

!Character commentStamp: '<historical>' prior: 0!
I represent a character or Unicode Code Point. My instances are the elements of the CharacterSequence hierarchy, including String and UnicodeString.!


!String methodsFor: 'copying' stamp: 'jmv 11/26/2024 16:09:46'!
copyWith: newElement
	newElement codePoint > 255 ifTrue: [
		^self asUnicodeString copyWith: newElement ].
	^super copyWith: newElement! !


!Character methodsFor: 'accessing' stamp: 'jmv 11/26/2024 16:00:43'!
asciiValue
	"Answer the value of the receiver that represents its ASCII encoding.
	This is the same as the Unicode Code Point for the first 128 characters.
	Answer nil if out of range, i.e. we are not part of the ASCII Character set."

	| n |
	n := self codePoint.
	^ n < 128 ifTrue: [ n ].! !

