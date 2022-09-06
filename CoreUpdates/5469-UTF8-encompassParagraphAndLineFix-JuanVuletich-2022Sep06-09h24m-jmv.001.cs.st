'From Cuis 6.0 [latest update: #5467] on 6 September 2022 at 9:24:57 am'!

!ArrayOfCharactersAndCodePoints methodsFor: 'converting' stamp: 'jmv 9/6/2022 09:16:55'!
encompassLine: anInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)
	See also encompassParagraph:"
	| left rightCr rightNull |
	left _ (self lastIndexOf: `Character newLineCharacter` startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	rightCr _ (self indexOf: `Character newLineCharacter` startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	rightNull _ (self indexOf: `Character null` startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	^left to: (rightCr min: rightNull)! !

!ArrayOfCharactersAndCodePoints methodsFor: 'converting' stamp: 'jmv 9/4/2022 00:04:58'!
encompassParagraph: anInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size).
	Look also for null characters. Never include null characters in the answer.
	See also #encompassLine:"
	| left rightCr rightNull |
	left _ (self lastIndexOf: `Character newLineCharacter` startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	rightCr _ (self indexOf: `Character newLineCharacter` startingAt: (anInterval last max: anInterval first) ifAbsent: [self size]).
	rightNull _ (self indexOf: `Character null` startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	^left to: (rightCr min: rightNull)! !


!Utf8String methodsFor: 'paragraph support' stamp: 'jmv 9/6/2022 09:19:53'!
encompassLine: aCodePointInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)
	See also encompassParagraph:"
	| argumentLeftByte argumentRightByte leftByte rightCrByte left nullPosition right |
	argumentLeftByte _ self byteIndexAt: aCodePointInterval first.
	argumentRightByte _ self byteIndexAt: aCodePointInterval last.

	"Find byte position and code point position following last newLine before us."
	leftByte _ (bytes lastIndexOf: `Character newLineCharacter numericValue` startingAt: argumentLeftByte - 1 ifAbsent: [0]) + 1.
	left _ self codePointIndexAt: leftByte.

	"Find byte position and code point position for next newLine. Don't include it."
	right _ count.
	rightCrByte _ bytes indexOf: `Character newLineCharacter numericValue` startingAt: (argumentRightByte max: argumentLeftByte).
	rightCrByte = 0 ifFalse: [
		right _ (self codePointIndexAt: rightCrByte)-1].

	"But stop before if any nulls. Don't include them!!"
	nullPosition _ bytes indexOf: 0 startingAt: (argumentRightByte max: argumentLeftByte).
	nullPosition = 0 ifFalse: [
		right _ right min: ((self codePointIndexAt: nullPosition)-1) ].

	^left to: right! !

!Utf8String methodsFor: 'paragraph support' stamp: 'jmv 9/6/2022 09:10:44'!
encompassParagraph: aCodePointInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size).
	Look also for null characters. Never include null characters in the answer.
	See also #encompassLine:"
	| argumentLeftByte argumentRightByte leftByte rightCrByte left nullPosition right |
	argumentLeftByte _ self byteIndexAt: aCodePointInterval first.
	argumentRightByte _ self byteIndexAt: aCodePointInterval last.

	"Find byte position and code point position following last newLine before us."
	leftByte _ (bytes lastIndexOf: `Character newLineCharacter numericValue` startingAt: argumentLeftByte - 1 ifAbsent: [0]) + 1.
	left _ self codePointIndexAt: leftByte.

	"Find byte position and code point position for next newLine. Include it."
	right _ count.
	rightCrByte _ bytes indexOf: `Character newLineCharacter numericValue` startingAt: (argumentRightByte max: argumentLeftByte).
	rightCrByte = 0 ifFalse: [
		right _ self codePointIndexAt: rightCrByte].

	"But stop before if any nulls. Don't include them!!"
	nullPosition _ bytes indexOf: 0 startingAt: (argumentRightByte max: argumentLeftByte).
	nullPosition = 0 ifFalse: [
		right _ right min: ((self codePointIndexAt: nullPosition)-1) ].

	^left to: right! !

