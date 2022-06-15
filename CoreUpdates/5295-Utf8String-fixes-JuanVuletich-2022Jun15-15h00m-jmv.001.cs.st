'From Cuis 6.0 [latest update: #5293] on 15 June 2022 at 3:01:29 pm'!

!Utf8String methodsFor: 'paragraph support' stamp: 'jmv 6/15/2022 15:00:23'!
encompassLine: aCodePointInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)
	See also encompassParagraph:"
	| argumentLeftByte argumentRightByte leftByte rightCrByte rightNullByte |
	argumentLeftByte _ self byteIndexAt: aCodePointInterval first.
	argumentRightByte _ self byteIndexAt: aCodePointInterval last.
	leftByte _ (bytes lastIndexOf: `Character newLineCharacter numericValue` startingAt: argumentLeftByte - 1 ifAbsent:[0]) + 1.
	rightCrByte _ (bytes indexOf: `Character newLineCharacter numericValue` startingAt: (argumentRightByte max: argumentLeftByte) ifAbsent: [bytes size+1])-1.
	rightNullByte _ (bytes indexOf: 0 startingAt: (argumentRightByte max: argumentLeftByte) ifAbsent: [bytes size+1])-1.
	^(self codePointIndexAt: leftByte) to: (self codePointIndexAt: (rightCrByte min: rightNullByte))! !

!Utf8String methodsFor: 'paragraph support' stamp: 'jmv 6/15/2022 15:00:32'!
encompassParagraph: aCodePointInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size).
	Look also for null characters. Never include null characters in the answer.
	See also #encompassLine:"
	| argumentLeftByte argumentRightByte leftByte rightCrByte rightNullByte |
	argumentLeftByte _ self byteIndexAt: aCodePointInterval first.
	argumentRightByte _ self byteIndexAt: aCodePointInterval last.
	leftByte _ (bytes lastIndexOf: `Character newLineCharacter numericValue` startingAt: argumentLeftByte - 1 ifAbsent:[0]) + 1.
	rightCrByte _ (bytes indexOf: `Character newLineCharacter numericValue` startingAt: (argumentRightByte max: argumentLeftByte) ifAbsent: [bytes size]).
	rightNullByte _ (bytes indexOf: 0 startingAt: (argumentRightByte max: argumentLeftByte) ifAbsent: [bytes size+1])-1.
	^(self codePointIndexAt: leftByte) to: (self codePointIndexAt: (rightCrByte min: rightNullByte))! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/15/2022 14:55:53'!
byteIndexAt: codePointIndex
	| byteIndex |
	codePointIndex < 2 ifTrue: [ ^codePointIndex ]. "For 0 and 1"
	self isAscii ifTrue: [ ^ codePointIndex ].
	codePointIndex = (self size+1) ifTrue: [
		"The position a hypotetical extra code point would start at.
		Useful to find the last byte position for some codePointIndex. See senders."
		^bytes size+1 ].
	byteIndex _ codePointIndex > self stride
		ifTrue: [
			strideIndex ifNil: [ self computeCountAndIndex ].
			strideIndex at: codePointIndex - 1 // self stride ]
		ifFalse: [ 1 ].
	codePointIndex - 1 \\ self stride timesRepeat: [ 
		byteIndex _ byteIndex + (self encodedByteCountAt: byteIndex) ].
	^ byteIndex! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/15/2022 14:56:02'!
codePointIndexAt: byteIndex
	| codePointIndex byteCursor |
	byteIndex < 2 ifTrue: [ ^byteIndex ]. "For 0 and 1"
	self isAscii ifTrue: [ ^ byteIndex ].
	codePointIndex := byteCursor := 1.
	strideIndex ifNil: [ self computeCountAndIndex ].
	strideIndex ifNotEmpty: [  
		strideIndex 
			findBinaryIndex: [ :x | byteIndex - x ] 
			do: [ :found | ^ (found * self stride) + 1 ] 
			ifNone: [ :lower :upper |
				lower = 0 
			 		ifFalse: [ 
						byteCursor := strideIndex at: lower. 
						codePointIndex := (lower * self stride) + 1 ] ] ].
	[ byteCursor <= bytes size ] whileTrue: [
		byteCursor = byteIndex 
			ifTrue: [ ^ codePointIndex ].
		byteCursor := byteCursor + (self encodedByteCountAt: byteCursor).
		codePointIndex := codePointIndex + 1 ].
	^ 0! !

