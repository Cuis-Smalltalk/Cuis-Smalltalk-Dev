Class {
	#name : 'ArrayOfCharactersAndCodePoints',
	#superclass : 'Array',
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'converting' }
ArrayOfCharactersAndCodePoints >> asStreamResult [
	"See senders.
	Also see #thatCanHoldCodePoints"
	^UnicodeString newFrom: self
]

{ #category : 'accessing' }
ArrayOfCharactersAndCodePoints >> at: index [
	"Full compatibility with String"
	^(super at: index) ifNil: [ `Character null` ].
]

{ #category : 'converting' }
ArrayOfCharactersAndCodePoints >> encompassLine: anInterval [
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)
	See also encompassParagraph:"
	| left rightCr rightNull |
	left := (self lastIndexOf: `Character newLineCharacter` startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	rightCr := (self indexOf: `Character newLineCharacter` startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	rightNull := (self indexOf: `Character null` startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	^left to: (rightCr min: rightNull)
]

{ #category : 'converting' }
ArrayOfCharactersAndCodePoints >> encompassParagraph: anInterval [
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size).
	Look also for null characters. Never include null characters in the answer.
	See also #encompassLine:"
	| left rightCr rightNull |
	left := (self lastIndexOf: `Character newLineCharacter` startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	rightCr := (self indexOf: `Character newLineCharacter` startingAt: (anInterval last max: anInterval first) ifAbsent: [self size]).
	rightNull := (self indexOf: `Character null` startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	^left to: (rightCr min: rightNull)
]

{ #category : 'testing' }
ArrayOfCharactersAndCodePoints >> is: aSymbol [
	^ aSymbol == #ArrayOfCharactersAndCodePoints or: [ super is: aSymbol ]
]
