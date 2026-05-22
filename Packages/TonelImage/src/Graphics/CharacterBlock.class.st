"
My instances contain information about displayed characters. They are used to return the results of methods:
	TextComposition characterBlockAtPoint: aPoint and
	TextComposition characterBlockForIndex: stringIndex.
Any recomposition or movement of a TextComposition can make the instance obsolete.
My instances are effectively inmutable, as the only method settings instance variables is private and used just to build new instances.
"
Class {
	#name : 'CharacterBlock',
	#superclass : 'Rectangle',
	#instVars : [
		'stringIndex',
		'text',
		'textLine'
	],
	#category : 'Graphics-Text',
	#package : 'Graphics',
	#tag : 'Text'
}

{ #category : 'instance creation' }
CharacterBlock class >> stringIndex: anInteger text: aText topLeft: topLeft extent: extent textLine: aLine [
	^self new setStringIndex: anInteger text: aText topLeft: topLeft extent: extent textLine: aLine
]

{ #category : 'comparing' }
CharacterBlock >> < aCharacterBlock  [
	"Answer whether the string index of the receiver precedes that of aCharacterBlock."
	^stringIndex < aCharacterBlock stringIndex
]

{ #category : 'comparing' }
CharacterBlock >> <= aCharacterBlock  [
	"Answer whether the string index of the receiver does not come after that of
	aCharacterBlock."
	^(self > aCharacterBlock) not
]

{ #category : 'comparing' }
CharacterBlock >> = aCharacterBlock [

	self == aCharacterBlock ifTrue: [ ^ true ].

	self species == aCharacterBlock species
		ifFalse: [ ^false ].
		
	^ stringIndex = aCharacterBlock stringIndex
]

{ #category : 'comparing' }
CharacterBlock >> > aCharacterBlock  [
	"Answer whether the string index of the receiver comes after that of
	aCharacterBlock."
	^aCharacterBlock < self
]

{ #category : 'comparing' }
CharacterBlock >> >= aCharacterBlock  [
	"Answer whether the string index of the receiver does not precede that of
	aCharacterBlock."
	^(self < aCharacterBlock) not
]

{ #category : 'comparing' }
CharacterBlock >> hash [
	"Hash is reimplemented because = is implemented."

	^stringIndex hash
]

{ #category : 'comparing' }
CharacterBlock >> max: aCharacterBlock [
	aCharacterBlock ifNil:[^self].
	^aCharacterBlock > self
		ifTrue:[ aCharacterBlock]
		ifFalse:[self].
]

{ #category : 'comparing' }
CharacterBlock >> min: aCharacterBlock [
	aCharacterBlock ifNil:[^self].
	^aCharacterBlock < self
		ifTrue:[ aCharacterBlock]
		ifFalse:[self].
]

{ #category : 'printing' }
CharacterBlock >> printOn: aStream [

	aStream nextPutAll: 'a CharacterBlock with index '.
	stringIndex printOn: aStream.
	(text notNil and: [text size> 0 and: [stringIndex between: 1 and: text size]])
		ifTrue: [
			aStream nextPutAll: ' and character '.
			(text at: stringIndex) printOn: aStream].
	aStream nextPutAll: ' and rectangle '.
	super printOn: aStream.
	textLine ifNotNil: [
		aStream newLine; nextPutAll: ' in '.
		textLine printOn: aStream].

]

{ #category : 'private' }
CharacterBlock >> setStringIndex: anInteger text: aText topLeft: topLeft extent: extent textLine: aLine [

	stringIndex := anInteger.
	text := aText.
	textLine := aLine.
	self setOrigin: topLeft corner: topLeft + extent
]

{ #category : 'accessing' }
CharacterBlock >> stringIndex [
	"Answer the position of the receiver in the string it indexes."
	^stringIndex
]

{ #category : 'accessing' }
CharacterBlock >> textLine [
	^ textLine
]
