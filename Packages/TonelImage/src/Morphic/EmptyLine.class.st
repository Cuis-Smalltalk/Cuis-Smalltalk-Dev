"
An EmptyLine is a special line that has actually no text. It is used to place the cursor below the last line if it ends with cr. It is also used in case of empty text.
"
Class {
	#name : 'EmptyLine',
	#superclass : 'TextLine',
	#instVars : [
		'paragraphStyle'
	],
	#category : 'Morphic-Text Support',
	#package : 'Morphic',
	#tag : 'Text Support'
}

{ #category : 'initialization' }
EmptyLine >> initialize [
	super initialize.
	internalSpaces := 0.
	paddingWidth := 0.
	isFirstLine := true
]

{ #category : 'testing' }
EmptyLine >> isEmptyLine [
	^true
]

{ #category : 'accessing' }
EmptyLine >> paragraphStyle [

	^paragraphStyle
]

{ #category : 'accessing' }
EmptyLine >> paragraphStyle: aParagraphtStyle [

	paragraphStyle := aParagraphtStyle
]
