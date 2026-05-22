Class {
	#name : 'TextDoIt',
	#superclass : 'TextAction',
	#instVars : [
		'evalString'
	],
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'as yet unclassified' }
TextDoIt class >> evalString: str [
	"
	('Some text. ',
	(Text string: '<click here>' attribute: (TextDoIt evalString: '123456 print')), 
	' more regular text') edit
	"
	^ self new evalString: str
]

{ #category : 'comparing' }
TextDoIt >> = other [
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other evalString = evalString]
]

{ #category : 'text action' }
TextDoIt >> actOnClickFor: anObject [
	"Note: evalString gets evaluated IN THE CONTEXT OF anObject
	 -- meaning that self and all instVars are accessible"
	Compiler evaluate: evalString for: anObject logged: false.
	^ true 
]

{ #category : 'as yet unclassified' }
TextDoIt >> analyze: aString [

	| list |
	list := super analyze: aString.
	evalString := list at: 1.
	^ list at: 2
]

{ #category : 'accessing' }
TextDoIt >> evalString [
	^evalString
]

{ #category : 'as yet unclassified' }
TextDoIt >> evalString: str [
	evalString := str 
]

{ #category : 'comparing' }
TextDoIt >> hash [
	"Hash is reimplemented because = is implemented."

	^evalString hash
]

{ #category : 'as yet unclassified' }
TextDoIt >> info [
	^ evalString
]
