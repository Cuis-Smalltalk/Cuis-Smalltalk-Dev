"
('Some text ', (Text string: 'theLink' attributes: {TextURL new url: 'www.koko.kom'}), ' blah, blah.') edit
"
Class {
	#name : 'TextURL',
	#superclass : 'TextAction',
	#instVars : [
		'url'
	],
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'comparing' }
TextURL >> = other [
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other url = url]
]

{ #category : 'text action' }
TextURL >> actOnClickFor: anObject  [
	"Do what you can with this URL.  Later a web browser."
	^true
]

{ #category : 'as yet unclassified' }
TextURL >> analyze: aString [

	| list |
	list := super analyze: aString.
	url := list at: 1.
	^ list at: 2
]

{ #category : 'comparing' }
TextURL >> hash [
	"Hash is reimplemented because = is implemented."

	^url hash
]

{ #category : 'as yet unclassified' }
TextURL >> info [
	^ url
]

{ #category : 'accessing' }
TextURL >> url [
	^url
]

{ #category : 'as yet unclassified' }
TextURL >> url: aString [
	url := aString
]
