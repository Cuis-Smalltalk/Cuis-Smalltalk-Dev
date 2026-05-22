"
TextAnchors support anchoring of images in text.  A TextAnchor exists as an attribute of text emphasis, and it gets control like a FontReference, through the emphasizeScanner: message.  Depending on whether its anchoredMorph is a Morph or a Form, it repositions the morph, or displays the form respectively.  The coordination between composition, display and selection can best be understood by browsing the various implementations of placeEmbeddedObject:.

In the morphic world, simply embed any morph in text.  In the old world, you can create an image reference using code such as the following.

"A Form"
('Hello', (Text withForm: (EllipseMorph new imageForm: 2)), 'world') edit
('Hello', (Text withForm: (EllipseMorph new imageForm: 32)), 'world') edit

"A Morph"
((Text withAll: 'foo') , (Text string: '*' asString attribute: (TextAnchor new anchoredFormOrMorph: EllipseMorph new)) , (Text withAll: 'bar')) edit

In this case you select a piece of the screen, and it gets anchored to a one-character text in the editor's past buffer.  If you then paste into some other text, you will see the image as an embedded image.
"
Class {
	#name : 'TextAnchor',
	#superclass : 'TextAttribute',
	#instVars : [
		'anchoredFormOrMorph'
	],
	#category : 'Morphic-Text Support',
	#package : 'Morphic',
	#tag : 'Text Support'
}

{ #category : 'comparing' }
TextAnchor >> = other [
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other anchoredFormOrMorph = anchoredFormOrMorph]
]

{ #category : 'accessing' }
TextAnchor >> anchoredFormOrMorph [
	^anchoredFormOrMorph
]

{ #category : 'accessing' }
TextAnchor >> anchoredFormOrMorph: aFormOrMorph [
	anchoredFormOrMorph := aFormOrMorph
]

{ #category : 'testing' }
TextAnchor >> canBeJoinedWith: aTextAttribute [
	"Never join in RunArray. Not even when self == aTextAttribute!"
	^false
]

{ #category : 'comparing' }
TextAnchor >> hash [
	"Hash is reimplemented because = is implemented."

	^anchoredFormOrMorph hash
]

{ #category : 'testing' }
TextAnchor >> isForFormatting [
	"Answer false for those attributes that are not for text formatting.
	This is, those that are not considered in #withFormattingAttributeValues:do:"
	^false
]

{ #category : 'testing' }
TextAnchor >> mayBeExtended [
	"A textAnchor is designed to modify only a single character, and therefore must not be extended by the ParagraphEditor's emphasisHere facility"
	^ false
]

{ #category : 'copying' }
TextAnchor >> postCopy [

	anchoredFormOrMorph := anchoredFormOrMorph copy
]
