"
I am a TextModelMorph that knows enough to make myself invisible when necessary.
"
Class {
	#name : 'BrowserCommentTextMorph',
	#superclass : 'TextModelMorph',
	#instVars : [
		'separator',
		'code',
		'proportionalHeight'
	],
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'displaying' }
BrowserCommentTextMorph >> hidePane [

	self hide.
	separator visible ifTrue: [
		proportionalHeight := self layoutSpec proportionalLayoutHeight ].
	separator layoutSpec fixedHeight: 0.
	self layoutSpec proportionalHeight: 0.
	code layoutSpec proportionalHeight: 1.0.
	separator hide.
]

{ #category : 'accessing' }
BrowserCommentTextMorph >> separator: adjusterMorph code: codeMorph [
	separator := adjusterMorph.
	code := codeMorph.
]

{ #category : 'displaying' }
BrowserCommentTextMorph >> showPane [

	self show.
	proportionalHeight ifNotNil: [
		self layoutSpec proportionalHeight: proportionalHeight.
		code layoutSpec proportionalHeight: 1.0 - proportionalHeight ].
	separator layoutSpec fixedHeight: Theme current layoutAdjusterThickness.
	separator show
]

{ #category : 'updating' }
BrowserCommentTextMorph >> update: anAspect [
	super update: anAspect.
	anAspect == #editSelection ifFalse: [ ^self ].
	model textProvider isEditingExistingClass
		ifTrue: [ self showPane ]
		ifFalse: [ self hidePane ]
]
