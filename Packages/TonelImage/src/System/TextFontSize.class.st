Class {
	#name : 'TextFontSize',
	#superclass : 'TextAttribute',
	#instVars : [
		'pointSize'
	],
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'instance creation' }
TextFontSize class >> pointSize: aNumber [
	"Reference only baseFonts. Any emphasis should be done with TextEmphasis."
	^ self new pointSize: aNumber
]

{ #category : 'comparing' }
TextFontSize >> = other [
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [ other pointSize = pointSize ]
]

{ #category : 'comparing' }
TextFontSize >> dominates: other [
	^ other class == self class
]

{ #category : 'iterating' }
TextFontSize >> forFontSizeDo: aBlock [
	aBlock value: pointSize
]

{ #category : 'comparing' }
TextFontSize >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^pointSize hash
]

{ #category : 'testing' }
TextFontSize >> isFont [
	^true
]

{ #category : 'accessing' }
TextFontSize >> pointSize [
	^pointSize
]

{ #category : 'accessing' }
TextFontSize >> pointSize: aNumber [
	pointSize := aNumber
]
