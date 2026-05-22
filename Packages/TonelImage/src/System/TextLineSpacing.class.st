"
(
(Text string: 'This text has no style set', String newLineString),
(Text string: 'This is has big line spacing', String newLineString attribute: (TextLineSpacing factor: 2.4)),
(Text string: 'This is has big line spacing', String newLineString attribute: (TextLineSpacing factor: 2.4)),
(Text string: 'This text has no style set', String newLineString)
) edit
"
Class {
	#name : 'TextLineSpacing',
	#superclass : 'TextAttribute',
	#instVars : [
		'lineSpacingFactor'
	],
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'instance creation' }
TextLineSpacing class >> factor: aNumber [
	^self new lineSpacingFactor: aNumber
]

{ #category : 'comparing' }
TextLineSpacing >> = other [
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other lineSpacingFactor = lineSpacingFactor]
]

{ #category : 'comparing' }
TextLineSpacing >> dominates: other [
	^ other class == self class
]

{ #category : 'iterating' }
TextLineSpacing >> forLineSpacingDo: aBlock [
	aBlock value: lineSpacingFactor
]

{ #category : 'comparing' }
TextLineSpacing >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^ lineSpacingFactor hash
]

{ #category : 'testing' }
TextLineSpacing >> isParagraphAttribute [
	"Attributes that answer true will always be applied to whole paragraphs, i.e. starting at the position after a newLine (or 1) and ending at a newLine (or text size)
	"
	^true
]

{ #category : 'accessing' }
TextLineSpacing >> lineSpacingFactor [
	^lineSpacingFactor
]

{ #category : 'accessing' }
TextLineSpacing >> lineSpacingFactor: aNumber [
	lineSpacingFactor := aNumber
]
