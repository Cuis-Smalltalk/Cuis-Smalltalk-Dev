"
Warning: TextAlignment and ParagraphStyleReference (What is this referring to?) should always be applied to whole 'paragraphs' in the text. See #isParagraphAttribute

(
(Text string: 'This text has no style set', String crString),
(Text string: 'This is centered', String crString attribute: TextAlignment centered),
(Text string: 'This text has no style set', String crString)
) edit
"
Class {
	#name : 'TextAlignment',
	#superclass : 'TextAttribute',
	#instVars : [
		'alignment'
	],
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'instance creation' }
TextAlignment class >> centered [
	^self new alignment: 2
]

{ #category : 'instance creation' }
TextAlignment class >> justified [
	^self new alignment: 3
]

{ #category : 'instance creation' }
TextAlignment class >> leftFlush [
	^self new alignment: 0
]

{ #category : 'instance creation' }
TextAlignment class >> rightFlush [
	^self new alignment: 1
]

{ #category : 'comparing' }
TextAlignment >> = other [
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other alignment = alignment]
]

{ #category : 'accessing' }
TextAlignment >> alignment [
	^alignment
]

{ #category : 'accessing' }
TextAlignment >> alignment: aNumber [
	alignment := aNumber.
]

{ #category : 'comparing' }
TextAlignment >> dominates: other [
	"There can be only one..."
	^self class == other class
]

{ #category : 'iterating' }
TextAlignment >> forTextAlignmentDo: aBlock [
	aBlock value: alignment
]

{ #category : 'comparing' }
TextAlignment >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^ alignment hash
]

{ #category : 'testing' }
TextAlignment >> isCenteredOrRightFlush [
	^alignment = 2 or: [ alignment = 1 ]
]

{ #category : 'testing' }
TextAlignment >> isParagraphAttribute [
	"Attributes that answer true will always be applied to whole paragraphs, i.e. starting at the position after a newLine (or 1) and ending at a newLine (or text size)
	"
	^true
]

{ #category : 'testing' }
TextAlignment >> isTextAlignment [
	^true
]
