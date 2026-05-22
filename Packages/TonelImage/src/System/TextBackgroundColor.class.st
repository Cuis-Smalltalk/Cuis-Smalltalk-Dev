"
A TextBackgroundColor encodes a highlight (background) color change applicable over a given range of text.
"
Class {
	#name : 'TextBackgroundColor',
	#superclass : 'TextAttribute',
	#instVars : [
		'color'
	],
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'constants' }
TextBackgroundColor class >> black [
	^ self new color: `Color black`
]

{ #category : 'constants' }
TextBackgroundColor class >> blue [
	^ self new color: `Color blue`
]

{ #category : 'instance creation' }
TextBackgroundColor class >> color: aColor [
	^ self new color: aColor
]

{ #category : 'constants' }
TextBackgroundColor class >> cyan [
	^ self new color: `Color cyan`
]

{ #category : 'constants' }
TextBackgroundColor class >> gray [
	^ self new color: `Color gray`
]

{ #category : 'constants' }
TextBackgroundColor class >> green [
	^ self new color: `Color green`
]

{ #category : 'constants' }
TextBackgroundColor class >> magenta [
	^ self new color: `Color magenta`
]

{ #category : 'constants' }
TextBackgroundColor class >> red [
	^ self new color: `Color red`
]

{ #category : 'constants' }
TextBackgroundColor class >> white  [
	^ self new color: `Color white`
]

{ #category : 'constants' }
TextBackgroundColor class >> yellow [
	^ self new color: `Color yellow`
]

{ #category : 'comparing' }
TextBackgroundColor >> = other [
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other color = color]
]

{ #category : 'accessing' }
TextBackgroundColor >> color [
	^ color
]

{ #category : 'accessing' }
TextBackgroundColor >> color: aColor [
	color := aColor
]

{ #category : 'comparing' }
TextBackgroundColor >> dominates: other [
	"Won't affect any other than TextBackgroundColor"
	other isBackgroundColor ifFalse: [ ^false ].
	"In the TextColor hierarchy, a real TextBackgroundColor dominates a ShoutTextBackgroundColor"
	^ other isForShout or: [ self isForShout not ].
]

{ #category : 'iterating' }
TextBackgroundColor >> forTextBackgroundColorDo: aBlock [
	aBlock value: color
]

{ #category : 'comparing' }
TextBackgroundColor >> hash [
	^ color hash
]

{ #category : 'testing' }
TextBackgroundColor >> isBackgroundColor [
	^true
]

{ #category : 'printing' }
TextBackgroundColor >> printOn: strm [
	super printOn: strm.
	strm nextPutAll: ' code: '; print: color
]
