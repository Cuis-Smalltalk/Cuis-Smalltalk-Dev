"
A TextColor encodes a text color change applicable over a given range of text.
"
Class {
	#name : 'TextColor',
	#superclass : 'TextAttribute',
	#instVars : [
		'color'
	],
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'constants' }
TextColor class >> black [
	^ self new color: `Color black`
]

{ #category : 'constants' }
TextColor class >> blue [
	^ self new color: `Color blue`
]

{ #category : 'instance creation' }
TextColor class >> color: aColor [
	^ self new color: aColor
]

{ #category : 'constants' }
TextColor class >> cyan [
	^ self new color: `Color cyan`
]

{ #category : 'constants' }
TextColor class >> gray [
	^ self new color: `Color gray`
]

{ #category : 'constants' }
TextColor class >> green [
	^ self new color: `Color green`
]

{ #category : 'constants' }
TextColor class >> magenta [
	^ self new color: `Color magenta`
]

{ #category : 'constants' }
TextColor class >> red [
	^ self new color: `Color red`
]

{ #category : 'constants' }
TextColor class >> white  [
	^ self new color: `Color white`
]

{ #category : 'constants' }
TextColor class >> yellow [
	^ self new color: `Color yellow`
]

{ #category : 'comparing' }
TextColor >> = other [
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other color = color]
]

{ #category : 'accessing' }
TextColor >> color [
	^ color
]

{ #category : 'accessing' }
TextColor >> color: aColor [
	color := aColor
]

{ #category : 'comparing' }
TextColor >> dominates: other [
	"Won't affect any other than TextColor"
	other isTextColor ifFalse: [ ^false ].
	"In the TextColor hierarchy, a real TextColor dominates a ShoutTextColor"
	^ other isForShout or: [ self isForShout not ].
]

{ #category : 'iterating' }
TextColor >> forTextColorDo: aBlock [
	aBlock value: color
]

{ #category : 'comparing' }
TextColor >> hash [
	^ color hash
]

{ #category : 'testing' }
TextColor >> isTextColor [
	^true
]

{ #category : 'printing' }
TextColor >> printOn: strm [
	super printOn: strm.
	strm nextPutAll: ' code: '; print: color
]
