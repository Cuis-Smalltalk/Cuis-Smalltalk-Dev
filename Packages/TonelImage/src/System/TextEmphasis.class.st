"
A TextEmphasis, encodes a characteristic applicable to all fonts.  The encoding is as follows:
	1	bold
	2	italic
	4	underlined		
	8	struck out
	16 	Superscript
	32 	Subscript
"
Class {
	#name : 'TextEmphasis',
	#superclass : 'TextAttribute',
	#instVars : [
		'emphasisCode'
	],
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'instance creation' }
TextEmphasis class >> bold [
	^ self new emphasisCode: AbstractFont boldCode
]

{ #category : 'instance creation' }
TextEmphasis class >> italic [
	^ self new emphasisCode: AbstractFont italicCode
]

{ #category : 'instance creation' }
TextEmphasis class >> normal [
	^ self new emphasisCode: AbstractFont normalCode
]

{ #category : 'instance creation' }
TextEmphasis class >> struckThrough [
	^ self new emphasisCode: AbstractFont struckThroughCode
]

{ #category : 'instance creation' }
TextEmphasis class >> subscript [
	^ self new emphasisCode: AbstractFont subscriptCode
]

{ #category : 'instance creation' }
TextEmphasis class >> superscript [
	^ self new emphasisCode: AbstractFont superscriptCode
]

{ #category : 'instance creation' }
TextEmphasis class >> underlined [
	^ self new emphasisCode: AbstractFont underlinedCode
]

{ #category : 'comparing' }
TextEmphasis >> = other [
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other emphasisCode = emphasisCode]
]

{ #category : 'testing' }
TextEmphasis >> dominatedByNormalEmphasis [
	"TextEmphasis normal (i.e. cmd-0) should turn off emphasis"
	^ true
]

{ #category : 'comparing' }
TextEmphasis >> dominates: other [
	(emphasisCode = 0 and: [ other dominatedByNormalEmphasis ]) ifTrue: [^ true].
	^ (other class == self class)
		and: [ emphasisCode = other emphasisCode ]
]

{ #category : 'accessing' }
TextEmphasis >> emphasisCode [
	^ emphasisCode
]

{ #category : 'accessing' }
TextEmphasis >> emphasisCode: int [
	emphasisCode := int
]

{ #category : 'iterating' }
TextEmphasis >> forTextEmphasisDo: aBlock [
	aBlock value: emphasisCode
]

{ #category : 'comparing' }
TextEmphasis >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^emphasisCode hash

]

{ #category : 'printing' }
TextEmphasis >> printOn: strm [
	super printOn: strm.
	strm nextPutAll: ' code: '; print: emphasisCode
]
