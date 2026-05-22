Class {
	#name : 'TextFontFamily',
	#superclass : 'TextAttribute',
	#instVars : [
		'familyName'
	],
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'instance creation' }
TextFontFamily class >> familyName: aString [
	"Reference only baseFonts. Any emphasis should be done with TextEmphasis."
	^ self new familyName: aString
]

{ #category : 'instance creation' }
TextFontFamily class >> pointSize: aNumber [
	"Reference only default family baseFont. Any emphasis should be done with TextEmphasis.
	Store only pointSize"
	^ self new familyName: nil pointSize: aNumber
]

{ #category : 'comparing' }
TextFontFamily >> = other [
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [ other familyName = familyName ]
]

{ #category : 'comparing' }
TextFontFamily >> dominates: other [
	^ other class == self class
]

{ #category : 'accessing' }
TextFontFamily >> familyName [
	^familyName
]

{ #category : 'accessing' }
TextFontFamily >> familyName: aStringOrNil [
	familyName := aStringOrNil.
]

{ #category : 'iterating' }
TextFontFamily >> forFontFamilyDo: aBlock [
	aBlock value: familyName
]

{ #category : 'comparing' }
TextFontFamily >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^familyName hash
]

{ #category : 'testing' }
TextFontFamily >> isFont [
	^true
]
