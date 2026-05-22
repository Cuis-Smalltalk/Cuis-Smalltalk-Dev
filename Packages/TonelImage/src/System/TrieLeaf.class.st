"
My instances hold the key/value pairs for Tries.
"
Class {
	#name : 'TrieLeaf',
	#superclass : 'Object',
	#instVars : [
		'keys',
		'values'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'accessing' }
TrieLeaf >> at: aString ifAbsent: aBlock characterIndex: i [
	"Unused argument i is just for polymorphism with TrieNode"
	| keyIndex |
	keys isArray ifTrue: [
		keyIndex := keys indexOf: aString.
		^ keyIndex = 0
			ifTrue: [ aBlock value ]
			ifFalse: [ values at: keyIndex ]].
	keys = aString ifTrue: [ ^ values ].
	^ aBlock value
]

{ #category : 'accessing' }
TrieLeaf >> at: aString put: aValue characterIndex: i [
	"Unused argument i is just for polymorphism with TrieNode"
	| keyIndex |
	keys ifNil: [
		keys := aString.
		values := aValue.
		^aValue ].
	keys isArray ifFalse: [
		keys = aString
			ifFalse: [
				keys := {keys. aString }.
				values := {values. aValue}]
			ifTrue: [ values := aValue ].
		^ aValue ].
	keyIndex := keys indexOf: aString.
	keyIndex = 0
		ifTrue: [
			keys := keys copyWith: aString.
			values := values copyWith: aValue ]
		ifFalse: [ values at: keyIndex put: aValue ].
	^ aValue
]

{ #category : 'testing' }
TrieLeaf >> isEmpty [
	^keys isNil
]

{ #category : 'enumerating' }
TrieLeaf >> keysAndValuesDo: twoArgBlock [

	keys isArray
		ifTrue: [ keys with: values do: twoArgBlock ]
		ifFalse: [ twoArgBlock value: keys value: values ]
]

{ #category : 'removing' }
TrieLeaf >> removeKey: aString ifAbsent: aBlock characterIndex: i [
	"Unused argument i is just for polymorphism with TrieNode"
	| keyIndex answer |
	keys isArray ifTrue: [
		keyIndex := keys indexOf: aString.
		^ keyIndex = 0
			ifTrue: [ aBlock value ]
			ifFalse: [
				answer := values at: keyIndex.
				keys size = 2
					ifTrue: [
						keys := keys at: 3-keyIndex.
						values := values at: 3-keyIndex ]
					ifFalse: [
						keys := (keys copyFrom: 1 to: keyIndex-1), (keys copyFrom: keyIndex+1 to: keys size).
						values := (values copyFrom: 1 to: keyIndex-1), (values copyFrom: keyIndex+1 to: values size) ].
				answer]].
	keys = aString ifTrue: [
		answer := values.
		keys := nil.
		values := nil.
		^ answer ].
	^ aBlock value
]
