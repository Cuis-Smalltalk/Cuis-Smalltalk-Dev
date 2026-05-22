"
Part of an IniFile.
"
Class {
	#name : 'IniFileSection',
	#superclass : 'Object',
	#instVars : [
		'name',
		'entries'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'instance creation' }
IniFileSection class >> new [
	^ self shouldNotImplement
]

{ #category : 'instance creation' }
IniFileSection class >> newGlobal [
	^ self basicNew initializeGlobal
]

{ #category : 'instance creation' }
IniFileSection class >> newNamed: aString [
	^ self basicNew initializeNamed: aString
]

{ #category : 'accessing' }
IniFileSection >> at: entryName [
	"Answer the value, or nil if it hasn't been set."

	^entries at: entryName ifAbsent: nil.

]

{ #category : 'accessing' }
IniFileSection >> at: entryName orDefault: aBlock  [
	"If entry is missing, answer the result of evaluating aBlock,
	but don't add the section or entry."

	^entries at: entryName ifAbsent: aBlock.

]

{ #category : 'accessing' }
IniFileSection >> at: entryName orPutDefault: aBlock  [
	"If entry is missing, answer the result of evaluating aBlock,
	AND add it as the value of the entry."

	^entries at: entryName ifAbsentPut: aBlock.

]

{ #category : 'accessing' }
IniFileSection >> at: entryName put: aValue [
	"Just store the value."

	entries at: entryName put: aValue.
	^aValue.
]

{ #category : 'accessing' }
IniFileSection >> entries [
	^entries
]

{ #category : 'enumerating' }
IniFileSection >> entriesDo: aBlock [
	entries keysAndValuesDo: [:key :value | aBlock value: key value: value]
]

{ #category : 'initialization' }
IniFileSection >> initializeGlobal [
	"Initialize a Global section. There should be, at most, one of these in an IniFile."

	name := nil.
	entries := OrderedDictionary new.
]

{ #category : 'initialization' }
IniFileSection >> initializeNamed: aString [
	"Initialize a Global section. There should be, at most, one of these in an IniFile."

	name := aString.
	entries := OrderedDictionary new.
]

{ #category : 'accessing' }
IniFileSection >> size [
	^entries size
]

{ #category : 'writing' }
IniFileSection >> writeOn: aStream [
	name ifNotNil: [ 
		aStream
			nextPut: $[; nextPutAll: name; nextPut: $]; newLine ].
	entries associationsDo: [ :assoc |
		aStream nextPutAll: assoc key; nextPut: $=; nextPutAll: (assoc value ifNil: ['']); newLine ].

]
