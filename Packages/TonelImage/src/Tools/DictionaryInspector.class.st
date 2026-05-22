Class {
	#name : 'DictionaryInspector',
	#superclass : 'Inspector',
	#instVars : [
		'keyArray'
	],
	#category : 'Tools-Inspector',
	#package : 'Tools',
	#tag : 'Inspector'
}

{ #category : 'selecting' }
DictionaryInspector >> addEntry: aKey [
	object at: aKey put: nil.
	self calculateKeyArray.
	selectionIndex := self numberOfFixedFields + (keyArray indexOf: aKey).
	self changed: #inspectObject.
	self changed: #selectionIndex.
	self changed: #fieldList.
	self update
]

{ #category : 'selecting' }
DictionaryInspector >> calculateKeyArray [
	"Recalculate the KeyArray from the object being inspected"

	keyArray := object keysSortedSafely asArray.
	selectionIndex := 0.

]

{ #category : 'accessing' }
DictionaryInspector >> fieldList [
	^ self baseFieldList
		, (keyArray collect: [:key | key printString])
]

{ #category : 'initialization' }
DictionaryInspector >> initialize [

	super initialize.
	self calculateKeyArray
]

{ #category : 'selecting' }
DictionaryInspector >> nonFixedSelectionIndex [
	^self selectionIndex - self numberOfFixedFields
]

{ #category : 'private' }
DictionaryInspector >> numberOfFixedFields [
	^self lastMetaField + self objectClassInstSize
]

{ #category : 'user interface support' }
DictionaryInspector >> objectSpecificMenuSpecFrom: aWindow [

	^aWindow menuSpecForDictionary
]

{ #category : 'selecting' }
DictionaryInspector >> refreshView [
	| i |
	i := selectionIndex.
	self calculateKeyArray.
	selectionIndex := i.
	self changed: #fieldList.
	self acceptedContentsChanged.
]

{ #category : 'menu' }
DictionaryInspector >> removeSelection [

	self selectionIndexFixed ifTrue: [^ self changed: #flash].
	object removeKey: (keyArray at: self nonFixedSelectionIndex).
	selectionIndex := 0.
	acceptedContentsCache := ''.
	self calculateKeyArray.
	self changed: #inspectObject.
	self changed: #selectionIndex.
	self changed: #fieldList
]

{ #category : 'menu' }
DictionaryInspector >> renameEntryTo: newKey [

	| oldKey value |
	self selectionIndexFixed ifTrue: [^ self changed: #flash].
	oldKey := keyArray at: self nonFixedSelectionIndex.
	value := object at: oldKey.
	object removeKey: oldKey.
	object at: newKey put: value.
	self calculateKeyArray.
	selectionIndex := self numberOfFixedFields + (keyArray indexOf: newKey).
	self changed: #selectionIndex.
	self changed: #inspectObject.
	self changed: #fieldList.
	self update
]

{ #category : 'selecting' }
DictionaryInspector >> replaceSelectionValue: anObject  [
	self selectionIndexFixed ifTrue: [^ super replaceSelectionValue: anObject].
	^ object
		at: (keyArray at: self nonFixedSelectionIndex)
		put: anObject
]

{ #category : 'selecting' }
DictionaryInspector >> selectedKey [
	"Create a browser on all senders of the selected key"
	| i |
	i := self nonFixedSelectionIndex.
	i > 0 ifFalse: [ ^ nil ].
	^keyArray at: i
]

{ #category : 'selecting' }
DictionaryInspector >> selection [

	self selectionIndexFixed ifTrue: [^ super selection].
	^ object at: (keyArray at: self nonFixedSelectionIndex) ifAbsent: nil
]

{ #category : 'selecting' }
DictionaryInspector >> selectionIndexFixed [
	^self selectionIndex <= self numberOfFixedFields
]
