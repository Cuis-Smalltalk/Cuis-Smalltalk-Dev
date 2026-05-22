Class {
	#name : 'SequenceableCollectionInspector',
	#superclass : 'Inspector',
	#category : 'Tools-Inspector',
	#package : 'Tools',
	#tag : 'Inspector'
}

{ #category : 'accessing' }
SequenceableCollectionInspector >> fieldList [
	"Must tolerate malformed objects"

	| fieldsHere |
	object isNil ifTrue: [^#()].
	fieldsHere := [self indexLabelsForSize: self objectSize]
		on: UnhandledError
		do: [:ex | ex return: #()].
	^self baseFieldList , fieldsHere
"
OrderedCollection new inspect
(OrderedCollection newFrom: #(3 5 7 123)) inspect
(OrderedCollection newFrom: (1 to: 1000)) inspect
"
]

{ #category : 'accessing' }
SequenceableCollectionInspector >> objectSize [
	"Must tolerate malformed objects"

	^[object size]
		on: UnhandledError
		do: [:ex | ex return: 0]
]

{ #category : 'selecting' }
SequenceableCollectionInspector >> replaceSelectionValue: anObject  [
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. The value of the selected variable is set to the value, anObject."

	self selectionIndexNonMeta <= self objectClassInstSize
		ifTrue: [^ super replaceSelectionValue: anObject].
	object at: self selectedObjectIndex put: anObject
]

{ #category : 'selecting' }
SequenceableCollectionInspector >> selectedSlotName [

	self selectionIsMeta ifTrue: [^super selectedSlotName].
	self selectionIndexNonMeta <= self objectClassInstSize ifTrue: [^super selectedSlotName].
	^'(self at: ', super selectedSlotName, ')'
]

{ #category : 'selecting' }
SequenceableCollectionInspector >> selection [
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."

	self selectionIndexNonMeta <= self objectClassInstSize ifTrue: [^ super selection].
	^ object at: self selectedObjectIndex
]
