"
A verison of the Inspector specialized for inspecting Sets.  It displays the elements of the set like elements of an array.  Note that the indices, being phyical locations in the hash table, are not meaningful outside of the set.
"
Class {
	#name : 'SetInspector',
	#superclass : 'Inspector',
	#category : 'Tools-Inspector',
	#package : 'Tools',
	#tag : 'Inspector'
}

{ #category : 'selecting' }
SetInspector >> arrayIndexForSelection [
	^ (self fieldList at: selectionIndex) asNumber
]

{ #category : 'accessing' }
SetInspector >> fieldList [
	
	(object isNil or: [ object array isNil]) ifTrue: [^ Set new].
	
	^ self baseFieldList, (object array withIndexCollect: [:each :i | each ifNotNil: [i printString]]) select: [:each | each notNil]
]

{ #category : 'user interface support' }
SetInspector >> objectSpecificMenuSpecFrom: aWindow [

	^aWindow menuSpecForSet
]

{ #category : 'menu' }
SetInspector >> removeSelection [

	self selectionIndexFixed ifTrue: [^ self changed: #flash].
	object remove: self selection.
	selectionIndex := 0.
	acceptedContentsCache := ''.
	self changed: #inspectObject.
	self changed: #fieldList.
	self changed: #selectionIndex.
]

{ #category : 'selecting' }
SetInspector >> replaceSelectionValue: anObject [
	self selectionIndexFixed ifTrue: [^ super replaceSelectionValue: anObject].
	^ object array at: self arrayIndexForSelection put: anObject
]

{ #category : 'selecting' }
SetInspector >> selection [

	self selectionIndexFixed ifTrue: [^ super selection].
	^ object array at: self arrayIndexForSelection
]

{ #category : 'selecting' }
SetInspector >> selectionIndexFixed [
	^self selectionIndexNonMeta <= self objectClassInstSize
]
