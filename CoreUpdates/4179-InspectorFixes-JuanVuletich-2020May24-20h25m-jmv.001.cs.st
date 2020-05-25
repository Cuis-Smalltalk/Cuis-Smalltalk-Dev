'From Cuis 5.0 [latest update: #4176] on 24 May 2020 at 9:22:20 pm'!

!SetInspector methodsFor: 'selecting' stamp: 'jmv 5/24/2020 21:18:03'!
selectionIndexFixed
	^self selectionIndexNonMeta <= self objectClassInstSize! !


!DictionaryInspector methodsFor: 'menu' stamp: 'jmv 5/24/2020 21:15:04'!
removeSelection

	self selectionIndexFixed ifTrue: [^ self changed: #flash].
	object removeKey: (keyArray at: self nonFixedSelectionIndex).
	selectionIndex := 0.
	acceptedContentsCache _ ''.
	self calculateKeyArray.
	self changed: #inspectObject.
	self changed: #selectionIndex.
	self changed: #fieldList! !

!DictionaryInspector methodsFor: 'menu' stamp: 'jmv 5/24/2020 21:15:48'!
renameEntryTo: newKey

	| oldKey value |
	self selectionIndexFixed ifTrue: [^ self changed: #flash].
	oldKey _ keyArray at: self nonFixedSelectionIndex.
	value _ object at: oldKey.
	object removeKey: oldKey.
	object at: newKey put: value.
	self calculateKeyArray.
	selectionIndex := self numberOfFixedFields + (keyArray indexOf: newKey).
	self changed: #selectionIndex.
	self changed: #inspectObject.
	self changed: #fieldList.
	self update! !


!SetInspector methodsFor: 'menu' stamp: 'jmv 5/24/2020 21:18:19'!
removeSelection

	self selectionIndexFixed ifTrue: [^ self changed: #flash].
	object remove: self selection.
	selectionIndex := 0.
	acceptedContentsCache _ ''.
	self changed: #inspectObject.
	self changed: #fieldList.
	self changed: #selectionIndex.! !

!SetInspector methodsFor: 'selecting' stamp: 'jmv 5/24/2020 21:20:23'!
replaceSelectionValue: anObject
	self selectionIndexFixed ifTrue: [^ super replaceSelectionValue: anObject].
	^ object array at: self arrayIndexForSelection put: anObject! !

!SetInspector methodsFor: 'selecting' stamp: 'jmv 5/24/2020 21:21:47'!
selection

	self selectionIndexFixed ifTrue: [^ super selection].
	^ object array at: self arrayIndexForSelection! !

