'From Cuis 5.0 [latest update: #4163] on 22 May 2020 at 7:49:10 pm'!

!Inspector methodsFor: 'constants' stamp: 'sqr 5/21/2020 18:34:19'!
indexHead
	"This is the max index shown before skipping to the 
	last i2 elements of very long arrays"
	^ 500! !

!Inspector methodsFor: 'constants' stamp: 'sqr 5/21/2020 18:41:19'!
indexLabelsForSize: aSize

	^aSize <= self indexSize
		ifTrue: [(1 to: aSize) collect: [:i | i printString]]
		ifFalse: [(1 to: self indexHead) , (aSize - self indexTail + 1 to: aSize) collect: [:i | i printString]]! !

!Inspector methodsFor: 'constants' stamp: 'sqr 5/21/2020 18:34:42'!
indexSize
	^self indexHead + self indexTail! !

!Inspector methodsFor: 'constants' stamp: 'sqr 5/21/2020 18:34:25'!
indexTail
	"This is the number of elements to show at the end
	of very long arrays"
	^ 30! !

!Inspector methodsFor: 'constants' stamp: 'sqr 5/21/2020 17:37:50'!
lastMetaField
	"This value must be synchronized with the base field list, the
	string selection indices, and the senders of this message"

	^2! !

!Inspector methodsFor: 'constants' stamp: 'sqr 5/21/2020 17:26:53'!
printStringLimit

	^12000! !

!Inspector methodsFor: 'constants' stamp: 'sqr 5/21/2020 17:30:31'!
stringSelectionIndices

	^#(0 2)! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 17:36:21'!
safelyPrintWith: aBlock

	^aBlock
		on: UnhandledError
		do:
			[:ex |
				ex return:
					(self printStringErrorText
						addAttribute: TextColor red;
						yourself)
			]! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:52:35'!
selectedObjectIndex
	"Answer the index of the inspectee's collection that the current selection refers to."

	| basicIndex |
	basicIndex _ self selectionIndexNonMeta - self objectClassInstSize.
	^(self objectSize <= self indexSize or: [basicIndex <= self indexHead])
		ifTrue: [basicIndex]
		ifFalse: [self objectSize - self indexSize + basicIndex]! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:20:57'!
selectionIndexNonMeta

	^self selectionIndex - self lastMetaField! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:18:07'!
selectionIsMeta

	^self selectionIndex <= self lastMetaField! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:18:36'!
selectionIsUnmodifiable
	"Answer if the current selected variable is modifiable via acceptance in the code pane.  For most inspectors, no selection and a selection of self (selectionIndex = 1) are unmodifiable"

	^self selectionIsMeta! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:20:18'!
selectionMeta
	"Answer the value of the selected meta field"

	self selectionIndex = 0 ifTrue: [^ ''].
	self selectionIndex = 1 ifTrue: [^ object].
	self selectionIndex = 2 ifTrue: [^ self safelyPrintWith: [object longPrintStringLimitedTo: self printStringLimit]].
	^self selectionMetaUnknown! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:20:01'!
selectionMetaUnknown
	
	^'<Unknown meta field index ', self selectionIndex printString, '>'! !

!Inspector methodsFor: 'private' stamp: 'sqr 5/21/2020 18:51:48'!
objectClassInstSize

	^object class instSize! !

!Inspector methodsFor: 'private' stamp: 'sqr 5/21/2020 18:59:15'!
objectSize

	^object basicSize! !

!Inspector methodsFor: 'private' stamp: 'sqr 5/21/2020 18:18:45'!
printStringErrorFieldName

	^self selectionIsUnmodifiable
		ifTrue: ['self']
		ifFalse: [self selectedSlotName ifNil: ['??']]! !


!CompiledMethodInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:27:08'!
selectionIsUnmodifiable
	"Answer if the current selected variable is unmodifiable via acceptance in the code pane.  For most inspectors, no selection and a selection of self (selectionIndex = 1) are unmodifiable"

	^ true! !

!CompiledMethodInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 19:09:37'!
selectionMeta

	self selectionIndex = 0 ifTrue: [^ ''].
	self selectionIndex = 1 ifTrue: [^ object ].
	self selectionIndex = 2 ifTrue: [^ self safelyPrintWith: [object symbolic]].
	self selectionIndex = 3 ifTrue: [^ self safelyPrintWith: [object headerDescription]].
	^self selectionMetaUnknown! !

!CompiledMethodInspector methodsFor: 'constants' stamp: 'sqr 5/21/2020 17:55:47'!
lastMetaField
	"This value must be synchronized with the base field list, the
	string selection indices, and the senders of this message"

	^3! !

!CompiledMethodInspector methodsFor: 'constants' stamp: 'sqr 5/21/2020 17:57:32'!
stringSelectionIndices

	^#(0 2 3)! !


!DictionaryInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:03:31'!
nonFixedSelectionIndex
	^self selectionIndex - self numberOfFixedFields! !

!DictionaryInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:01:20'!
selectionIndexFixed
	^self selectionIndex <= self numberOfFixedFields! !

!DictionaryInspector methodsFor: 'constants' stamp: 'sqr 5/21/2020 17:57:08'!
stringSelectionIndices

	^#(0)! !


!OrderedCollectionInspector methodsFor: 'accessing' stamp: 'sqr 5/22/2020 19:47:10'!
objectSize

	^object size! !

!OrderedCollectionInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 19:33:54'!
selectedSlotName

	self selectionIsMeta ifTrue: [^super selectedSlotName].
	self selectionIndexNonMeta <= self objectClassInstSize ifTrue: [^super selectedSlotName].
	^'(self at: ', super selectedSlotName, ')'! !


!Inspector methodsFor: 'accessing' stamp: 'sqr 5/21/2020 19:03:54'!
fieldList
	"Answer the base field list plus an abbreviated list of indices."

	object class isVariable ifFalse: [^ self baseFieldList].
	^ self baseFieldList , (self indexLabelsForSize: self objectSize)! !

!Inspector methodsFor: 'accessing' stamp: 'sqr 5/21/2020 18:18:41'!
object: anObject 
	"Set anObject to be the object being inspected by the receiver."

	| oldIndex |
	anObject == object
		ifTrue: [self update]
		ifFalse:
			[oldIndex := self selectionIsUnmodifiable ifTrue: [selectionIndex] ifFalse: [0].
			self inspect: anObject.
			oldIndex := oldIndex min: self fieldList size.
			self changed: #inspectObject.
			oldIndex > 0
				ifTrue: [self toggleIndex: oldIndex].
			self changed: #fieldList.
			self acceptedContentsChanged ]! !

!Inspector methodsFor: 'accessing' stamp: 'sqr 5/21/2020 18:18:50'!
selectedClass
	"Answer the class of the receiver's current selection"

	self selectionIsUnmodifiable ifTrue: [^ object class].
	^ self selection class! !

!Inspector methodsFor: 'accessing' stamp: 'sqr 5/21/2020 17:38:47'!
update
	"Reshow contents, assuming selected value may have changed."

	selectionIndex = 0 ifTrue: [^self].
	acceptedContentsCache _ self selectionPrintString.
	self acceptedContentsChanged.
	self changed: #selectionIndex! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 19:01:51'!
replaceSelectionValue: anObject 
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. The value of the selected variable is set to the value, 
	anObject."
	| si instVarIndex |
	self selectionIsUnmodifiable ifTrue: [
		self toggleIndex: (si := selectionIndex).  
		self toggleIndex: si.
		^ object].
	instVarIndex := self selectionIndexNonMeta.
	instVarIndex > self objectClassInstSize
		ifFalse: [^ object instVarAt: instVarIndex put: anObject].
	object class isVariable or: [self error: 'Cannot replace selection'].
	^object basicAt: self selectedObjectIndex put: anObject! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 19:01:10'!
selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	| index |
	self selectionIsMeta ifTrue: [^self selectionMeta].
	self selectionIndexNonMeta <= self objectClassInstSize
		ifTrue: [^ object instVarAt: self selectionIndexNonMeta].
	index _ self selectedObjectIndex.
	^object isString
		ifTrue: [ object at: index ]
		ifFalse: [ object basicAt: index ]! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:30:40'!
selectionPrintString

	(self stringSelectionIndices includes: self selectionIndex) ifTrue: [^self selection].
	^self safelyPrintWith: [self selection printTextLimitedTo: self printStringLimit]! !

!Inspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 19:16:57'!
toggleIndex: anInteger
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. If anInteger is the index of this variable, then deselect it. 
	Otherwise, make the variable whose index is anInteger be the selected 
	item."

	selectionIndex := selectionIndex = anInteger ifTrue: [0] ifFalse: [anInteger].
	acceptedContentsCache _ self selectionPrintString.
	self acceptedContentsChanged.
	self changed: #selectionIndex! !

!Inspector methodsFor: 'stepping' stamp: 'sqr 5/21/2020 17:43:15'!
stepAt: millisecondSinceLast
	| newText |
	newText := self selectionPrintString.
	newText = acceptedContentsCache ifFalse: [
		acceptedContentsCache _ newText.
		self acceptedContentsChanged ]! !

!Inspector methodsFor: 'private' stamp: 'sqr 5/21/2020 17:46:30'!
printStringErrorText

	^('<error in printString: evaluate "',
		self printStringErrorFieldName,
		' printString" to debug>') asText! !


!CompiledMethodInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:28:30'!
selection

	| bytecodeIndex |
	self selectionIsMeta ifTrue: [^self selectionMeta].
	self selectionIndexNonMeta <= object numLiterals
		ifTrue: [ ^ object objectAt: selectionIndex - self lastMetaField + 1 ].
	bytecodeIndex _ selectionIndex - object numLiterals - self lastMetaField.
	^ object at: object initialPC + bytecodeIndex - 1! !


!ContextInspector methodsFor: 'accessing' stamp: 'sqr 5/21/2020 18:54:07'!
selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	| basicIndex |
	self selectionIsMeta ifTrue: [^self selectionMeta].
	self selectionIndexNonMeta <= self objectClassInstSize
		ifTrue: [^ object instVarAt: self selectionIndexNonMeta].
	basicIndex := self selectionIndexNonMeta - self objectClassInstSize.
	^object debuggerMap namedTempAt: basicIndex in: object
! !


!DictionaryInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:04:32'!
replaceSelectionValue: anObject 
	self selectionIndexFixed ifTrue: [^ super replaceSelectionValue: anObject].
	^ object
		at: (keyArray at: self nonFixedSelectionIndex)
		put: anObject! !

!DictionaryInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:04:40'!
selectedKey
	"Create a browser on all senders of the selected key"
	| i |
	i _ self nonFixedSelectionIndex.
	i > 0 ifFalse: [ ^ nil ].
	^keyArray at: i! !

!DictionaryInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:04:52'!
selection

	self selectionIndexFixed ifTrue: [^ super selection].
	^ object at: (keyArray at: self nonFixedSelectionIndex) ifAbsent: nil! !

!DictionaryInspector methodsFor: 'menu' stamp: 'sqr 5/21/2020 18:04:11'!
removeSelection
	selectionIndex = 0 ifTrue: [^ self changed: #flash].
	object removeKey: (keyArray at: self nonFixedSelectionIndex).
	selectionIndex := 0.
	acceptedContentsCache _ ''.
	self calculateKeyArray.
	self changed: #inspectObject.
	self changed: #selectionIndex.
	self changed: #fieldList! !

!DictionaryInspector methodsFor: 'menu' stamp: 'sqr 5/21/2020 18:05:08'!
renameEntryTo: newKey

	| value |
	value := object at: (keyArray at: self nonFixedSelectionIndex).
	object removeKey: (keyArray at: self nonFixedSelectionIndex).
	object at: newKey put: value.
	self calculateKeyArray.
	selectionIndex := self numberOfFixedFields + (keyArray indexOf: newKey).
	self changed: #selectionIndex.
	self changed: #inspectObject.
	self changed: #fieldList.
	self update! !

!DictionaryInspector methodsFor: 'private' stamp: 'sqr 5/21/2020 18:54:20'!
numberOfFixedFields
	^self lastMetaField + self objectClassInstSize! !


!OrderedCollectionInspector methodsFor: 'accessing' stamp: 'sqr 5/22/2020 19:47:49'!
fieldList

	| fieldsHere |
	object isNil ifTrue: [^#()].
	fieldsHere _ [self indexLabelsForSize: self objectSize]
		on: UnhandledError
		do: [:ex | ex return: #()].	
	^self baseFieldList , fieldsHere
"
OrderedCollection new inspect
(OrderedCollection newFrom: #(3 5 7 123)) inspect
(OrderedCollection newFrom: (1 to: 1000)) inspect
"! !

!OrderedCollectionInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:54:40'!
replaceSelectionValue: anObject 
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. The value of the selected variable is set to the value, anObject."

	self selectionIndexNonMeta <= self objectClassInstSize
		ifTrue: [^ super replaceSelectionValue: anObject].
	object at: self selectedObjectIndex put: anObject! !

!OrderedCollectionInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:53:08'!
selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."

	self selectionIndexNonMeta <= self objectClassInstSize ifTrue: [^ super selection].
	^ object at: self selectedObjectIndex! !


!SetInspector methodsFor: 'menu' stamp: 'sqr 5/21/2020 18:53:25'!
removeSelection
	selectionIndex <= self objectClassInstSize ifTrue: [^ self changed: #flash].
	object remove: self selection.
	selectionIndex := 0.
	acceptedContentsCache _ ''.
	self changed: #inspectObject.
	self changed: #fieldList.
	self changed: #selectionIndex.! !

!SetInspector methodsFor: 'selecting' stamp: 'sqr 5/21/2020 18:53:34'!
selection
	self selectionIsMeta ifTrue: [^self selectionMeta].
	self selectionIndexNonMeta <= self objectClassInstSize
		ifTrue: [^ object instVarAt: self selectionIndexNonMeta].
	^ object array at: self arrayIndexForSelection! !


!Float64Array methodsFor: 'inspecting' stamp: 'sqr 5/22/2020 19:48:34'!
inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^OrderedCollectionInspector! !

!methodRemoval: OrderedCollectionInspector #selectedObjectIndex stamp: 'sqr 5/22/2020 19:45:16'!
OrderedCollectionInspector removeSelector: #selectedObjectIndex!
!methodRemoval: DictionaryInspector #contentsIsString stamp: 'sqr 5/22/2020 19:45:16'!
DictionaryInspector removeSelector: #contentsIsString!
!methodRemoval: CompiledMethodInspector #selectionUnmodifiable stamp: 'sqr 5/22/2020 19:45:16'!
CompiledMethodInspector removeSelector: #selectionUnmodifiable!
!methodRemoval: CompiledMethodInspector #contentsIsString stamp: 'sqr 5/22/2020 19:45:16'!
CompiledMethodInspector removeSelector: #contentsIsString!
!methodRemoval: Inspector #selectionUnmodifiable stamp: 'sqr 5/22/2020 19:45:16'!
Inspector removeSelector: #selectionUnmodifiable!
!methodRemoval: Inspector #contentsIsString stamp: 'sqr 5/22/2020 19:45:16'!
Inspector removeSelector: #contentsIsString!
!methodRemoval: Inspector #i2 stamp: 'sqr 5/22/2020 19:45:16'!
Inspector removeSelector: #i2!
!methodRemoval: Inspector #i1 stamp: 'sqr 5/22/2020 19:45:16'!
Inspector removeSelector: #i1!

!Inspector reorganize!
('accessing' baseFieldList context: fieldList object object: selectedClass selectedClassOrMetaClass update wantsSteps)
('auto complete' computeMessageEntriesIn:ofInstVarNamed:)
('code' doItContext doItReceiver)
('constants' indexHead indexLabelsForSize: indexSize indexTail lastMetaField printStringLimit stringSelectionIndices)
('contents' acceptedStringOrText methodNodeOf:ifErrorsParsing:)
('initialization' initialize inspect:)
('selecting' accept: replaceSelectionValue: safelyPrintWith: selectedObjectIndex selectedSlotName selection selectionIndex selectionIndexNonMeta selectionIsMeta selectionIsUnmodifiable selectionMeta selectionMetaUnknown selectionPrintString toggleIndex:)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('stepping' stepAt:)
('testing' is:)
('user commands' inspectSelection)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('private' objectClassInstSize objectSize printStringErrorFieldName printStringErrorText)
!


!OrderedCollectionInspector reorganize!
('accessing' fieldList objectSize)
('selecting' replaceSelectionValue: selectedSlotName selection)
!

!classRemoval: #SequenceableCollectionInspector stamp: 'sqr 5/22/2020 19:48:55'!
Smalltalk removeClassNamed: #SequenceableCollectionInspector!

