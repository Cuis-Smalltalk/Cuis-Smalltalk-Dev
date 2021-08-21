'From Cuis 5.0 [latest update: #4782] on 21 August 2021 at 6:41:34 pm'!

!Inspector methodsFor: 'testing' stamp: 'HAW 8/21/2021 18:38:30'!
shouldPrintSelectionAsString

	^ self selectionIndex = 0 or: [ self selectionIndex = 2 ]! !


!Inspector methodsFor: 'selecting' stamp: 'HAW 8/21/2021 18:39:53'!
selectionPrintString
	"Returns the current selection as a string"
	
	^self shouldPrintSelectionAsString 
		ifTrue: [ self selection ]
		ifFalse: [ self safelyPrintWith: [ self selection printTextLimitedTo: self printStringLimit ] ].! !


!Inspector reorganize!
('accessing' baseFieldList context: fieldList object: selectedClass selectedClassOrMetaClass update wantsSteps)
('auto complete' computeMessageEntriesIn:ofInstVarNamed:)
('code' doItContext)
('constants' indexHead indexLabelsForSize: indexSize indexTail lastMetaField printStringLimit)
('contents' acceptedStringOrText methodNodeOf:ifErrorsParsing:)
('initialization' initialize initializeEvents inspect:)
('selecting' accept: replaceSelectionValue: safelyPrintWith: selectedObjectIndex selectedSlotName selection selectionIndex selectionIndexNonMeta selectionIsMeta selectionIsUnmodifiable selectionMeta selectionMetaUnknown selectionPrintString toggleIndex:)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('stepping' stepAt:)
('testing' is: shouldPrintSelectionAsString)
('user commands' inspectSelection)
('user interface support' autoCompleterClassFor: displayLabel editorClassFor: suggestObjectSpecificMenuItemsFor:from: textStylerClassFor:)
('private' changedSelectionIndex classDefinitionChangedFrom:to: objectClass objectClassInstSize objectSize printStringErrorFieldName printStringErrorText)
('private - mirror interface' object:basicAt: object:instVarAt: objectClass: objectSize:)
('doIt/accept/explain' doItReceiver)
!

