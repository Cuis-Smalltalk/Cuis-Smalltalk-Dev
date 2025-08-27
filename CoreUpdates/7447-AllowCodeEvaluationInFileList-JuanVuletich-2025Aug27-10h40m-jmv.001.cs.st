'From Cuis7.5 [latest update: #7447] on 27 August 2025 at 10:43:09 am'!

!FileList methodsFor: 'user interface support' stamp: 'jmv 8/27/2025 10:34:18'!
doItContext
	^ nil! !

!FileList methodsFor: 'user interface support' stamp: 'jmv 8/27/2025 10:32:36'!
doItReceiver
	^ nil! !


!Inspector reorganize!
('accessing' baseFieldList fieldList object: printedSelectionIndices selectedClass selectedClassOrMetaClass update wantsSteps)
('auto complete' computeMessageEntriesIn:ofInstVarNamed:)
('constants' indexHead indexLabelsForSize: indexSize indexTail lastMetaField printStringLimit)
('contents' acceptedStringOrText methodNodeOf:ifErrorsParsing:)
('initialization' initialize initializeEvents inspect:)
('selecting' accept: replaceSelectionValue: safelyPrintWith: selectedObjectIndex selectedSlotName selection selectionIndex selectionIndexNonMeta selectionIsMeta selectionIsUnmodifiable selectionMeta selectionMetaUnknown selectionPrintString toggleIndex:)
('shout styling' aboutToStyleWith: allowsStylingFor:isTextDirty: bindingNamesDo: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('stepping' stepAt:)
('testing' is: shouldAssumeSelectionIsPrinted)
('user commands' inspectSelection)
('user interface support' autoCompleterClassFor: displayLabel doItContext doItReceiver editorClassFor: objectSpecificMenuSpecFrom: textStylerClassFor:)
('private' changedSelectionIndex classDefinitionChangedFrom:to:oldClassDefinition: objectClass objectClassInstSize objectSize printStringErrorFieldName printStringErrorText)
('private - mirror interface' object:basicAt: object:instVarAt: objectClass: objectSize:)
('binding' bindingOf:)
!

