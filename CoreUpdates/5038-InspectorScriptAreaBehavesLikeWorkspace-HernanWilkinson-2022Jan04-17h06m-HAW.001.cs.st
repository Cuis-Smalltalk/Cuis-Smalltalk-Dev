'From Cuis 6.0 [latest update: #5032] on 4 January 2022 at 6:08:45 pm'!
!classDefinition: #Inspector category: #'Tools-Inspector'!
TextProvider subclass: #Inspector
	instanceVariableNames: 'acceptedContentsCache object selectionIndex context bindings '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Inspector'!

!PluggableTextModel methodsFor: 'binding' stamp: 'HAW 1/4/2022 18:02:38'!
bindingOf: aString

	^textProvider bindingOf: aString! !


!Inspector methodsFor: 'shout styling' stamp: 'HAW 1/4/2022 18:06:01'!
hasBindingThatBeginsWith: aString 

	| beginsWithBlock |
	
	beginsWithBlock := [ :varName | varName beginsWith: aString ].
	
	^(self objectClass allInstVarNames anySatisfy: beginsWithBlock) or: [ bindings keys anySatisfy: beginsWithBlock ]! !

!Inspector methodsFor: 'shout styling' stamp: 'HAW 1/4/2022 17:35:55'!
initializeBindings
	
	bindings _ Dictionary new.
	self changed: #actualContents.
! !

!Inspector methodsFor: 'binding' stamp: 'HAW 1/4/2022 18:02:01'!
bindingOf: aString
	
	(bindings includesKey: aString) ifFalse: [
		aString first isUppercase
			ifTrue: [^nil]
			ifFalse: [bindings at: aString put: nil]].
		
	^bindings associationAt: aString! !


!Inspector methodsFor: 'initialization' stamp: 'HAW 1/4/2022 17:36:05'!
initialize

	super initialize.
	acceptedContentsCache _ ''.
	selectionIndex := 0.
	self initializeEvents.
	self initializeBindings ! !

!Inspector methodsFor: 'shout styling' stamp: 'HAW 1/4/2022 17:32:23'!
bindingNamesDo: aBlock

	self objectClass allInstVarNames do: aBlock.
	bindings keysDo: aBlock! !

!Inspector methodsFor: 'shout styling' stamp: 'HAW 1/4/2022 17:33:50'!
hasBindingOf: aString
	^ (self objectClass allInstVarNames includes: aString) or: [ 	bindings includesKey: aString ]! !


!PluggableTextModel reorganize!
('accessing' actualContents textGetter:textSetter:selectionGetter: textProvider)
('testing' is: refusesToAccept wantsFrameAdornments)
('misc' autoSelectString getSelection refetch releaseCachedState)
('updating' update:)
('commands' acceptContentsFrom:)
('shout styling' shouldStyle:with:)
('initialization' initWith:)
('user interface support' autoCompleterClass editorClass textStylerClass)
('auto complete' classOfThisContext classOfWorkspaceVarNamed: computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('binding' bindingOf:)
!

!classDefinition: #Inspector category: #'Tools-Inspector'!
TextProvider subclass: #Inspector
	instanceVariableNames: 'acceptedContentsCache object selectionIndex context bindings'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Inspector'!

!Inspector reorganize!
('accessing' baseFieldList context: fieldList object: printedSelectionIndices selectedClass selectedClassOrMetaClass update wantsSteps)
('auto complete' computeMessageEntriesIn:ofInstVarNamed:)
('code' doItContext)
('constants' indexHead indexLabelsForSize: indexSize indexTail lastMetaField printStringLimit)
('contents' acceptedStringOrText methodNodeOf:ifErrorsParsing:)
('initialization' initialize initializeEvents inspect:)
('selecting' accept: replaceSelectionValue: safelyPrintWith: selectedObjectIndex selectedSlotName selection selectionIndex selectionIndexNonMeta selectionIsMeta selectionIsUnmodifiable selectionMeta selectionMetaUnknown selectionPrintString toggleIndex:)
('shout styling' bindingNamesDo: hasBindingOf: hasBindingThatBeginsWith: initializeBindings shouldStyle:with:)
('stepping' stepAt:)
('testing' is: shouldAssumeSelectionIsPrinted)
('user commands' inspectSelection)
('user interface support' autoCompleterClassFor: displayLabel editorClassFor: suggestObjectSpecificMenuItemsFor:from: textStylerClassFor:)
('private' changedSelectionIndex classDefinitionChangedFrom:to: objectClass objectClassInstSize objectSize printStringErrorFieldName printStringErrorText)
('private - mirror interface' object:basicAt: object:instVarAt: objectClass: objectSize:)
('doIt/accept/explain' doItReceiver)
('binding' bindingOf:)
!

