'From Cuis 5.0 of 7 November 2016 [latest update: #3461] on 2 October 2018 at 4:57:52 pm'!

!TextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:32:43'!
classOfBlockArgNamed: aName

	^ nil! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:55:03'!
classOfBlockTempVarNamed: aName

	^ nil! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:32:49'!
classOfInstVarNamed: aName

	^ nil! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:32:56'!
classOfTempVarNamed: aName

	^ nil! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:33:03'!
classOfThisContext

	^ nil! !

!TextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:33:09'!
classOfWorkspaceVarNamed: aName

	^ nil! !


!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:34:05'!
classOfBlockArgNamed: aName

	^ textProvider classOfBlockArgNamed: aName ! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:56:47'!
classOfBlockTempVarNamed: aName

	^ textProvider classOfBlockTempVarNamed: aName ! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:34:21'!
classOfInstVarNamed: aName

	^ textProvider classOfInstVarNamed: aName ! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:34:36'!
classOfTempVarNamed: aName

	^ textProvider classOfTempVarNamed: aName ! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:34:50'!
classOfThisContext

	^ textProvider classOfThisContext ! !

!PluggableTextModel methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:35:04'!
classOfWorkspaceVarNamed: aName

	^ textProvider classOfWorkspaceVarNamed: aName ! !


!Workspace methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:50:00'!
classOfBindingOf: aName
												
	^ (self bindingOf: aName) value ifNotNil: [ :aValue | aValue class ] ! !

!Workspace methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:50:14'!
classOfTempVarNamed: aName

	^ self classOfBindingOf: aName ! !

!Workspace methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:50:27'!
classOfWorkspaceVarNamed: aName
												
	^ self classOfBindingOf: aName ! !


!TextProvider methodsFor: 'auto complete' stamp: 'HAW 9/28/2018 13:50:02'!
classOfBlockArgNamed: aName

	^ nil! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:54:36'!
classOfBlockTempVarNamed: aName

	^ nil! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 9/28/2018 13:50:17'!
classOfInstVarNamed: aName

	^ nil! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 9/28/2018 13:50:42'!
classOfTempVarNamed: aName

	^ nil! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 9/28/2018 13:51:28'!
classOfThisContext

	^ nil! !

!TextProvider methodsFor: 'auto complete' stamp: 'HAW 9/28/2018 13:51:53'!
classOfWorkspaceVarNamed: aName

	^ nil! !


!Debugger methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:52:39'!
classOfBlockArgNamed: aName

	^self classOfTempVarNamed: aName ! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:52:47'!
classOfBlockTempVarNamed: aName

	^self classOfTempVarNamed: aName ! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:37:49'!
classOfInstVarNamed: aName 
	
	^ contextStackIndex ~= 0 ifTrue: [ (self receiver instVarNamed: aName) class]! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:53:29'!
classOfTempVarNamed: aName
		
	| context tempIndex |
							
	context := self selectedContext.
	tempIndex := context tempNames indexOf: aName ifAbsent: [ ^ nil ].
	
	^ (self debuggerMap namedTempAt: tempIndex in: context) class
	
	! !

!Debugger methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:52:45'!
classOfThisContext
	
	^ self selectedContext class! !


!Inspector methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 15:38:26'!
classOfInstVarNamed: aName 
	
	^ (object instVarNamed: aName) class ! !


!Character methodsFor: 'testing' stamp: 'HAW 10/2/2018 16:31:57'!
isRightBracket

	^self = $]! !


!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:19:24'!
classOfBlockArgNamed: aName

	^ nil! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:55:33'!
classOfBlockTempVarNamed: aName

	^ nil! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:19:34'!
classOfInstVarNamed: aName

	^ nil! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:19:42'!
classOfTempVarNamed: aName

	^ nil! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:19:51'!
classOfThisContext

	^ nil! !

!ClassNameRequestMorph methodsFor: 'auto complete' stamp: 'HAW 10/2/2018 16:19:58'!
classOfWorkspaceVarNamed: aName

	^ nil! !


!AutoCompleter methodsFor: 'keyboard - private' stamp: 'HAW 10/2/2018 16:36:34'!
shouldOpenMorphWhenNoPrefixFor: currentChar 
	
	^currentChar isAlphaNumeric or: [ currentChar isRightBracket ]! !


!AutoCompleter methodsFor: 'keyboard - private' stamp: 'HAW 10/2/2018 16:34:55'!
shouldOpenMorphWhenNoPrefixAt: currentPos 
	
	^  model textSize >= currentPos 
		and: [ currentPos > 0
		and: [ self shouldOpenMorphWhenNoPrefixFor: (model actualContents at: currentPos) ]]! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 10/2/2018 16:29:25'!
computeEntries

	| allSource contextClass specificModel last3Ranges range |
	
	allSource _ model actualContents string.
	specificModel _ self textProviderOrModel.
	contextClass _ self selectedClassOrMetaClassIn: specificModel. 
	last3Ranges _ self parse: (allSource copyFrom: 1 to: position) in: contextClass and: specificModel.
	range _ last3Ranges third ifNil: [ ^entries _ #() ].

	range end = position
		ifTrue: [ self computeEntriesOfMessageOrIdentifiersFor: allSource using: last3Ranges at: range in: contextClass and: specificModel ]
	 	ifFalse: [ self computeMessageEntriesWithEmptyPrefixFor: allSource at: range in: contextClass and: specificModel ].

	! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 10/2/2018 16:50:09'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel
	
	| id receiverClass |
	
	id _ allSource copyFrom: range start to: range end.
	receiverClass _ range rangeType 
		caseOf: {
			[ #globalVar ] 		-> [ (Smalltalk at: id asSymbol) class ].
			[ #self ] 				-> [ contextClass ].
			[ #super ] 				-> [ contextClass superclass ].
			[ #true ] 				-> [ True ].
			[ #false ] 				-> [ False ].
			[ #nil ] 				-> [ UndefinedObject ].
			[ #character ] 		-> [ id first class ].
			[ #number ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #string ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #symbol ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #stringSymbol ]  	-> [ self classOfLiteral: id in: contextClass ].
			[ #instVar ] 			-> [ specificModel classOfInstVarNamed: id ].
			[ #tempVar ] 			-> [ specificModel classOfTempVarNamed: id ].
			[ #workspaceVar ]		-> [ specificModel classOfWorkspaceVarNamed: id ].
			[ #blockArg ]			-> [ specificModel classOfBlockArgNamed: id ].
			[ #blockTempVar ]		-> [ specificModel classOfBlockTempVarNamed: id ].
			[ #thisContext ] 		-> [ specificModel classOfThisContext ]. 
			[ #classVar ] 			-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #poolConstant ] 	-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #blockEnd ]			-> [ BlockClosure ]. } 
		otherwise: [ nil ].
		
	self computeMessageEntries: receiverClass! !

!methodRemoval: SmalltalkCompleter #classOfBlockArgNamed:in:!
SmalltalkCompleter removeSelector: #classOfBlockArgNamed:in:!
!methodRemoval: SmalltalkCompleter #classOfBlockArgNamed:inDebugger:!
SmalltalkCompleter removeSelector: #classOfBlockArgNamed:inDebugger:!
!methodRemoval: SmalltalkCompleter #classOfInstVarNamed:in:!
SmalltalkCompleter removeSelector: #classOfInstVarNamed:in:!
!methodRemoval: SmalltalkCompleter #classOfInstVarNamed:inDebugger:!
SmalltalkCompleter removeSelector: #classOfInstVarNamed:inDebugger:!
!methodRemoval: SmalltalkCompleter #classOfInstVarNamed:inInspector:!
SmalltalkCompleter removeSelector: #classOfInstVarNamed:inInspector:!
!methodRemoval: SmalltalkCompleter #classOfTempVarNamed:in:!
SmalltalkCompleter removeSelector: #classOfTempVarNamed:in:!
!methodRemoval: SmalltalkCompleter #classOfTempVarNamed:inContext:!
SmalltalkCompleter removeSelector: #classOfTempVarNamed:inContext:!
!methodRemoval: SmalltalkCompleter #classOfTempVarNamed:inDebugger:!
SmalltalkCompleter removeSelector: #classOfTempVarNamed:inDebugger:!
!methodRemoval: SmalltalkCompleter #classOfTempVarNamed:inInspector:!
SmalltalkCompleter removeSelector: #classOfTempVarNamed:inInspector:!
!methodRemoval: SmalltalkCompleter #classOfTempVarNamed:inWorkspace:!
SmalltalkCompleter removeSelector: #classOfTempVarNamed:inWorkspace:!
!methodRemoval: SmalltalkCompleter #classOfThisContextIn:!
SmalltalkCompleter removeSelector: #classOfThisContextIn:!
!methodRemoval: SmalltalkCompleter #classOfThisContextInDebugger:!
SmalltalkCompleter removeSelector: #classOfThisContextInDebugger:!
!methodRemoval: SmalltalkCompleter #classOfWorkspaceVarNamed:in:!
SmalltalkCompleter removeSelector: #classOfWorkspaceVarNamed:in:!
!methodRemoval: SmalltalkCompleter #classOfWorkspaceVarNamed:inInspector:!
SmalltalkCompleter removeSelector: #classOfWorkspaceVarNamed:inInspector:!
!methodRemoval: SmalltalkCompleter #classOfWorkspaceVarNamed:inWorkspace:!
SmalltalkCompleter removeSelector: #classOfWorkspaceVarNamed:inWorkspace:!
!methodRemoval: SmalltalkCompleter #shouldOpenMorphWhenNoPrefixFor:!
SmalltalkCompleter removeSelector: #shouldOpenMorphWhenNoPrefixFor:!
!methodRemoval: Debugger #classOfThisContextIn:!
Debugger removeSelector: #classOfThisContextIn:!

!TextModel reorganize!
('accessing' actualContents actualContents: basicActualContents: contents: textSize undoRedoCommands undoRedoCommandsPosition)
('misc' getSelection refetch)
('testing' canBindVariables isTextEmpty wantsFrameAdornments)
('commands' acceptContentsFrom:)
('shout support' privateAddBoldAt: privateRemoveBoldAt:)
('undoable commands' basicReplaceAttributesFrom:to:with: basicReplaceFrom:to:with: commandForReplaceFrom:to:with: logUndoAndAddAttribute:from:to: logUndoAndRemoveAttributes:from:to: logUndoAndReplaceFrom:to:with: redoAndEvaluate: undoAndEvaluate:)
('object serialization' convertToCurrentVersion:refStream:)
('initialization' flushUndoRedoCommands initialize openLabel:)
('copying' postCopy)
('user interface support' autoCompleterClass editorClass formatAndStyleIfNeededWith: textStylerClass)
('auto complete' classOfBlockArgNamed: classOfBlockTempVarNamed: classOfInstVarNamed: classOfTempVarNamed: classOfThisContext classOfWorkspaceVarNamed:)
!


!PluggableTextModel reorganize!
('accessing' actualContents textGetter:textSetter:selectionGetter: textProvider)
('testing' is: refusesToAccept wantsFrameAdornments)
('misc' autoSelectString getSelection refetch)
('updating' update:)
('commands' acceptContentsFrom:)
('shout styling' shouldStyle:with:)
('initialization' initWith:)
('user interface support' autoCompleterClass editorClass textStylerClass)
('auto complete' classOfBlockArgNamed: classOfBlockTempVarNamed: classOfInstVarNamed: classOfTempVarNamed: classOfThisContext classOfWorkspaceVarNamed:)
!


!Workspace reorganize!
('binding' bindingNamesDo: bindingOf: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('variable declarations' mustDeclareVariableWording nameForObject: toggleVariableDeclarationMode)
('gui' openLabel:)
('shout styling' shouldStyle shouldStyle:with: toggleStyling toggleStylingLabel)
('initialization' initialize)
('user interface support' autoCompleterClass editorClass textStylerClass)
('auto complete' classOfBindingOf: classOfTempVarNamed: classOfWorkspaceVarNamed:)
('testing' canBindVariables is:)
!


!TextProvider reorganize!
('contents' acceptedContents acceptedContentsChanged acceptedStringOrText)
('accessing' contentsSelection)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('auto complete' classOfBlockArgNamed: classOfBlockTempVarNamed: classOfInstVarNamed: classOfTempVarNamed: classOfThisContext classOfWorkspaceVarNamed:)
!


!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context:)
('notifier menu')
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack handleLabelUpdatesIn:whenExecuting: peelToFirst proceed restart returnValue: selectPC send stepIntoBlock up where)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('class list' selectedClass)
('dependents access' updateInspectors wantsSteps)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
('breakpoints' toggleBreakOnEntry)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:upTo:toImplement:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUndertand implement:inClass:context: shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnSubclassResponsibility)
('testing' is:)
('*Refactoring' isEditingMethod)
('*Cuis-University-Model' askForSuperclassOf:toImplement:ifCancel:)
('auto complete' classOfBlockArgNamed: classOfBlockTempVarNamed: classOfInstVarNamed: classOfTempVarNamed: classOfThisContext)
!


!Inspector reorganize!
('accessing' baseFieldList context: fieldList i1 i2 object object: selectedClass selectedClassOrMetaClass update wantsSteps)
('selecting' accept: contentsIsString replaceSelectionValue: selectedSlotName selection selectionIndex selectionPrintString selectionUnmodifiable toggleIndex:)
('code' doItContext doItReceiver)
('private' printStringErrorText)
('stepping' stepAt:)
('contents' acceptedStringOrText)
('initialization' initialize inspect:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('user commands' inspectSelection)
('auto complete' classOfInstVarNamed:)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('testing' is:)
!


!ClassNameRequestMorph reorganize!
('auto complete' classOfBlockArgNamed: classOfBlockTempVarNamed: classOfInstVarNamed: classOfTempVarNamed: classOfThisContext classOfWorkspaceVarNamed:)
('user interface support' autoCompleterClassFor:)
!


!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntry)
('entries - private ' canComputeMessageEntriesFor:and: classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: computeIdentifierEntries computeMessageEntries: computeMessageEntriesFor:at:in:and: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithEmptyPrefixFor:at:in:and: parse:in:and: selectedClassOrMetaClassIn: textProviderOrModel)
('testing' opensWithTab)
!

