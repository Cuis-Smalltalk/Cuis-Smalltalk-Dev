'From Cuis 5.0 [latest update: #3843] on 19 August 2019 at 1:15:38 pm'!

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 8/19/2019 12:32:23'!
changeSelector: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier

	aNodeUnderCursor isMessageNode 
		ifTrue: [ self changeSelectorOf: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier ]
		ifFalse: [ morph flash ]
! !

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 8/19/2019 12:32:54'!
changeSelectorOf: aMessageNode in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier

	aChangeSelectorApplier 
		createAndValueHandlingExceptionsOn: model textProvider 
		of: aMessageNode 
		in: aSelectedClass 
		at: aSelectedSelector! !

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 8/19/2019 12:33:40'!
changeSelectorTo: aSelector in: aClassToRefactor using: aChangeSelectorApplier
	
	aChangeSelectorApplier 
		createAndValueHandlingExceptionsOn: model textProvider 
		for: aSelector 
		in: aClassToRefactor ! !

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 8/19/2019 12:30:16'!
contextualChangeSelectorInMethodUsing: aChangeSelectorApplier
	
	self
		withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualChangeSelectorOf: methodNode in: selectedClass using: aChangeSelectorApplier ]
		ifErrorsParsing: [ :anError | morph flash ]		! !

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 8/19/2019 12:31:37'!
contextualChangeSelectorOf: aMethodNode in: aSelectedClass using: aChangeSelectorApplier

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self changeSelector: nodeUnderCursor in: aSelectedClass at: aMethodNode selector using: aChangeSelectorApplier ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self changeSelectorTo: aMethodNode selector in: aSelectedClass using: aChangeSelectorApplier ]
				ifFalse: [ morph flash ]]
! !

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 8/19/2019 12:29:50'!
contextualChangeSelectorUsing: aChangeSelectorApplier
	
	self isEditingClassDefinition 
		ifTrue: [ morph flash ]
		ifFalse: [ self ifChangeSelectorCanBeAppliedDo: [ self contextualChangeSelectorInMethodUsing: aChangeSelectorApplier ]]! !


!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 8/19/2019 12:22:15'!
changeKeywordOrder

	model selectedMessageName ifNotNil: [ :oldSelector |
		ChangeKeywordsSelectorOrderApplier createAndValueHandlingExceptions: [
			ChangeKeywordsSelectorOrderApplier on: model for: oldSelector in: model selectedClassOrMetaClass ]].! !


!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 8/19/2019 12:34:29'!
contextualAddParameter
	
	self contextualChangeSelectorUsing: RefactoringApplier addParameterApplier ! !

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 8/19/2019 13:14:32'!
contextualRemoveParameter

	self contextualChangeSelectorUsing: RefactoringApplier removeParameterApplier ! !

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 8/19/2019 13:15:03'!
contextualRemoveParameter: aKeyboardEvent 
	
	self contextualRemoveParameter.
	^true! !

!methodRemoval: CodeWindow #changeKeywordOrder!
CodeWindow removeSelector: #changeKeywordOrder!
!methodRemoval: SmalltalkEditor #addParameter:in:at:!
SmalltalkEditor removeSelector: #addParameter:in:at:!
!methodRemoval: SmalltalkEditor #addParameterOf:in:at:!
SmalltalkEditor removeSelector: #addParameterOf:in:at:!
!methodRemoval: SmalltalkEditor #addParameterOf:in:at:using:!
SmalltalkEditor removeSelector: #addParameterOf:in:at:using:!
!methodRemoval: SmalltalkEditor #addParameterTo:in:!
SmalltalkEditor removeSelector: #addParameterTo:in:!
!methodRemoval: SmalltalkEditor #contextualAddParameterInMethod!
SmalltalkEditor removeSelector: #contextualAddParameterInMethod!
!methodRemoval: SmalltalkEditor #contextualAddParameterOf:in:!
SmalltalkEditor removeSelector: #contextualAddParameterOf:in:!
!methodRemoval: SmalltalkEditor #contextualRemoveParameterInMethod!
SmalltalkEditor removeSelector: #contextualRemoveParameterInMethod!
!methodRemoval: SmalltalkEditor #contextualRemoveParameterOf:in:!
SmalltalkEditor removeSelector: #contextualRemoveParameterOf:in:!
!methodRemoval: SmalltalkEditor #removeParameter:in:at:!
SmalltalkEditor removeSelector: #removeParameter:in:at:!
!methodRemoval: SmalltalkEditor #removeParameterOf:in:at:!
SmalltalkEditor removeSelector: #removeParameterOf:in:at:!
!methodRemoval: SmalltalkEditor #removeParameterTo:in:!
SmalltalkEditor removeSelector: #removeParameterTo:in:!

!SmalltalkEditor reorganize!
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: compileSelectionFor:in:ifFail: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: selectionDoItSourceCodeIn: withReceiverRangeOf:in:selectorPosition:do:)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorUnderCursorDo:ifErrorsParsing: withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector: withSelectorUnderCursorDo:otherwise:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu:)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifChangeSelectorCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('*LiveTyping' actualImplementorsOfIt actualImplementorsOfIt: actualLocalImplementorsOfIt actualLocalImplementorsOfIt: actualSendersOfIt actualSendersOfIt: balloonTypeInfoAt: balloonTypeInfoInMethodAt: balloonTypeInfoOf:in:definedAt: balloonTypeInfoOfInstanceVariableAt: showSelectionTypeInfo showSelectionTypeInfo: showSelectionTypeInfoInMethod showTypeInfoOf:in:definedAt: showTypeInfoOfInstanceVariable typeCheckMethod typeCheckMethod:)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualRemoveParameter contextualRemoveParameter:)
!


!CodeWindow reorganize!
('GUI building' buildLowerPanes buildMorphicAnnotationsPane buildMorphicCodePane defaultAnnotationPaneHeight lineDiffButton lineDiffButtonHelp optionalButtonRow optionalButtonTuples prettyLineDiffButton prettyLineDiffButtonHelp prettyWordDiffButton prettyWordDiffButtonHelp wordDiffButton wordDiffButtonHelp)
('accessing' buttonWithSelector: inheritanceButton)
('updating' decorateButtons decorateForInheritance updateListsAndCode)
('top window' activateAndSendTopToBack:)
('menu commands' browseAllMessages browseClassRefs browseClassVarRefs browseClassVariables browseFullProtocol browseHierarchy browseImplementors browseInstVarDefs browseInstVarRefs browseLocalImplementors browseLocalSendersOfMessages browseMessages browseMethodFull browseProtocol browseSenders browseSendersOfMessages browseUnusedMethods browseVersions findMethodInChangeSets inspectInstances inspectSubInstances makeSampleInstance methodHierarchy openSingleMessageBrowser showUnreferencedClassVars showUnreferencedInstVars)
('menu building' addContentsTogglesTo: offerClassListMenu offerClassListMenu2 offerWhatToShowMenu openMessageListMenu openMessageListMenu2)
('misc' getSelectorAndSendQuery:to: getSelectorAndSendQuery:to:with: selectMessageAndEvaluate: sendQuery:to: useSelector:orGetSelectorAndSendQuery:to:)
('keyboard shortcuts' classListKey:from: messageCatListKey:from: messageListKey:from: systemCatListKey:from:)
('initialization' model:)
('*LiveTyping' browseActualImplementorsOfSelectedMethod browseActualSendersOfSelectedMethod)
!

