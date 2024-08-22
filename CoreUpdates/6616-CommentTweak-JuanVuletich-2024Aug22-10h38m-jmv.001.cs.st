'From Cuis7.1 [latest update: #6615] on 22 August 2024 at 10:51:00 am'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 8/22/2024 10:40:06'!
paste
	| objectName |
	
	model canBindVariables ifTrue: [
		"Not pasting String or Text, but some other object.
		Create a Workspace variable pointing to original object (Not a copy!!!!!!)"
		Clipboard contentsOriginalObject ifNotNil: [ :object |
			objectName := model nameForObject: object.
			(model bindingOf: objectName) value: object.
			self replaceSelectionWith: objectName.
			^ self ]].
	^ super paste! !


!SmalltalkEditor reorganize!
('clipboard' clipboardStringOrText replaceUnicodeArrowsWithSmalltalkArrows)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualExtractAsParameter contextualRemoveParameter extractAsParameter)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
('contextual rename' contextualRename contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: selectedClassOrMetaClassOrUndefinedObject withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('do-its' afterCompiling:do:for:in:ifFail: afterCompiling:do:ifFail: debug:receiver:in: debugIt doIt doItProfiling: evaluate:andDo:ifFail:profiled: evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: withReceiverRangeOf:in:selectorPosition:do:)
('editing keys' cancel encloseWith: pasteInitials)
('events' clickAndHalfAt:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('extract to temporary' extractToTemporary)
('extract method' extractMethod)
('menu')
('menu messages' browseClassFromIt browseImplementorsAt: browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingIt paste referencesToIt referencesToSelectedLiteral selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorAt:do:ifBehavior:otherwise: withSelectorUnderCursorDo:ifBehavior:otherwise:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier)
('running tests' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('temp to inst var' temporaryToInstanceVariable withNodeUnderCursorDo:ifAbsent:)
('typing/selecting keys' argNext argPrev displayIfFalse displayIfTrue newLine: normalCharacter:)
('private' codeProvider hasValidCurrentCompiledMethod lineOrNodeSelectAndEmptyCheck: performCodeExtractionRefactoringWith:)
('quick action (shift+click)' hoverHelpStringOfEvaluating: hoverHelpToShowEvaluating: implementorsOfNodeUnder: quickPrintOfNodeUnder:)
('inline method' contextualInlineMethod contextualInlineMethodOf:in: inlineMethodInUsage:)
('inline temporaray variable' inlineTemporaryVariable)
('menu & kbd shortcuts' defaultMenuSpec)
!

