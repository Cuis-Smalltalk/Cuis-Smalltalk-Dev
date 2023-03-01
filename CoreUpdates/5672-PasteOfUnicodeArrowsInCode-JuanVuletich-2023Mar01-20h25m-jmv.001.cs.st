'From Cuis 6.0 [latest update: #5671] on 1 March 2023 at 8:51:36 pm'!

!SmalltalkEditor methodsFor: 'clipboard' stamp: 'jmv 3/1/2023 20:46:05'!
clipboardStringOrText
	| clipContents |
	clipContents _ super clipboardStringOrText asPlainString.
	self replaceUnicodeArrowsWithSmalltalkArrows ifTrue: [
		clipContents _ clipContents copyReplaceAll: `(UnicodeCodePoint codePoint: 16r2190) asString` with: `(Character smalltalkLeftArrow ) asString`.
		clipContents _ clipContents copyReplaceAll: `(UnicodeCodePoint codePoint: 16r2191) asString` with: `(Character smalltalkUpArrow ) asString`.
		].
	^clipContents.! !

!SmalltalkEditor methodsFor: 'clipboard' stamp: 'jmv 3/1/2023 20:39:07'!
replaceUnicodeArrowsWithSmalltalkArrows
	"Useful, for instance, to paste code from TheCuisBook.pdf.
	Some day we may change our minds, or maybe add a preference."

	^true! !


!Editor reorganize!
('accessing' currentAttributes help morph morph: userHasEdited)
('accessing-selection' hasSelection selectionInterval)
('clipboard' clipboardStringOrText clipboardTextPut: copySelection: cut: mass noop: paste:)
('menu messages' paste pasteString)
('new selection' deselectAndPlaceCursorAt: selectInterval: selectInvisiblyFrom:to: selectInvisiblyMark:point: selectLine selectMark:point: selectWord selectWordLeftDelimiters:rightDelimiters: selectWordOrDelimitedText wordLeftDelimiters wordRangeLeftDelimiters:rightDelimiters: wordRangeLeftDelimiters:rightDelimiters:startingAt: wordRangeUnder: wordRightDelimiters)
('nonediting/nontyping keys' cursorDown: cursorLeft: cursorPageDown: cursorPageUp: cursorRight: cursorUp: selectWord:)
('typing/selecting keys' backspace: cursorTopHome: enter: newLine: normalCharacter: returnKey: selectAll selectAll:)
('private' beginningOfLine: beginningOfNextParagraph: beginningOfParagraph: beginningOfText endOfLine: endOfParagraph: endOfText lineSelectAndEmptyCheck: moveCursor:forward:event: nextWordEnd: nextWordEndFrom:goingForwards: nextWordStart: nextWordStartFrom:goingForwards: previousWordStart: sameColumn:newLine:forward: setIndices:forward: wordSelectAndEmptyCheck:)
('menu commands' offerMenuFromEsc:)
('typing support' cmdShortcuts shortcuts)
!


!SmalltalkEditor reorganize!
('clipboard' clipboardStringOrText replaceUnicodeArrowsWithSmalltalkArrows)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualExtractAsParameter contextualExtractAsParameter: contextualRemoveParameter contextualRemoveParameter: extractAsParameter)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: selectedClassOrMetaClassOrUndefinedObject withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('do-its' afterCompiling:do:for:in:ifFail: afterCompiling:do:ifFail: debug:receiver:in: debugIt doIt doItProfiling: doItSourceCodeFor:in: evaluate:andDo:ifFail:profiled: evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: withReceiverRangeOf:in:selectorPosition:do:)
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('events' clickAndHalf)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('extract to temporary' extractToTemporary extractToTemporary:)
('extract method' extractMethod extractMethod:)
('menu' createMenuCollectingOptionsWith: getMenu openMenu:)
('menu messages' browseClassFromIt browseImplementorsAt: browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt referencesToSelectedLiteral selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorAt:do:ifBehavior:otherwise: withSelectorUnderCursorDo:ifBehavior:otherwise:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine: normalCharacter:)
('private' codeProvider hasValidCurrentCompiledMethod performCodeExtractionRefactoringWith:)
('quick action (shift+click)' hoverHelpStringOfEvaluating: hoverHelpToShowEvaluating: implementorsOfNodeUnder: quickPrintOfNodeUnder:)
('inline method' contextualInlineMethod contextualInlineMethod: contextualInlineMethodOf:in: inlineMethodInUsage:)
('inline temporaray variable' inlineTemporaryVariable inlineTemporaryVariable:)
!

