'From Cuis7.3 [latest update: #7005] on 24 January 2025 at 11:42:57 am'!

!SmalltalkEditor methodsFor: 'accessing' stamp: 'jmv 1/23/2025 15:46:11'!
selectedClass
	^ self codeProvider selectedClass! !


!EntryFieldMorph methodsFor: 'events-processing' stamp: 'jmv 1/23/2025 16:32:44'!
focusKeyboardFor: aKeyboardEvent
	"If aKeyboardEvent tab or shift-tab use it to navigate keyboard focus.
	ctrl key ignored."
	aKeyboardEvent isTab
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifFalse: [ aKeyboardEvent hand keyboardFocusNext ]
				ifTrue: [ aKeyboardEvent hand keyboardFocusPrevious ].
			^ true ].
	^super focusKeyboardFor: aKeyboardEvent! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 1/23/2025 16:39:48'!
escAction
	"Return the action to perform when user presses <Esc> key"

	^self valueOfProperty: #escAction! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 1/23/2025 16:38:47'!
processKeystrokeEvent: evt

	(acceptOn = #enter and: [evt isReturnKey]) ifTrue: [^ self acceptContents].

	self pauseBlinking.

	"Return - check for special action"
	evt isReturnKey ifTrue: [
		self crAction ifNotNil: [ :action |
			^action value ]].

	"Esc - check for special action"
	evt isEsc ifTrue: [			
		self escAction ifNotNil: [ :action |
			^action value ]].

	self handleInteraction: [ editor processKeystrokeEvent: evt ].	
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].

	"Only accept (on any) if we have unaccepted edits, otherwise the morph will flash."
	(hasUnacceptedEdits and: [ acceptOn == #any ]) ifTrue: [ self acceptContents ].! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 1/23/2025 16:53:15'!
findMethodSourceContainingIt
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source).  Slow!!"
	| searchString selection scopeClass scopeClassName includeSubclasses includeSuperclasses labelList package |

	searchString := StringRequestMorph
		request: 'Enter search text:'
		initialAnswer: self selectedString
		orCancel: [^self].

	scopeClass := self selectedClass.
	scopeClassName := scopeClass name asString.
	includeSubclasses := false.
	includeSuperclasses := false.

	labelList := {
		'This method'. 
		'All methods in image'. } asOrderedCollection.
	(scopeClass notNil)
		ifTrue: [
			labelList  addAll: {
				'Class ', scopeClassName printString.
				'Class ', scopeClassName printString, ' and subclasses'.
				'Class ', scopeClassName printString, ', subclasses, and superclasses'.}.
			package := self codeProvider isEditingExistingMethod
				ifTrue: [CodePackage packageOfMethod: self codeProvider currentCompiledMethod asMethodReference ifNone: []]
				ifFalse: [CodePackage packageOfClass: scopeClass ifNone: []].
			package ifNotNil: [labelList add: 'In my package: ', package packageName]].

	selection := (SelectionMenu labelList: labelList selections: nil) startUpWithCaption: 'Search Scope'.

	(selection = 0) ifTrue:[^self].
	(selection = 1) "This method"
		ifTrue:[^self find: searchString].
	(selection = 2) "All methods in image"
		ifTrue:[scopeClass := nil].
	"(selection = 3) ifTrue:[]. ""Class only - nothing to do"
	(selection = 4) "Class and subclasses"
		ifTrue:[includeSubclasses := true].
	(selection = 5) "Class, subclasses, and superclasses"
		ifTrue:[
			includeSubclasses := true.
			includeSuperclasses := true].
	(selection = 6) "In my package if any "
		ifTrue: [^ Smalltalk browseMethodsWithSourceString: searchString in: package].
	Smalltalk browseMethodsWithSourceString: searchString scopeClass: scopeClass includeSubclasses: includeSubclasses includeSuperclasses: includeSuperclasses! !


!SmalltalkEditor reorganize!
('accessing' codeProvider selectedClass)
('clipboard' clipboardStringOrText replaceUnicodeArrowsWithSmalltalkArrows)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualExtractAsParameter contextualRemoveParameter extractAsParameter)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
('contextual rename' contextualRename contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('do-its' afterCompiling:do:for:in:ifFail: afterCompiling:do:ifFail: debug:receiver:in: debugIt doIt doItProfiling: evaluate:andDo:ifFail:profiled: evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: withReceiverRangeOf:in:selectorPosition:do:)
('editing keys' cancel encloseWith: pasteInitials)
('events' clickAndHalfAt:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimiter: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('extract to temporary' extractToTemporary)
('extract method' extractMethod)
('menu messages' browseClassFromIt browseImplementorsAt: browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingIt paste referencesToIt referencesToSelectedLiteral selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing toggleBreakOnEntry withSelectorAt:do:ifBehavior:otherwise: withSelectorUnderCursorDo:ifBehavior:otherwise:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier)
('running tests' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('temp to inst var' temporaryToInstanceVariable withNodeUnderCursorDo:ifAbsent:)
('typing/selecting keys' argNext argPrev displayIfFalse displayIfTrue newLine: normalCharacter:)
('private' lineOrNodeSelectAndEmptyCheck: performCodeExtractionRefactoringWith:)
('quick action (shift+click)' hoverHelpStringOfEvaluating: hoverHelpToShowEvaluating: implementorsOfNodeUnder: quickPrintOfNodeUnder:)
('inline method' contextualInlineMethod contextualInlineMethodOf:in: inlineMethodInUsage:)
('inline temporaray variable' inlineTemporaryVariable)
('menu & kbd shortcuts' defaultMenuSpec editSubmenuSpec findMethodSourceContainingIt keyboardShortuctsSubmenuSpec refactoringsSubmenuSpec)
!

