'From Cuis7.1 [latest update: #6820] on 19 November 2024 at 5:01:14 pm'!

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/19/2024 15:34:12'!
defaultMenuSpec
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	| codeProvider acceptEntry methodEntries isEditingAMethod |
	isEditingAMethod := false.
	codeProvider := self codeProvider.
	((codeProvider is: #CodeProvider) and: [ codeProvider isEditingMethod ])
		ifTrue: [
			isEditingAMethod := true.
			acceptEntry := self hasUnacceptedEdits
				ifTrue: [ #(270				'Save (s)' 							$s 	editor 			acceptContents 						acceptIcon 							'Save') ]
				ifFalse: [ #(270 				'Save' 							nil 	editor 			nil 						acceptIcon 							'Save') ].
			methodEntries := #(
				#(20 		'-----')
				#(30		'What to Show' 									nil 	nil 			3000 						preferencesIcon 							'Select alternative views of the method')
				#(40		'Toggle Break' 									nil 	editor 			toggleBreakOnEntry 						debugIcon 							'Add or remove a Breakpoint')
				#(200 		'-----')
				#(210		'Refactorings' 									nil 	nil 			2000 						listAddIcon)
				#(280		'Save & Run Test in Class (t)' 									$t 	editor 			acceptAndTest 						weatherFewCloudsIcon 							'Saves method, runs it as test and then all tests in class. Opens debugger if error.')
				#(290		'Save & Run Test in Category (y)' 									$y 	editor 			acceptAndTestAll 						weatherFewCloudsIcon 							'Saves method, runs it as test and then all tests in class category. Opens debugger if error.')
				#(300		'Save & Debug Test (r)' 									$r 	editor 			acceptAndDebugTest 						weatherFewCloudsIcon 							'Saves method and debugs it')
			) ]
		ifFalse: [
			acceptEntry := self hasUnacceptedEdits
				ifTrue: [ #(270				'Save to file (s)' 							$s 	editor 			acceptContents 						acceptIcon 							'Save contents to a file') ]
				ifFalse: [ #(270 				'Save to file' 							nil 	editor 			nil 						acceptIcon 							'Save contents to a file') ]].
	^Array streamContents: [ :strm |
		strm nextPutAll:
			#(
				#(0 		'Smalltalk Editor' 					true)
				#(10		'Help... (h)' 							$h 	editor 			openHelp 						helpIcon 							'Open Help')
				#(50 		'-----')
				#(60		'Do it (d)' 							$d 	editor 			doIt 						doItIcon 							'Do it (selection is a valid expression)')
				#(70		'Print it (p)' 							$p 	editor 			printIt 						printIcon 							'Print it (selection is a valid expression)')
				#(80		'Inspect it (i)' 							$i 	editor 			inspectIt 						inspectIcon 							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
				#(90		'Explore it (I)' 							$I 	editor 			exploreIt 						exploreIcon							'Inspect via Object Explorer')
				#(100		'Debug it (D)' 							$D 	editor 			debugIt 						debugIcon							'Debug it')
				#(110		'Profile it' 							nil 	editor 			profileIt 						clockIcon)
				#(120 		'-----')
				#(130		'Edit' 							nil 	nil 			1000 						textEditorIcon)
				#(140 		'-----')
				#(150		'Explain' 							nil 	editor 			explain 						helpIcon)
				#(160		'Browse it (b)' 							$b 	editor 			browseIt 						editFindReplaceIcon 							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
				#(170		'Senders (n)' 							$n 	editor 			sendersOfIt 						mailForwardIcon 							'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(180		'Implementors (m)' 							$m 	editor 			implementorsOfIt 						developmentIcon 							'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(190		'References (N)' 							$N 	editor 			referencesToIt 						addressBookIcon 							'References to it (selection is a class name, or cursor is over a class-list or message-list)')
				#(220 		'-----')
				#(230		'Method Strings with it (E)' 							$E	editor 			methodStringsContainingIt 								genericTextIcon 					'Method strings containing it')
				#(240		'Method Source with it (e)' 							$e 	editor 			methodSourceContainingIt 								scriptIcon 					'Method source containing it')
				#(250		'Class Comments with it' 							nil 	editor 			classCommentsContainingIt 								chatIcon)
				#(260 		'-----')
			);
			nextPut: acceptEntry.
		isEditingAMethod ifTrue: [
			strm nextPutAll: methodEntries ].
		strm
			nextPutAll: self editSubmenuSpec;
			nextPutAll: self keyboardShortuctsSubmenuSpec.
		isEditingAMethod ifTrue: [
			strm nextPutAll: self refactoringsSubmenuSpec ].
		isEditingAMethod ifTrue: [
			strm nextPutAll: self codeProvider whatToShowSubmenuSpec ].
	].! !


!SmalltalkEditor reorganize!
('clipboard' clipboardStringOrText replaceUnicodeArrowsWithSmalltalkArrows)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualExtractAsParameter contextualRemoveParameter extractAsParameter)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
('contextual rename' contextualRename contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: selectedClassOrMetaClassOrUndefinedObject withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
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
('private' codeProvider hasValidCurrentCompiledMethod lineOrNodeSelectAndEmptyCheck: performCodeExtractionRefactoringWith:)
('quick action (shift+click)' hoverHelpStringOfEvaluating: hoverHelpToShowEvaluating: implementorsOfNodeUnder: quickPrintOfNodeUnder:)
('inline method' contextualInlineMethod contextualInlineMethodOf:in: inlineMethodInUsage:)
('inline temporaray variable' inlineTemporaryVariable)
('menu & kbd shortcuts' defaultMenuSpec editSubmenuSpec keyboardShortuctsSubmenuSpec refactoringsSubmenuSpec)
!

