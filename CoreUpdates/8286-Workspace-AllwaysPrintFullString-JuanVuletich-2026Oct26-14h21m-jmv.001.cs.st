'From Cuis7.9 [latest update: #8281] on 26 October 2026 at 2:29:19 pm'!
!classDefinition: #Workspace category: #'System-Text'!
TextModel subclass: #Workspace
	instanceVariableNames: 'bindings mustDeclareVariables shouldStyle fullPrintIt '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!Workspace methodsFor: 'initialization' stamp: 'jmv 26/Oct/2026, 5:28:55 pm (UTC)'!
initialize
	
	super initialize.
	self initializeBindings.
	mustDeclareVariables := false.! !


!WorkspaceWindow methodsFor: 'menu' stamp: 'jmv 26/Oct/2026, 5:27:13 pm (UTC)'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addLine.
	aCustomMenu
		add: 'reset variables'
		target: model
		action: #initializeBindings.
	aCustomMenu
		addUpdating: #mustDeclareVariableWording
		target: model
		action: #toggleVariableDeclarationMode.
	aCustomMenu
		addUpdating: #toggleStylingLabel
		target: model
		action: #toggleStyling.! !

!WorkspaceWindow methodsFor: 'menu' stamp: 'jmv 26/Oct/2026, 5:27:41 pm (UTC)'!
offerWindowMenu
	| aMenu |
	aMenu := self buildWindowMenu.
	aMenu addLine.
	aMenu
		add: 'reset variables'
		target: model
		action: #initializeBindings
		icon: #warningIcon.
	aMenu
		addUpdating: #mustDeclareVariableWording
		target: model
		action: #toggleVariableDeclarationMode.
	aMenu
		addUpdating: #toggleStylingLabel
		target: model
		action: #toggleStyling.
	aMenu popUpInWorld: self world! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 26/Oct/2026, 5:28:19 pm (UTC)'!
prefProgramming
	^ #(
		#(atMinusDigitMeaning st80 #(st80 ansiSmalltalk disabled) )
		#(allowBlockArgumentAssignment false )
		#(allowNonBooleanReceiversOfBooleanMessages false )
		#(alternativeBrowseIt false )
		#(browseWithPrettyPrint false )
		#(debugHaloHandle true )
		#(debugLogTimestamp true )
		#(debugPreDebugger false )
		#(debugShowDamage false )
		#(decorateBrowserButtons true )
		#(diffsInChangeList true )
		#(diffsWithPrettyPrint false )
		#(extraDebuggerButtons true )
		#(highlightBlockNesting true )
		#(italicsInShout true)
		#(listClassesHierarchically true )
		#(logDebuggerStackToFile false )
		#(prettyPrintRectangularBlocks false )
		#(shiftClickShowsImplementors false )
		#(shoutInWorkspaces true )
		#(workspacesStyleByParagraphs true )
		#(showAnnotations true )
		#(showLinesInHierarchyViews true )
		#(stylingWithEmphasisInWorkspaces false )
		#(systemCategoryAnnotations #(classCount instanceMethodsCount classMethodsCount "linesOfCode" messageSendsCount) )
		#(classAnnotations #(instanceMethodsCount classMethodsCount "linesOfCode" messageSendsCount) )
		#(messageCategoryAnnotations #(messagesCount messageSendsCount) )
		#(methodAnnotations #(timeStamp "linesOfCode" messageSendsCount messageCategory implementorsCount sendersCount packages library changeSets properties) )	
		#(syntaxHighlightingAsYouType true )
		#(showAssignmentAsLeftArrow false )
		#(replaceUnicodeArrowsWithSmalltalkArrows true)
		#(useWideMinusAndCenteredAsteriskGlyphs true )
		#(clearPackagePathsOnImageMove true)
	)! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 26/Oct/2026, 5:28:39 pm (UTC)'!
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."

	self lineSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result |
			| text |
			text := result printText.
			text := text copyReplaceAll: String lfString with: String lfString, ' '.
			text := ' ', text asText shoutDisableEmphasis, ' .'.
			self afterSelectionInsertAndSelect: (text initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false! !

!methodRemoval: Workspace #toggleFullPrintItLabel stamp: 'jmv 26/Oct/2026, 5:27:59 pm (UTC)'!
Workspace removeSelector: #toggleFullPrintItLabel!
!methodRemoval: Workspace #fullPrintIt stamp: 'jmv 26/Oct/2026, 5:28:43 pm (UTC)'!
Workspace removeSelector: #fullPrintIt!
!methodRemoval: Workspace #toggleFullPrintIt stamp: 'jmv 26/Oct/2026, 5:28:07 pm (UTC)'!
Workspace removeSelector: #toggleFullPrintIt!
!methodRemoval: TextModel #fullPrintIt stamp: 'jmv 26/Oct/2026, 5:28:45 pm (UTC)'!
TextModel removeSelector: #fullPrintIt!
!classDefinition: #Workspace category: #'System-Text'!
TextModel subclass: #Workspace
	instanceVariableNames: 'bindings mustDeclareVariables shouldStyle'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
