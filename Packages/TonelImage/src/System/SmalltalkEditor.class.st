"
The editor built specifically for Smalltalk code
"
Class {
	#name : 'SmalltalkEditor',
	#superclass : 'TextEditor',
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'explain' }
SmalltalkEditor class >> plateA [
	"As in boiler plate. Answer a string which appears in several explanations"
	
	^ '.  To see the other definitions,', self plateX
]

{ #category : 'explain' }
SmalltalkEditor class >> plateB [
	"As in boiler plate. Answer a string which appears in several explanations"
	
	^ '.  To see the definitions,', self plateX
]

{ #category : 'explain' }
SmalltalkEditor class >> plateX [
	"As in boiler plate. Answer a string which appears in several explanations"
	
	^ ' go to the message list pane, get the menu, and select ''implementors of...''.'
]

{ #category : 'cached state access' }
SmalltalkEditor class >> releaseClassCachedState [

	"We are not calling super to avoid cleansing class vars many times.
	So, repeat inherited class instVars!"
	keyboardActions := nil.
]

{ #category : 'running tests' }
SmalltalkEditor >> acceptAndDebugTest [
	
	^self acceptAndWithMethodDo: [ :aPotencialTestMethod | 
		aPotencialTestMethod isTestMethod ifTrue: [ 
			aPotencialTestMethod methodClass debugAsFailure: aPotencialTestMethod selector ]]
]

{ #category : 'running tests' }
SmalltalkEditor >> acceptAndTest [
	
	self acceptThenTestMethodAndSuite: [ :aMethod | self testSuiteOf: aMethod methodClass ].
]

{ #category : 'running tests' }
SmalltalkEditor >> acceptAndTestAll [
	
	self acceptThenTestMethodAndSuite: [ :aMethod | self testSuiteForCategoryOf: aMethod methodClass ].
]

{ #category : 'running tests' }
SmalltalkEditor >> acceptAndWithMethodDo: aBlock [
	
	| potencialTestMethod |
	
	self acceptContents ifFalse: [ ^false ].
	
	potencialTestMethod := self codeProvider currentCompiledMethod.
	^potencialTestMethod 
		ifNil: [ false ]
		ifNotNil: [
			aBlock value: potencialTestMethod.
			true]
]

{ #category : 'running tests' }
SmalltalkEditor >> acceptThenTestMethodAndSuite: aSuiteBuilder [
	
	self acceptAndWithMethodDo: [ :aPotencialTestMethod |
		self runAndDebuggIfNecessary: aPotencialTestMethod.
		self runTestSuite: (aSuiteBuilder value: aPotencialTestMethod) ]
]

{ #category : 'do-its' }
SmalltalkEditor >> afterCompiling: aSourceCode do: aBlock for: aReceiver in: anEvaluationContext ifFail: aFailBlock [
	
	| methodNode method compiler |
	methodNode := [
		compiler := Compiler new.
		compiler		
			compileNoPattern: aSourceCode
			in: aReceiver class
			context: anEvaluationContext
			notifying: self
			ifFail: [ ^ aFailBlock value ]]
		on: OutOfScopeNotification
		do: [ :ex | ex resume: true ].

	methodNode block returnLast.
	method := methodNode generate.
	method methodNode: methodNode.

	^aBlock value: compiler value: method value: aReceiver value: anEvaluationContext 

]

{ #category : 'do-its' }
SmalltalkEditor >> afterCompiling: aSourceCode do: aBlock ifFail: failBlock [

	^ self
		afterCompiling: aSourceCode
		do: aBlock
		for: self codeProvider doItReceiver
		in: self codeProvider doItContext
		ifFail: failBlock.
]

{ #category : 'contextual push up/down inst. var' }
SmalltalkEditor >> apply: aBlock inClassDefinitionOf: aClassDefinitionNode in: aSelectedClass  [
	
	| analyzer cursorPosition |
	
	analyzer := ClassDefinitionNodeAnalyzer for: aClassDefinitionNode.
	cursorPosition := self startIndex.
	
	(analyzer isAtInstanceVariables: cursorPosition) 
		ifTrue: [ aBlock value: aSelectedClass ]
		ifFalse: [ morph flash ]
	

]

{ #category : 'typing/selecting keys' }
SmalltalkEditor >> argNext [
	"Invoked by cmd-A.
	 Search forward from the end of the selection for a colon followed by
		a space.  Place the text cursor after the space.  If none are found, place the
		text cursor at the end of the text.."

	| start t |
	t := model actualContents.
	start := t findString: ': ' startingAt: self startIndex.
	start = 0 ifTrue: [ start := t size + 1].
	self deselectAndPlaceCursorAt: start + 2.
	^true
]

{ #category : 'typing/selecting keys' }
SmalltalkEditor >> argPrev [
	"Invoked by cmd-Q.
	 Search backwards from the start of the selection for a colon followed by
		a space.  Place the text cursor after the space.  If none are found, place the
		text cursor at the start of the text.."

	| t i |
	t := model actualContents.
	i := self stopIndex.
	i > 1 ifTrue: [
		i := i -2.
		[i > 0 and: [ (t at: i) ~~ $  or: [(t at: i-1) ~~ $: ]]] whileTrue: [
			i := i -1 ].
		self deselectAndPlaceCursorAt: i + 1.
	].
	^true
]

{ #category : 'menu messages' }
SmalltalkEditor >> browseClassFromIt [
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."

	| aClass |
	self wordSelectAndEmptyCheck: [
		^ Smalltalk browseHierarchy: Object selector: nil ].

	aClass := Utilities
		classFromPattern: self selectedString withBlanksCondensed
		withCaption: 'choose a class to browse...'.
	aClass ifNil: [^ morph flash ].

	Smalltalk
		browseHierarchy: aClass
		selector: nil.
]

{ #category : 'menu messages' }
SmalltalkEditor >> browseImplementorsAt: aPosition [

	self 
		withSelectorAt: aPosition 
		do: [ :selector | Smalltalk browseAllImplementorsOf: selector ]
		ifBehavior: [ :behavior | Smalltalk browseFull: behavior selector: nil ]
		otherwise: [ self implementorsOfItWhenErrorsParsing ].
]

{ #category : 'menu messages' }
SmalltalkEditor >> browseIt [
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry |
	(Preferences at: #alternativeBrowseIt) ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [
		^ Smalltalk browseFull: Object selector: nil ].

	aSymbol := self selectedSymbol ifNil: [
		[
			self
				evaluate: self selection string
				andDo: [ :result | result class name ]
				ifFail: [ ^morph flash ]
				profiled: false
		] on: ParserNotification do: [ ^morph flash ]].

	aSymbol first isUppercase
		ifTrue: [
			anEntry := (Smalltalk
				at: aSymbol
				ifAbsent: [
					Smalltalk browseAllImplementorsOf: aSymbol.
					^ nil]).
			anEntry ifNil: [^ morph flash].
			(anEntry isKindOf: Class)
				ifFalse: [anEntry := anEntry class].
			Smalltalk browseFull: anEntry selector: nil ]
		ifFalse: [
			Smalltalk browseAllImplementorsOf: aSymbol ].
]

{ #category : 'editing keys' }
SmalltalkEditor >> cancel [
	"Cancel all edits done. Revert to saved version."
	model refetch.
]

{ #category : 'contextual add/remove parameter' }
SmalltalkEditor >> changeSelector: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier [

	aNodeUnderCursor isMessageNode 
		ifTrue: [ self changeSelectorOf: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier ]
		ifFalse: [ morph flash ]

]

{ #category : 'contextual add/remove parameter' }
SmalltalkEditor >> changeSelectorOf: aMessageNode in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier [

	aChangeSelectorApplier
		on: aMessageNode
		createAndValueHandlingExceptionsOn: model textProvider
		in: aSelectedClass
		at: aSelectedSelector
]

{ #category : 'contextual add/remove parameter' }
SmalltalkEditor >> changeSelectorTo: aSelector in: aClassToRefactor using: aChangeSelectorApplier [
	
	aChangeSelectorApplier 
		createAndValueHandlingExceptionsOn: model textProvider 
		for: aSelector 
		in: aClassToRefactor 
]

{ #category : 'menu messages' }
SmalltalkEditor >> classCommentsContainingIt [
	"Open a browser class comments which contain the current selection somewhere in them."

	self lineSelectAndEmptyCheck: [^ self].
	Smalltalk browseClassCommentsWithString: self selectedString
]

{ #category : 'menu & kbd shortcuts' }
SmalltalkEditor >> classDefRefactoringsSubmenuSpec [
	" 'Refactorings' SubMenu"

	^#(
		#(2010 	'-----')
		#(2020	'Rename... (R)' 						$R 	editor 	contextualRenameInClassDefinition 	saveAsIcon 	'Renames what is under cursor')
		#(2030 	'-----')
		#(2040	'Push Up Instance Variable' 			nil 	editor 	contextualPushUpInClassDefinition 	goTopIcon)
		#(2050	'Push Down Instance Variable' 		nil 	editor 	contextualPushDownInClassDefinition 	goBottomIcon)
	).
]

{ #category : 'events' }
SmalltalkEditor >> clickAndHalfAt: localEventPosition [

	| here |
	here := self pointIndex.
	(here between: 2 and: model textSize)
		ifTrue: [
			super clickAndHalfAt: localEventPosition ]
		ifFalse: [
			"if at beginning or end, select entire string"
			self selectAll ]
]

{ #category : 'clipboard' }
SmalltalkEditor >> clipboardStringOrText [
	"When pasting possibly external text into Smalltalk code,
	assume any Unicode left and up arrows are meant to be Assignment and Return.
	This is just a help to users. Not really necessary for the system itself."
	| clipContents |
	clipContents := super clipboardStringOrText.
	self replaceUnicodeArrowsWithSmalltalkArrows ifTrue: [
		clipContents := clipContents copyReplaceAll: `(Character codePoint: 16r2190) asString` with: `(Character smalltalkLeftArrow ) asString`.
		clipContents := clipContents copyReplaceAll: `(Character codePoint: 16r2191) asString` with: `(Character smalltalkUpArrow ) asString`.
		].
	^clipContents.
]

{ #category : 'accessing' }
SmalltalkEditor >> codeProvider [
	^ (model is: #hasTextProvider)
		ifTrue: [ model textProvider ]
		ifFalse: [ model ]
]

{ #category : 'contextual add/remove parameter' }
SmalltalkEditor >> contextualAddParameter [
	
	self contextualChangeSelectorUsing: RefactoringApplier addParameterApplier 
]

{ #category : 'contextual add/remove parameter' }
SmalltalkEditor >> contextualChangeSelectorInMethodUsing: aChangeSelectorApplier [
	
	self
		withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualChangeSelectorOf: methodNode in: selectedClass using: aChangeSelectorApplier ]
		ifErrorsParsing: [ :anError | morph flash ]		
]

{ #category : 'contextual add/remove parameter' }
SmalltalkEditor >> contextualChangeSelectorOf: aMethodNode in: aSelectedClass using: aChangeSelectorApplier [

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self changeSelector: nodeUnderCursor in: aSelectedClass at: aMethodNode selector using: aChangeSelectorApplier ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self changeSelectorTo: aMethodNode selector in: aSelectedClass using: aChangeSelectorApplier ]
				ifFalse: [ morph flash ]]

]

{ #category : 'contextual add/remove parameter' }
SmalltalkEditor >> contextualChangeSelectorUsing: aChangeSelectorApplier [
	
	self ifSourceCodeRefactoringCanBeAppliedDo: [ self contextualChangeSelectorInMethodUsing: aChangeSelectorApplier ]
]

{ #category : 'contextual add/remove parameter' }
SmalltalkEditor >> contextualExtractAsParameter [

	self ifSourceCodeRefactoringCanBeAppliedDo: [ self extractAsParameter ]
]

{ #category : 'inline method' }
SmalltalkEditor >> contextualInlineMethod [

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		self
			withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualInlineMethodOf: methodNode in: selectedClass ]
			ifErrorsParsing: [ :anError | morph flash ]	]	
]

{ #category : 'inline method' }
SmalltalkEditor >> contextualInlineMethodOf: aMethodNode in: aSelectedClass [

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor |
			nodeUnderCursor isMessageNode ifFalse: [^morph flash].

			((aMethodNode completeSourceRangesOf: nodeUnderCursor ifAbsent: [self shouldNotHappen])
				detect: [ :aSourceRange | aSourceRange includes: self startIndex]
					ifFound: [ :aSourceRange | | messageNodeReference |
						messageNodeReference := MessageNodeReference
							messageNode: nodeUnderCursor
							selector: aMethodNode selector
							class: aSelectedClass
							completeSourceRange: aSourceRange.
						self inlineMethodInUsage: messageNodeReference.	]
					ifNone: [self shouldNotHappen ])]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition ifFalse: [ ^ morph flash ].

			RefactoringApplier inlineMethodApplier
				createAndValueHandlingExceptionsOn: model textProvider
				forMethod: (MethodReference class: aSelectedClass selector: aMethodNode selector)]


]

{ #category : 'contextual push up/down inst. var' }
SmalltalkEditor >> contextualPushDownInClassDefinition  [

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		self
			withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | 
				self apply: [ :aSelectedClass | 
					(PushDownInstanceVariableApplier 
						on: self codeProvider 
						for: self wordUnderCursor 
						at: aSelectedClass ) value ] inClassDefinitionOf: classDefinitionNode in: selectedClass ]
			ifErrorsParsing: [ :anError | morph flash ]]
]

{ #category : 'contextual push up/down inst. var' }
SmalltalkEditor >> contextualPushUpInClassDefinition  [

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		self
			withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | 
				self apply: [ :aSelectedClass | 
					(PushUpInstanceVariableApplier 
						on: self codeProvider 
						for: self wordUnderCursor 
						at: aSelectedClass) value ] inClassDefinitionOf: classDefinitionNode in: selectedClass ]
			ifErrorsParsing: [ :anError | morph flash ]]
]

{ #category : 'contextual add/remove parameter' }
SmalltalkEditor >> contextualRemoveParameter [

	self contextualChangeSelectorUsing: RefactoringApplier removeParameterApplier 
]

{ #category : 'contextual rename' }
SmalltalkEditor >> contextualRenameInClassDefinition  [

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		self
			withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | 
				self contextualRenameInClassDefinitionOf: classDefinitionNode in: selectedClass]
			ifErrorsParsing: [ :anError | morph flash ] ]
]

{ #category : 'contextual rename' }
SmalltalkEditor >> contextualRenameInClassDefinitionOf: aClassDefinitionNode in: aSelectedClass  [
	
	| analyzer cursorPosition |
	
	analyzer := ClassDefinitionNodeAnalyzer for: aClassDefinitionNode.
	cursorPosition := self startIndex.
	
	(analyzer isAtSuperclass: cursorPosition) 
		ifTrue: [ ^self renameClassOn: self codeProvider for: analyzer superclass ].
	
	(analyzer isAtClassName: cursorPosition)
		ifTrue: [ ^self renameClassOn: self codeProvider for: aSelectedClass ].
		
	(analyzer isAtInstanceVariables: cursorPosition) 
		ifTrue: [ |selection variableToRename|
			selection := self selectedString.
			variableToRename := selection isEmpty ifTrue: [ self wordUnderCursor ] ifFalse: [ selection ].
			^self renameInstanceVariableOn: self codeProvider for: variableToRename at: aSelectedClass ].
		
	(analyzer isAtCategory: cursorPosition)
		ifTrue: [ 
			"I'm sure codeProvider is a Browser - Hernan"
			^self codeProvider renameSystemCategory ].
		
	morph flash
	

]

{ #category : 'contextual rename' }
SmalltalkEditor >> contextualRenameInMethod [

	self
		withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualRenameOf: methodNode in: selectedClass]
		ifErrorsParsing: [ :anError | morph flash ]		
]

{ #category : 'contextual rename' }
SmalltalkEditor >> contextualRenameOf: aMethodNode in: aSelectedClass [

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self rename: nodeUnderCursor in: aSelectedClass at: aMethodNode ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self ifSourceCodeRefactoringCanBeAppliedDo: [ self renameSelectorFor: aMethodNode selector in: aSelectedClass ]]
				ifFalse: [ morph flash ]]



]

{ #category : 'do-its' }
SmalltalkEditor >> debug: aCompiledMethod receiver: anObject in: evalContext [

	Debugger
		openDebugging: [
			aCompiledMethod
				valueWithReceiver: anObject
				arguments: (evalContext ifNil: [ #() ] ifNotNil: [ { evalContext } ])]
		to: aCompiledMethod
		label: 'Debug it'.
]

{ #category : 'do-its' }
SmalltalkEditor >> debugIt [

	self lineSelectAndEmptyCheck: [^self].
	self 
		afterCompiling: self selection string
		do: [ :compiler :method :receiver :context |
			(method notNil and: [method isQuick not]) ifTrue: [
				self debug: method receiver: receiver in: context ]]
		ifFail: nil.
]

{ #category : 'menu & kbd shortcuts' }
SmalltalkEditor >> defaultMenuSpec [
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	| codeProvider acceptEntry additionalEntries isEditingAMethod isEditingAClassDef |
	isEditingAMethod := false.
	isEditingAClassDef := false.
	additionalEntries := nil.
	codeProvider := self codeProvider.
	(codeProvider is: #CodeProvider)
		ifTrue: [
			acceptEntry := self hasUnacceptedEdits
				ifTrue: [ #(270	'Save (s)' 				$s 	editor 	acceptContents 		acceptIcon 				'Save') ]
				ifFalse: [ #(270 	'Save' 					nil 	editor 	nil 						acceptIcon 				'Save') ].
			codeProvider isEditingMethod ifTrue: [
				isEditingAMethod := true.
				additionalEntries := #(
					#(20 	'-----')
					#(30	'What to Show' 					nil 	nil 		3000 					preferencesIcon 		'Select alternative views of the method')
					#(40	'Toggle Break' 					nil 	editor 	toggleBreakOnEntry 	debugIcon 				'Add or remove a Breakpoint')
					#(200 	'-----')
					#(210	'Refactorings' 					nil 	nil 		2000 					listAddIcon)
					#(280	'Save & Run Test in Class (t)' 		$t 	editor 	acceptAndTest 			weatherFewCloudsIcon 'Saves method, runs it as test and then all tests in class. Opens debugger if error.')
					#(290	'Save & Run Test in Category tree (y)' 	$y 	editor 	acceptAndTestAll 		weatherFewCloudsIcon 'Saves method, runs it as test and then all tests in class category tree. Opens debugger if error.')
					#(300	'Save & Debug Test (r)' 			$r 	editor 	acceptAndDebugTest 	weatherFewCloudsIcon 'Saves method and debugs it')
				) ].
			self isEditingClassDefinition ifTrue: [
				isEditingAClassDef := true.
				additionalEntries := #(
					#(200 	'-----')
					#(210	'Refactorings' 					nil 	nil 		2000 					listAddIcon)
				) ]
			]
		ifFalse: [
			acceptEntry := self hasUnacceptedEdits
				ifTrue: [ #(270	'Save to file (s)' 			$s 	editor 	acceptContents 		acceptIcon 				'Save contents to a file') ]
				ifFalse: [ #(270 	'Save to file' 				nil 	editor 	nil 						acceptIcon 				'Save contents to a file') ]].
	^Array streamContents: [ :strm |
		strm nextPutAll:
			#(
				#(0 		'Smalltalk Editor' 				true)
				#(10	'Help... (h)' 					$h 	editor 	openHelp 				helpIcon 				'Open Help')
				#(50 	'-----')
				#(60	'Do it (d)' 					$d 	editor 	doIt 					doItIcon 				'Do it (selection is a valid expression)')
				#(70	'Print it (p)' 					$p 	editor 	printIt 					printIcon 				'Print it (selection is a valid expression)')
				#(80	'Inspect it (i)' 				$i 	editor 	inspectIt 				inspectIcon 			'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
				#(90	'Explore it (I)' 				$I 	editor 	exploreIt 				exploreIcon			'Inspect via Object Explorer')
				#(100	'Debug it (D)' 				$D 	editor 	debugIt 				debugIcon				'Debug it')
				#(110	'Profile it' 					nil 	editor 	profileIt 				clockIcon)
				#(120 	'-----')
				#(130	'Edit' 						nil 	nil 		1000 					textEditorIcon)
				#(140 	'-----')
				#(150	'Explain' 					nil 	editor 	explain 				helpIcon)
				#(160	'Browse it (b)' 				$b 	editor 	browseIt 				editFindReplaceIcon 	'Browse it (selection is a class name or cursor is over a class-list or message-list)')
				#(170	'Senders (n)' 				$n 	editor 	sendersOfIt 			mailForwardIcon 		'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(180	'Implementors (m)' 			$m 	editor 	implementorsOfIt 		developmentIcon 		'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(190	'References (N)' 				$N 	editor 	referencesToIt 			addressBookIcon 		'References to it (selection is a class name, or cursor is over a class-list or message-list)')
				#(220 	'-----')
				#(230	'Find with Scope (e)' 			$e 	editor 	findMethodSourceContainingIt scriptIcon 		'Find with scope')
				#(240	'Method Strings with it (E)' 	$E	editor 	methodStringsContainingIt 	genericTextIcon 'Method strings containing it')
				#(250	'Find in class comments' 		nil 	editor 	classCommentsContainingIt 	chatIcon)
				#(260 	'-----')
			);
			nextPut: acceptEntry.
		additionalEntries notNil ifTrue: [
			strm nextPutAll: additionalEntries ].
		strm
			nextPutAll: self editSubmenuSpec;
			nextPutAll: self keyboardShortuctsSubmenuSpec.
		isEditingAMethod ifTrue: [
			strm nextPutAll: self methodRefactoringsSubmenuSpec ].
		isEditingAMethod ifTrue: [
			strm nextPutAll: self codeProvider whatToShowSubmenuSpec ].
		isEditingAClassDef ifTrue: [
			strm nextPutAll: self classDefRefactoringsSubmenuSpec ].
	].
]

{ #category : 'typing/selecting keys' }
SmalltalkEditor >> displayIfFalse [
	"Replace the current text selection with the text 'ifFalse:'--initiated by 
	cmd-F."

	self addString: 'ifFalse:'.
]

{ #category : 'typing/selecting keys' }
SmalltalkEditor >> displayIfTrue [
	"Replace the current text selection with the text 'ifTrue:'--initiated by 
	cmd-T."

	self addString: 'ifTrue:'.
]

{ #category : 'do-its' }
SmalltalkEditor >> doIt [

	self doItProfiling: false
]

{ #category : 'do-its' }
SmalltalkEditor >> doItProfiling: aBoolean [
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"

	| hadSelection |
	hadSelection := self hasSelection.
	self lineOrNodeSelectAndEmptyCheck: [^ self ].
	self evaluateSelectionAndDo: nil ifFail: nil profiled: aBoolean.
	hadSelection ifFalse: [
		self deselectAndPlaceCursorAt: self stopIndex ].
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."
]

{ #category : 'menu & kbd shortcuts' }
SmalltalkEditor >> editSubmenuSpec [
	" 'Edit' SubMenu"

	^#(
		#(1010 	'-----')
		#(1020	'Find...(f)' 					$f 	editor 	find 					findIcon 			'Find')
		#(1030	'Find Again (g)' 				$g 	editor 	findAgain 				systemIcon 		'Find again')
		#(1040	'Use Selection for Find (j)'	 	$j 	editor 	setSearchString 		saveAsIcon 		'Set selection as search string for find again')
		#(1050 	'-----')
		#(1060	'Undo - multiple (z)' 			$z 	editor 	undo 					undoIcon 			'Undo (multiple levels)')
		#(1070	'Redo - multiple (Z)' 			$Z 	editor 	redo 					redoIcon 			'Redo (multiple levels)')
		#(1080	'Undo / Redo history' 			nil 	editor 	offerUndoHistory 		changesIcon)
		#(1090 	'-----')
		#(1100	'Copy (c)' 					$c 	editor 	copySelection 			copyIcon 			'Copy selection to the Clipboard')
		#(1110	'Cut (x)' 						$x 	editor 	cut 						cutIcon 			'Cut selection and store it in the Clipboard')
		#(1120 	'Compare to Clipboard (C)' 	$C 	editor 	compareToClipboard 	changesIcon 		'Compare selection to clipboard')
		#(1130	'Paste (v)' 					$v 	editor 	paste 					pasteIcon 			'Paste Clipboard contents')
		#(1140	'Paste...' 					nil 	editor 	pasteRecent 			worldIcon)
		#(1150 	'-----')
		#(1160	'Toggle WordWrap' 			nil 	editor 	wrapOnOff 				genericTextIcon)
		#(1170	'Clear Font' 					nil 	editor 	clearFont 				newIcon)
		#(1180	'Set Font... (k)' 				$k 	editor 	offerFontMenu 			preferencesDesktopFontIcon 	'Set font')
		#(1190	'Set Font Size' 				nil 	editor 	offerFontSizeMenu 	preferencesDesktopFontIcon)
	).
]

{ #category : 'editing keys' }
SmalltalkEditor >> encloseWith: aCharacter [
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| startIndex stopIndex oldSelection new enlargedBy |
	"This implementation is only for commenting and uncommenting code.
	Other cases are handled by the super implementation.
	This implementation only handles String and discards Text attributes."
	aCharacter == $" ifFalse: [
		^super encloseWith: aCharacter ].

	startIndex := self startIndex.
	stopIndex := self stopIndex.
	oldSelection := self selection string.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) == $"
				and: [(model actualContents at: stopIndex) == $"]])
		ifTrue: [
			"already enclosed in double colons; make selection no longer a comment"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection withDoubleColonsUndoubled ]
		ifFalse: [
			"not enclosed in double colons; make selection a comment"
			new := oldSelection withDoubleColonsDoubled.
			enlargedBy := new size - oldSelection size.
			self replaceSelectionWith: $" asString, new, $" asString.
			self selectFrom: startIndex+1 to: stopIndex + enlargedBy ].
	^ true.
]

{ #category : 'do-its' }
SmalltalkEditor >> evaluate: aSourceCode andDo: aBlock ifFail: failBlock profiled: doProfile [

	^ self 
		afterCompiling: aSourceCode 
		do: [ :compiler :method :receiver :context |
			method ifNotNil: [
				| result |
				result := compiler evaluateMethod: method to: receiver logged: true profiled: doProfile.
				aBlock notNil ifTrue: [
					aBlock value: result ]]]
		ifFail: failBlock.
]

{ #category : 'do-its' }
SmalltalkEditor >> evaluateSelectionAndDo: aBlock ifFail: failBlock profiled: doProfile [
	"Treat the current selection as an expression; evaluate it and evaluate aBlock with the result
	3 + 4
	"	

	self evaluate: self selection string andDo: aBlock ifFail: failBlock profiled: doProfile
]

{ #category : 'menu messages' }
SmalltalkEditor >> explain [
	"Try to shed some light on what kind of entity the current selection is. 
	The selection must be a single token or construct. Insert the answer after 
	the selection. Send private messages whose names begin with 'explain' 
	that return a string if they recognize the selection, else nil."

	| string tiVars cgVars selectors delimiters numbers sorry reply symbol provider |
	sorry := 'Sorry, I can''t explain that.  Please select a single
token, construct, or special character.'.
	(string := self selectedString) isEmpty
		ifTrue: [reply := '']
		ifFalse: [
			string := string withBlanksTrimmed.
			"Remove space, tab, cr"
			"Temps and Instance vars need only test strings that are all letters"
			(string detect: [:char | char isValidInIdentifiers not]
				ifNone: nil) ifNil: [
					tiVars := (self explainTemp: string)
						ifNil: [self explainInst: string]].
					
			provider := self codeProvider.
			(tiVars == nil and: [provider respondsTo: #explainSpecial:])
				ifTrue: [tiVars := provider explainSpecial: string].
			tiVars := tiVars
				ifNil: [ '']
				ifNotNil: [ tiVars , '\' withNewLines].
			"Context, Class, Pool, and Global vars, and Selectors need 
			only test symbols"
			(Symbol hasInterned: string ifTrue: [:s | symbol := s])
				ifTrue: [
					cgVars := (self explainCtxt: symbol) 
						ifNil: [ (self explainClass: symbol)
							ifNil: [ self explainGlobal: symbol]].
					"See if it is a Selector (sent here or not)"
					selectors := (self explainMySel: symbol)
						ifNil: [(self explainPartSel: string)
							ifNil: [ self explainAnySel: symbol]]]
				ifFalse: [selectors := self explainPartSel: string].
			cgVars := cgVars
				ifNil: [ '']
				ifNotNil: [cgVars , '\' withNewLines].
			selectors := selectors
				ifNil: [ '']
				ifNotNil: [ selectors , '\' withNewLines].
			delimiters := string size = 1
				ifTrue: ["single special characters"
					self explainChar: string]
				ifFalse: ["matched delimiters"
					self explainDelimiter: string].
			numbers := self explainNumber: string.
			numbers ifNil: [numbers := ''].
			delimiters ifNil: [delimiters := ''].
			reply := tiVars , cgVars , selectors , delimiters , numbers].
	reply isEmpty ifTrue: [reply := sorry].

	"morph showBalloon: reply.
	self runningWorld ifNotNil: [ :w | w findATranscript ].
	reply print."
	"This could be a preference."
	self afterSelectionInsertAndSelect: reply
]

{ #category : 'explain' }
SmalltalkEditor >> explainAnySel: symbol  [
	"Is this any message selector?"

	| list reply |
	list := Smalltalk allClassesImplementing: symbol.
	list isEmpty ifTrue: [^nil].
	list size < 12
		ifTrue: [reply := ' is a message selector which is defined in these classes ' , list printString]
		ifFalse: [reply := ' is a message selector which is defined in many classes'].
	^'#' , symbol , reply , '.\' withNewLines, 'Smalltalk browseAllImplementorsOf: #' , symbol
]

{ #category : 'explain' }
SmalltalkEditor >> explainChar: string [
	"Does string start with a special character?"

	| char |
	char := string at: 1.
	char == $. ifTrue: [^'Period marks the end of a Smalltalk statement.  A period in the middle of a number means a decimal point.  (The number is an instance of class Float).'].
	char == $' ifTrue: [^'The characters between two single quotes are made into an instance of class String'].
	char == $" ifTrue: [^'Double quotes enclose a comment.  Smalltalk ignores everything between double quotes.'].
	char == $# ifTrue: [^'The characters following a hash mark are made into an instance of class Symbol.  If parenthesis follow a hash mark, an instance of class Array is made.  It contains literal constants.'].
	(char == $( or: [char == $)]) ifTrue: [^'Expressions enclosed in parenthesis are evaluated first'].
	(char == $[ or: [char == $]]) ifTrue: [^'The code inside square brackets is an unevaluated block of code.  It becomes an instance of BlockClosure and is usually passed as an argument.'].
	(char == ${ or: [char == $}]) ifTrue: [^ 'A sequence of expressions separated by periods, when enclosed in curly braces, are evaluated to yield the elements of a new Array'].
	(char == $< or: [char == $>]) ifTrue: [^'<primitive: xx> means that this method is usually preformed directly by the virtual machine.  If this method is primitive, its Smalltalk code is executed only when the primitive fails.'].
	char == $^ ifTrue: [^'Up arrow means return from this method.  The value returned is the expression following the ^'].
	char == $| ifTrue: [^'Vertical bars enclose the names of the temporary variables used in this method.  In a block, the vertical bar separates the argument names from the rest of the code.'].
	char == $_ ifTrue: [^'Left arrow means assignment.  The value of the expression after the left arrow is stored into the variable before it.'].
	char == $; ifTrue: [^'Semicolon means cascading.  The message after the semicolon is sent to the same object which received the message before the semicolon.'].
	char == $: ifTrue: [^'A colon at the end of a keyword means that an argument is expected to follow.  Methods which take more than one argument have selectors with more than one keyword.  (One keyword, ending with a colon, appears before each argument).', '\\' withNewLines, 'A colon before a variable name just inside a block means that the block takes an agrument.  (When the block is evaluated, the argument will be assigned to the variable whose name appears after the colon).'].
	char == $$ ifTrue: [^'The single character following a dollar sign is made into an instance of class Character'].
	char == $- ifTrue: [^'A minus sign in front of a number means a negative number.'].
	char == $e ifTrue: [^'An e in the middle of a number means that the exponent follows.'].
	char == $r ifTrue: [^'An r in the middle of a bunch of digits is an instance of Integer expressed in a certain radix.  The digits before the r denote the base and the digits after it express a number in that base.'].
	char = Character space ifTrue: [^'the space Character'].
	char = Character tab ifTrue: [^'the tab Character'].
	char = Character cr ifTrue: [^'the carriage return Character'].
	char = Character lf ifTrue: [^'newline. The line feed Character'].
	^nil
]

{ #category : 'explain' }
SmalltalkEditor >> explainClass: symbol  [
	"Is symbol a class variable or a pool variable?"
	| class reply classes |
	(class := self codeProvider selectedClassOrMetaClass) ifNil: [^ nil].
	"no class is selected"
	(class isKindOf: Metaclass)
		ifTrue: [class := class soleInstance].
	classes := (Array with: class)
				, class allSuperclasses.
	"class variables"
	reply := classes detect: [:each | (each classVarNames detect: [:name | symbol = name]
					ifNone: nil)
					notNil]
				ifNone: nil.
	reply ifNotNil: [
		^ String streamContents: [:str |
			str
				nextPutAll: symbol;
				nextPutAll: ' is a class variable, defined in class ';
				nextPutAll: reply printString, '\' withNewLines;
				nextPutAll: 'Smalltalk browseAllCallsOn: (';
				nextPutAll: reply printString;
				nextPutAll: ' classPool associationAt: #';
				nextPutAll: symbol;
				nextPutAll: ').']].
	"pool variables"
	classes do: [:each | (each sharedPools
			detect: [:pool | (pool includesKey: symbol)
					and: 
						[reply := pool.
						true]]
			ifNone: nil)
			notNil].
	reply
		ifNil: [(Undeclared includesKey: symbol)
				ifTrue: [
					^ String streamContents: [:str |
						str
							nextPutAll: symbol;
							nextPutAll: ' is an undeclared variable.';
							nextPutAll: 'Smalltalk browseAllCallsOn: (Undeclared associationAt: #';
							nextPutAll: symbol;
							nextPutAll: ').']]]
		ifNotNil: 
			[classes := Array streamContents: [ :strm |
				Smalltalk
					allBehaviorsDo: [:each | (each sharedPools
							detect: 
								[:pool | 
								pool == reply]
							ifNone: nil)
							notNil ifTrue: [strm nextPut: each]]].
			"Perhaps not print whole list of classes if too long. (unlikely)"
			^ String streamContents: [:str |
				str
					nextPutAll: symbol;
					nextPutAll: ' is a pool variable from the pool ';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asPlainString;
					nextPutAll: ', which is used by the following classes ';
					nextPutAll: classes printString , '\' withNewLines;
					nextPutAll: 'Smalltalk browseAllCallsOn: (';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asPlainString;
					nextPutAll: ' bindingOf: #';
					nextPutAll: symbol;
					nextPutAll: ').']].
	^ nil
]

{ #category : 'explain' }
SmalltalkEditor >> explainCtxt: symbol  [
	"Is symbol a context variable?"

	| reply classes text cls |
	symbol = #nil ifTrue: [reply := ' is a constant.  It is the only instance of class UndefinedObject.  nil is the initial value of all variables.'].
	symbol = #true ifTrue: [reply := ' is a constant.  It is the only instance of class True and is the receiver of many control messages.'].
	symbol = #false ifTrue: [reply := ' is a constant.  It is the only instance of class False and is the receiver of many control messages.'].
	symbol = #thisContext ifTrue: [reply := ' is a context variable.  Its value is always the MethodContext which is executing this method.'].
	cls := self codeProvider selectedClassOrMetaClass.
	cls ifNil: [^ reply].	  "no class known"
	symbol = #self ifTrue: 
			[classes := cls withAllSubclasses.
			classes size > 12
				ifTrue: [text := cls printString , ' or a subclass']
				ifFalse: 
					[classes := classes printString.
					text := 'one of these classes' , (classes copyFrom: 4 to: classes size)].
			reply := ' is the receiver of this message; an instance of ' , text ].
	symbol = #super ifTrue: [reply := 'is just like self.  Messages to super are looked up in the superclass (' , cls superclass printString , ')'].
	^reply ifNotNil: [symbol, reply]
]

{ #category : 'explain' }
SmalltalkEditor >> explainDelimiter: string [
	"Is string enclosed in delimiters?"

	| str |
	(string at: 1) isLetter ifTrue: [^nil].  "only special chars"
	(string first = string last) ifTrue:
			[^ self explainChar: (String with: string first)]
		ifFalse:
			[(string first == $( and: [string last == $)]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first == $[ and: [string last == $]]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first == ${ and: [string last == $}]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first == $< and: [string last == $>]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first == $# and: [string last == $)]) ifTrue:
				[^'An instance of class Array.  The Numbers, Characters, or Symbols between the parenthesis are the elements of the Array.'].
			string first == $# ifTrue:
				[^'An instance of class Symbol.'].
			(string first == $$ and: [string size = 2]) ifTrue:
				[^'An instance of class Character.  This one is the character ', (String with: string last), '.'].
			(string first == $:) ifTrue:
				[str := string allButFirst.
				(self explainTemp: str) ifNotNil: [
					^'An argument to this block will be bound to the temporary variable ',
						str, '.']]].
	^ nil
]

{ #category : 'explain' }
SmalltalkEditor >> explainGlobal: symbol  [
	"Is symbol a global variable?"
	| reply classes |
	reply := Smalltalk at: symbol ifAbsent: [^nil].
	(reply class == Dictionary or:[reply isKindOf: SharedPool class])
		ifTrue: 
			[classes := Set new.
			Smalltalk allBehaviorsDo: [:each | (each sharedPools detect: [:pool | pool == reply]
					ifNone: nil)
					ifNotNil: [classes add: each]].
			classes := classes printString.
			^ String streamContents: [:str |
				str
					nextPutAll: symbol;
					nextPutAll: ' is a global variable.  It is a pool which is used by the following classes ';
					nextPutAll: (classes allButFirst: 5) ]].
	(reply isKindOf: Behavior)
		ifTrue: [^ String streamContents: [:str |
			str
				nextPutAll: symbol;
				nextPutAll: ' is a global variable.  ';
				nextPutAll: symbol;
				nextPutAll: ' is a class in category ';
				nextPutAll: reply category, '.', '\' withNewLines;
				nextPutAll: 'BrowserWindow fullOnClass: ';
				nextPutAll: symbol;
				nextPutAll: ' selector: nil';
				nextPut: $.]].
	symbol == #Smalltalk 
		ifTrue: [^ symbol, ' is a global.  Smalltalk is the only instance of SystemDictionary and holds all global variables.'].
	^ String streamContents: [:str |
		str
			nextPutAll: symbol;
			nextPutAll: ' is a global variable.  ';
			nextPutAll: symbol;
			nextPutAll: ' is ';
			nextPutAll: reply printString ]
]

{ #category : 'explain' }
SmalltalkEditor >> explainInst: string  [
	"Is string an instance variable of this class?"
	| classes cls |
	cls := self codeProvider selectedClassOrMetaClass.
	cls ifNil: [^ nil].	  "no class known"
	classes := (Array with: cls)
				, cls allSuperclasses.
	classes := classes detect: [:each | (each instVarNames
			detect: [:name | name = string] ifNone: nil)
			notNil] ifNone: [^nil].
	classes := classes printString.
	^ String streamContents: [:str |
		str
			nextPutAll: string;
			nextPutAll: ' is an instance variable of the receiver; defined in class ';
			nextPutAll: classes, '\' withNewLines;
			nextPutAll: 'Smalltalk browseAllAccessesTo: ''';
			nextPutAll: string;
			nextPutAll: ''' from: ';
			nextPutAll: classes;
			nextPut: $.]
]

{ #category : 'explain' }
SmalltalkEditor >> explainMySel: symbol  [
	"Is symbol the selector of this method?  Is it sent by this method?  If 
	not, then expalin will call (explainPartSel:) to see if it is a fragment of a 
	selector sent here.  If not, explain will call (explainAnySel:) to catch any 
	selector. "

	| lits classes msg |
	(msg := self codeProvider selectedMessageName) ifNil: [^nil].	"not in a message"
	classes := Smalltalk allClassesImplementing: symbol.
	classes size > 12
		ifTrue: [classes := 'many classes']
		ifFalse: [classes := 'these classes ' , classes printString].
	msg = symbol
		ifTrue: [
			^ String streamContents: [:str |
				str
					nextPut: $#;
					nextPutAll: symbol;
					nextPutAll: ' is the selector of this very method!  It is defined in ';
					nextPutAll: classes;
					nextPutAll: self class plateA]]
		ifFalse: [
			lits := (self codeProvider selectedClassOrMetaClass compiledMethodAt: msg) messages.
			(lits detect: [:each | each == symbol]
				ifNone: nil)
					ifNil: [^nil].
			^ String streamContents: [:str |
				str
					nextPut: $#;
					nextPutAll: symbol;
					nextPutAll: ' is a message selector which is defined in ';
					nextPutAll: classes;
					nextPutAll: self class plateA]]

]

{ #category : 'explain' }
SmalltalkEditor >> explainNumber: string  [
	"Is string a Number?"

	| strm c |
	(c := string at: 1) isDigit ifFalse: [(c == $- and: [string size > 1 and: [(string at: 2) isDigit]])
			ifFalse: [^nil]].
	strm := string readStream.
	c := Number readFrom: strm.
	strm atEnd ifFalse: [ ^nil ].
	c printString = string
		ifTrue: [ ^ string , ' is a ' , c class name ]
		ifFalse: [ ^ string , ' (= ' , c printString , ') is a ' , c class name ]
]

{ #category : 'explain' }
SmalltalkEditor >> explainPartSel: string  [
	"Is this a fragment of a multiple-argument selector sent in this method?"
	| lits whole reply classes s msg |
	(msg := self codeProvider selectedMessageName) ifNil: [^ nil].  "not in a message"
	string last == $: ifFalse: [^ nil].
	"Name of this method"
	lits := Array with: msg.
	(whole := lits detect: [:each | (each keywords detect: [:frag | frag = string]
					ifNone: nil) notNil]
				ifNone: nil)
		ifNotNil: [
			reply := ', which is the selector of this very method!'.
			s := self class plateA]
		ifNil: [ 
			"Selectors called from this method"
			lits := (self codeProvider selectedClassOrMetaClass compiledMethodAt: msg) messages.
			(whole := lits detect: [:each | (each keywords detect: [ :frag | frag = string ]
							ifNone: nil) notNil]
						ifNone: nil) notNil
				ifFalse: [string = 'primitive:'
					ifTrue: [^self explainChar: '<']
					ifFalse: [^nil]].
			reply := '.'.
			s := self class plateB].
		classes := Smalltalk allClassesImplementing: whole.
		classes size > 12
			ifTrue: [classes := 'many classes']
			ifFalse: [classes := 'these classes ' , classes printString].
		^ String streamContents: [:str |
			str
				nextPutAll: string;
				nextPutAll: ' is one part of the message selector ';
				nextPutAll: whole;
				nextPutAll: reply;
				nextPutAll: '  It is defined in ';
				nextPutAll: classes;
				nextPutAll: s]
]

{ #category : 'explain' }
SmalltalkEditor >> explainTemp: string  [
	"Is string the name of a temporary variable (method or block argument or temporary)?"

	| selectedClass methodNode tempNode |
	self codeProvider selectedMessageName ifNil: [^nil].	"not in a method"
	selectedClass := self codeProvider selectedClassOrMetaClass.
	methodNode := selectedClass parserClass new parse: self codeProvider selectedMessage class: selectedClass.
	tempNode := methodNode encoder tempNodes detect: [ :n | n name = string ] ifNone: [^nil].
	^(tempNode isArg
		ifTrue: [string, ' is an argument to this ']
		ifFalse: [string, ' is a temporary variable in this ']),
	   (tempNode isDeclaredAtMethodLevel
		ifTrue: ['method ']
		ifFalse: ['block '])
]

{ #category : 'do-its' }
SmalltalkEditor >> exploreIt [

	| hadSelection |
	hadSelection := self hasSelection.
	self lineOrNodeSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result | result explore ]
		ifFail: [ ^morph flash ]
		profiled: false.
	hadSelection ifFalse: [
		self deselectAndPlaceCursorAt: self stopIndex ].
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."
]

{ #category : 'contextual add/remove parameter' }
SmalltalkEditor >> extractAsParameter [

	^ RefactoringApplier extractAsParameterApplier createAndValueHandlingExceptions: [
		RefactoringApplier extractAsParameterApplier
			from: self selectionInterval 
			on: model textProvider 
			for: self codeProvider selectedMessageName 
			in: self codeProvider selectedClassOrMetaClass  ]
]

{ #category : 'extract method' }
SmalltalkEditor >> extractMethod [

	self performCodeExtractionRefactoringWith: ExtractMethodApplier
]

{ #category : 'extract to temporary' }
SmalltalkEditor >> extractToTemporary [

	self performCodeExtractionRefactoringWith: ExtractToTemporaryApplier
]

{ #category : 'menu messages' }
SmalltalkEditor >> fileItIn [
	"Make a Stream on the text selection and fileIn it."

	self selection fileIn
]

{ #category : 'menu & kbd shortcuts' }
SmalltalkEditor >> findMethodSourceContainingIt [
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source).  Slow!"
	| searchString selection scopeClass scopeClassName includeSubclasses includeSuperclasses labelList package |

	true ifTrue: [
		^FindInSourceDialog openOn: self ].

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
				ifTrue: [CodePackage packageOfMethod: self codeProvider currentCompiledMethod asMethodReference ifNone: nil]
				ifFalse: [CodePackage packageOfClass: scopeClass ifNone: nil].
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
	Smalltalk browseMethodsWithSourceString: searchString scopeClass: scopeClass includeSubclasses: includeSubclasses includeSuperclasses: includeSuperclasses
]

{ #category : 'quick action (shift+click)' }
SmalltalkEditor >> hoverHelpStringOfEvaluating: aNodeUnderCursor [

	^ self
		evaluate: aNodeUnderCursor printSourceCode
		andDo: [ :result | result printString ]
		ifFail: [ 
			morph flash.
			'Nothing to show' ]
		profiled: false
]

{ #category : 'quick action (shift+click)' }
SmalltalkEditor >> hoverHelpToShowEvaluating: aNodeUnderCursor [

	^ [[ self hoverHelpStringOfEvaluating: aNodeUnderCursor]
		on: MessageNotUnderstood 
		do: [ :mnu | | receiverPrintString | 
			"just guessing if the real receiver is the one in the message node to show a nicer error message.
			This will not be correct in cases such as: v1 xx: (v2 xx: 1). In this case will show that v1 does not understand xx: 
			when it is v2 the one that does not understand it, but this is so rare that I leave it anyway - Hernan"
			receiverPrintString := mnu message selector = aNodeUnderCursor selectorSymbol 
				ifTrue: [ aNodeUnderCursor originalReceiver printSourceCode ]
				ifFalse: [ mnu receiver printString ].
			mnu return: receiverPrintString, ' does not understand ', mnu message selector]]
		on: Error 
		do: [ :anError | anError return: anError messageText ]
]

{ #category : 'contextual rename' }
SmalltalkEditor >> ifSourceCodeRefactoringCanBeAppliedDo: aBlock [

	^(self hasUnacceptedEdits or: [morph hasEditingConflicts ])
		ifTrue: [ self inform: 'This refactoring can not be applied when there are unsaved changes' ]
		ifFalse: aBlock
]

{ #category : 'menu messages' }
SmalltalkEditor >> implementorsOfIt [
	"Open an implementors browser.
	If text selection defines a selector, take it. Otherwise, try finding selector under cursor. If this fails, consider the whole line."

	^self selectedSelector 
		ifNil: [ self browseImplementorsAt: self startIndex ]
		ifNotNil: [ :selector | Smalltalk browseAllImplementorsOf: selector ].
	
]

{ #category : 'menu messages' }
SmalltalkEditor >> implementorsOfItWhenErrorsParsing [

	"Open an implementors browser on the selected selector"

	| aSelector |

	self lineSelectAndEmptyCheck: [^ self].
	(aSelector := self selectedSelector) ifNil: [^ morph flash].
	Smalltalk browseAllImplementorsOf: aSelector
]

{ #category : 'quick action (shift+click)' }
SmalltalkEditor >> implementorsOfNodeUnder: aMousePosition [

	| mousePositionInText |
	
	mousePositionInText := morph positionInTextOf: aMousePosition.
	self browseImplementorsAt: mousePositionInText 
]

{ #category : 'inline method' }
SmalltalkEditor >> inlineMethodInUsage: aMessageNodeReference [

	RefactoringApplier inlineMethodApplier
		createAndValueHandlingExceptionsOn: model textProvider
		forMessageSend: aMessageNodeReference.
]

{ #category : 'inline temporaray variable' }
SmalltalkEditor >> inlineTemporaryVariable [

	self codeProvider currentCompiledMethod ifNotNil: [ :cm |
		InlineTemporaryVariableApplier createAndValueHandlingExceptions: [
			InlineTemporaryVariableApplier
				on: self
				for: self selectionInterval asSourceCodeInterval
				of: cm ]].
]

{ #category : 'do-its' }
SmalltalkEditor >> inspectIt [

	| hadSelection |
	hadSelection := self hasSelection.
	self lineOrNodeSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result | result inspect ]
		ifFail: [ ^morph flash ]
		profiled: false.
	hadSelection ifFalse: [
		self deselectAndPlaceCursorAt: self stopIndex ].
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."
]

{ #category : 'contextual rename' }
SmalltalkEditor >> isEditingClassDefinition [

	^(self codeProvider is: #Browser) and: [ self codeProvider isEditingExistingClass ]
]

{ #category : 'menu & kbd shortcuts' }
SmalltalkEditor >> keyboardShortuctsSubmenuSpec [
	"Keyboard shortcuts only"

	^#(
		"Keyboard shortcut only. No menu entry."
		#(1500	nil 	$0 	editor 	changeEmphasisNormal 			nil 	'Normal')
		#(1501	nil 	$1 	editor 	changeEmphasisBold 				nil 	'Bold')
		#(1502	nil 	$2 	editor 	changeEmphasisItalic 				nil 	'Italic')
		#(1503	nil 	$3 	editor 	changeEmphasisUnderlined 		nil 	'Underlined')
		#(1504	nil 	$4 	editor 	changeEmphasisStruckThrough 	nil 	'Struck Through')
		#(1505	nil 	$5 	editor 	changeEmphasisSuperscript 		nil 	'Superscript')
		#(1506	nil 	$6 	editor 	changeEmphasisSubscript 			nil 	'Subscript')
		#(1507	nil 	$l 	editor 	cancel 								nil 	'Cancel all edits')
		#(1508	nil 	$Q 	editor 	argPrev 							nil 	'Previous argument')
		#(1509	nil 	$W 	editor 	argNext 							nil 	'Next argument')
		#(1510	nil 	$T 	editor 	displayIfTrue)
		#(1511	nil 	$F 	editor 	displayIfFalse)
		#(1601 	nil 	$a 	editor 	selectAll 							nil 	'Select all')
		#(1603 	nil 	$U 	editor 	changeLineEndsToLf 				nil 	'Convert line endings to LF characters (Cuis convention) in selection')
		#(1604 	nil 	$( 	editor 	encloseWithParenthesis 			nil 	'Enclose within ( and ), or remove enclosing ( and )')
		#(1605 	nil 	$[ 	editor 	encloseWithSquareBrackets 		nil 	'Enclose within [ and ], or remove enclosing [ and ]')
		#(1606 	nil 	${ 	editor 	encloseWithCurlyBrackets 			nil 	'Enclose within { and }, or remove enclosing { and }')
		#(1607 	nil 	$' 	editor 	encloseWithSingleQuotes 			nil 	'Enclose within single quotes, or remove enclosing single quotes')
		#(1608 	nil 	$" 	editor 	encloseWithDoubleQuotes 			nil 	'Enclose within double quotes, or remove enclosing double quotes')
		#(1609 	nil 	$` 	editor 	encloseWithBackTicks 				nil 	'Enclose within backticks, or remove enclosing backticks')
		"#(1610 nil 	$< 	editor 	encloseWithAngleBrackets 			nil 	'Enclose within < and >, or remove enclosing ( and )')"
		#(1611 	nil 	$H 	editor 	cursorTopHome 					nil 	'Move cursor to start of text')
	).
]

{ #category : 'private' }
SmalltalkEditor >> lineOrNodeSelectAndEmptyCheck: returnBlock [
	"If the current selection is empty, either
		- expand it to be the entire current line
		- or expand it to a meaningful chunk of code.
	If after that's done the selection is still empty, then evaluate the returnBlock."

	self hasSelection ifFalse: [
		(model actualContents isLineEndAt: self pointIndex)
			ifTrue: [ self selectLine ]
			ifFalse: [
				self
					withMethodNodeAndClassDo: [ :methodNode :selectedClass |
						self selectNodeUnderCursorForInspectionIn: methodNode ]
					ifErrorsParsing: [ :anError | ]]].
	self selectionIsEmptyOrSeparators ifTrue: [
		self selectLine ].
]

{ #category : 'menu & kbd shortcuts' }
SmalltalkEditor >> methodRefactoringsSubmenuSpec [
	" 'Refactorings' SubMenu"

	^#(
		#(2010 	'-----')
		#(2020	'Rename... (R)' 						$R 	editor 	contextualRenameInMethod 			saveAsIcon 	'Renames what is under cursor')
		#(2030	'Extract as Parameter... (V)' 			$V 	editor 	contextualExtractAsParameter 		listAddIcon 	'Extracts the selected code as parameter')
		#(2040	'Add Parameter... (A)' 				$A 	editor 	contextualAddParameter 				listAddIcon 	'Adds parameter to message that is under cursor')
		#(2050	'Remove Parameter... (S)' 			$S 	editor 	contextualRemoveParameter 			listRemoveIcon 'Removes parameter to message that is under cursor')
		#(2060 	'-----')
		#(2070	'Extract Temporary... (J)' 				$J 	editor 	extractToTemporary 					saveAsIcon 	'Extracts the selected code into a temporary variable')
		#(2080	'Extract Method... (K)' 				$K 	editor 	extractMethod 							saveAsIcon 	'Extracts the selected code into a separate method')
		#(2090 	'-----')
		#(2100	'Inline Temporary... (O)' 				$O 	editor 	inlineTemporaryVariable 				saveAsIcon 	'Inlines the selected temporary variable into each usage')
		#(2110	'Inline Method... (L)' 					$L 	editor 	contextualInlineMethod 				saveAsIcon  	'Inlines the selected message send into desired colaborations')
		#(2120 	'-----')
		#(2130	'Temporary to Instance Variable (G)' 	$G 	editor 	temporaryToInstanceVariable 			saveAsIcon 	'Extracts temporary variable to instance variable')
	).
]

{ #category : 'menu messages' }
SmalltalkEditor >> methodSourceContainingIt [
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source).  Slow!"

	self lineSelectAndEmptyCheck: [^ self].
	Smalltalk browseMethodsWithSourceString: self selectedString
]

{ #category : 'menu messages' }
SmalltalkEditor >> methodStringsContainingIt [
	"Open a browser on methods which contain the current selection as part of a string constant."

	self lineSelectAndEmptyCheck: [^ self].
	Smalltalk browseMethodsWithString: self selectedString
]

{ #category : 'typing/selecting keys' }
SmalltalkEditor >> newLine: aKeyboardEvent [
	"Replace the current text selection with a newLine (i.e. LF) followed by as many tabs
	as there are leading tabs on the current line (+/- bracket count)."

	| char s i tabCount stopIndex newLineString |
	s := self privateCurrentString.
	stopIndex := self stopIndex.
	i := stopIndex.
	tabCount := 0.
	[ (i := i-1) > 0 and: [ (char := s at: i) isLineSeparator not ] ] whileTrue: [
		"Count brackets"
		char == $[ ifTrue: [tabCount := tabCount + 1].
		char == $] ifTrue: [tabCount := tabCount - 1]].
	[ (i := i + 1) < stopIndex and: [ (char := s at: i) isSeparator ] ] whileTrue: [
		"Count leading tabs"
		char = Character tab ifTrue: [ tabCount := tabCount + 1 ]].
	"Now inject newline with tabCount tabs, generating a new undoable command"
	newLineString := UnicodeString streamContents: [ :strm | strm newLineTab: tabCount ].
	model startNewUndoRedoCommand.
	self replaceSelectionWith: newLineString.
	^ false
]

{ #category : 'new selection' }
SmalltalkEditor >> nextTokenFrom: start direction: dir [
	"simple token-finder for compiler automated corrections"
	| loc str |
	loc := start + dir.
	str := self privateCurrentString.
	[(loc between: 1 and: str size) and: [(str at: loc) isSeparator]]
		whileTrue: [loc := loc + dir].
	^ loc
]

{ #category : 'typing/selecting keys' }
SmalltalkEditor >> normalCharacter: aKeyboardEvent  [
	"A nonspecial character is to be added to the stream of characters."

	| string key |

	"To enable:
	Preferences name: #autoReplaceNamedCharacters category: #gui value: true.
	Preferences saveToDisk: #autoReplaceNamedCharacters.
	PreferenceSet saveUserPrefs.
	"
	"To disable:
	Preferences name: #autoReplaceNamedCharacters category: #gui value: false.
	Preferences saveToDisk: #autoReplaceNamedCharacters.
	PreferenceSet saveUserPrefs.
	"
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	(Preferences at: #autoReplaceNamedCharacters) ifTrue: [
		aKeyboardEvent keyCharacter isLetter ifFalse: [
			string := self privateCurrentString.
			"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
			self pointIndex - 1 to: (self pointIndex - 20 max: 1) by: -1 do: [:i|
				(string at: i) == $\ ifTrue: [
					key := string copyFrom: i + 1 to: self pointIndex - 1.
					Character namedCharactersMap at: key ifPresent: [ :value |
						self selectFrom: i to: self pointIndex - 1; replaceSelectionWith: (UnicodeString with: value) ].
					^ super normalCharacter: aKeyboardEvent]]]].

	^ super normalCharacter: aKeyboardEvent
]

{ #category : 'new selection' }
SmalltalkEditor >> notify: aString at: anInteger in: aStream  [
	"The compilation of text failed. The syntax error is noted as the argument, 
	aString. Insert it in the text at starting character position anInteger."
	"This is a user command, and generates undo"
	model startNewUndoRedoCommand.
	self insertAndSelect: aString at: (anInteger max: 1).
]

{ #category : 'menu messages' }
SmalltalkEditor >> paste [
	| objectName |
	
	model canBindVariables ifTrue: [
		"Not pasting String or Text, but some other object.
		Create a Workspace variable pointing to original object (Not a copy!!!)"
		Clipboard contentsOriginalObject ifNotNil: [ :object |
			objectName := model nameForObject: object.
			(model bindingOf: objectName) value: object.
			self replaceSelectionWith: objectName.
			^ self ]].
	self pasteString.
]

{ #category : 'editing keys' }
SmalltalkEditor >> pasteInitials [
	"Replace the current text selection by an authorship name/date stamp; invoked by cmd-shift-v, easy way to put an authorship stamp in the comments of an editor."
	"This is a user command, and generates undo"

	| i |
	i := self stopIndex.
	self replaceSelectionWith: (Text fromString: Utilities changeStamp).
	self deselectAndPlaceCursorAt: i.
]

{ #category : 'private' }
SmalltalkEditor >> performCodeExtractionRefactoringWith: aRefactoringApplierClass [

	self codeProvider currentCompiledMethod ifNotNil: [ :cm |
		self ifSourceCodeRefactoringCanBeAppliedDo: [
			aRefactoringApplierClass createAndValueHandlingExceptions: [
				aRefactoringApplierClass
					on: self codeProvider
					for: self selectionInterval asSourceCodeInterval
					of: cm ] ] ].
]

{ #category : 'do-its' }
SmalltalkEditor >> printIt [
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."

	self lineSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result |
			| text |
			text := model fullPrintIt
				ifTrue: [result printText]
				ifFalse: [result printTextLimitedTo: 10000].
			text := text copyReplaceAll: String lfString with: String lfString, ' '.
			text := ' ', text asText shoutDisableEmphasis, ' .'.
			self afterSelectionInsertAndSelect: (text initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false
]

{ #category : 'do-its' }
SmalltalkEditor >> profileIt [

	self doItProfiling: true
]

{ #category : 'quick action (shift+click)' }
SmalltalkEditor >> quickPrintOfNodeUnder: aMousePosition [

	| mousePositionInText |
	
	mousePositionInText := morph positionInTextOf: aMousePosition.

	self 
		withMethodNodeAndClassDo: [ :methodNode :class | 
			methodNode 
				withParseNodeIncluding: mousePositionInText 
				do: [ :aNodeUnderCursor | morph showBalloon: (self hoverHelpToShowEvaluating: aNodeUnderCursor)]
				ifAbsent: [  morph flash  ]]
		ifErrorsParsing: [ :anError | morph showBalloon: anError messageText ]
]

{ #category : 'menu messages' }
SmalltalkEditor >> referencesToIt [
	"Open a references browser on the selected symbol: a variable name or class name"

	| selectedSymbol environment selectedString |
	self hasSelection ifFalse: [ self selectWord ].
	selectedSymbol := self selectedSymbol.

	"convenient access to class variables, including those in SharedPools"
	environment := self codeProvider selectedClassOrMetaClass ifNil: [ Smalltalk ].

	(selectedSymbol ifNotNil: [environment bindingOf: selectedSymbol]) ifNotNil: [ :reference |
		Smalltalk browseAllCallsOn: reference.
		^ self ].

	selectedString := self selectedString withoutSeparators.
	(environment ~= Smalltalk and: [ environment definesInstanceVariableNamedInHierarchy: selectedString ]) ifTrue: [
		Smalltalk browseAllAccessesTo: selectedString from: environment.
		^ self ].

	self referencesToSelectedLiteral.
]

{ #category : 'menu messages' }
SmalltalkEditor >> referencesToSelectedLiteral [
	"Evaluate the selected text and browse methods that reference the same literal"
	[
		self
			evaluateSelectionAndDo: [ :result |
				Smalltalk
					browseMessageList: (Smalltalk allReferencesToLiteral: result)
					name: 'Users of literal: ' , result asString
					autoHighlight: self selection
					allOccurrences: true ]
			ifFail: nil
			profiled: false ]
	on: UndeclaredVariableReference , UnknownSelector
	do: [ :ex |
		morph flash ]
]

{ #category : 'contextual rename' }
SmalltalkEditor >> rename: aNodeUnderCursor in: aSelectedClass at: aMethodNode [

	aNodeUnderCursor isTempOrArg ifTrue: [ ^self renameTemporary: aNodeUnderCursor at: aMethodNode ].

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		aNodeUnderCursor isMessageNode ifTrue: [
			^ self renameSelectorOf: aNodeUnderCursor in: aSelectedClass at: aMethodNode selector ].
		aNodeUnderCursor isInstanceVariableNode ifTrue: [
			^ self renameInstanceVariableOn: self codeProvider for: aNodeUnderCursor name at: aSelectedClass ].
		aNodeUnderCursor isLiteralVariableNode ifTrue: [ | variableName |
			variableName := aNodeUnderCursor key key.
			(Smalltalk classNamed: variableName) ifNotNil: [ :classToRename |
				^self renameClassOn: self codeProvider for: classToRename ].
			(Smalltalk bindingOf: variableName) ifNotNil: [
				^self renameGlobalOn: self codeProvider for: variableName ] ].

		^morph flash ]
]

{ #category : 'contextual rename' }
SmalltalkEditor >> renameClassOn: aBrowser for: aClassToRefactor [

	(RenameClassApplier for: aClassToRefactor) value
]

{ #category : 'contextual rename' }
SmalltalkEditor >> renameGlobalOn: aBrowser for: anOldName [

	(RenameGlobalApplier on: aBrowser for: anOldName) value
]

{ #category : 'contextual rename' }
SmalltalkEditor >> renameInstanceVariableOn: aBrowser for: anInstanceVariableName at: aClassToRefactor [

	(RenameInstanceVariableApplier on: aBrowser for: anInstanceVariableName at: aClassToRefactor) value
]

{ #category : 'contextual rename' }
SmalltalkEditor >> renameSelectorFor: aSelector in: aClassToRefactor [

	RefactoringApplier renameSelectorApplier createAndValueHandlingExceptionsOn: model textProvider for: aSelector in: aClassToRefactor 
]

{ #category : 'contextual rename' }
SmalltalkEditor >> renameSelectorOf: aMessageNode in: aSelectedClass at: aSelectedSelector [

	RefactoringApplier renameSelectorApplier
		on: aMessageNode
		createAndValueHandlingExceptionsOn: model textProvider
		in: aSelectedClass
		at: aSelectedSelector
]

{ #category : 'contextual rename' }
SmalltalkEditor >> renameTemporary: aTemporaryNode at: aMethodNode [

	self codeProvider isEditingMethod ifTrue: [ | applier |
		[ applier := RenameTemporaryApplier on: self for: aTemporaryNode at: aMethodNode ]
			on: SyntaxErrorNotification
			do: [:anError | ^self inform: (RenameTemporaryApplier errorMessageForCanNotParseMethod: anError) ].
		applier value ].

]

{ #category : 'clipboard' }
SmalltalkEditor >> replaceUnicodeArrowsWithSmalltalkArrows [
	"Useful, for instance, to paste code from TheCuisBook.pdf.
	Some day we may change our minds, or maybe add a preference."

	^true
]

{ #category : 'running tests' }
SmalltalkEditor >> runAndDebuggIfNecessary: aPotencialTestMethod [

	aPotencialTestMethod isTestMethod ifTrue: [
		aPotencialTestMethod methodClass debug: aPotencialTestMethod selector ]
]

{ #category : 'running tests' }
SmalltalkEditor >> runTestSuite: aTestSuite [

	(ProgressiveTestRunner for: aTestSuite) value
	
	
]

{ #category : 'do-its' }
SmalltalkEditor >> selectForInspection: aNodeUnderCursor in: aMethodNode  [
	
	(aNodeUnderCursor isLiteralNode or: [ aNodeUnderCursor isVariableNode ]) ifTrue: [ ^self selectNodeRange: aNodeUnderCursor in: aMethodNode ].
	aNodeUnderCursor isMessageNode ifTrue: [ ^self selectMessageNode: aNodeUnderCursor in: aMethodNode ].
]

{ #category : 'do-its' }
SmalltalkEditor >> selectMessageNode: aMessageNodeUnderCursor in: aMethodNode [
	
	| messageRange |
	
	self 
		withReceiverRangeOf: aMessageNodeUnderCursor 
		in: aMethodNode 
		selectorPosition: self startIndex 
		do: [ :receiverRange |
			messageRange := aMethodNode rangeForNode: aMessageNodeUnderCursor ifAbsent: [ ^ self ].
			self selectFrom: receiverRange first to: messageRange last ]
		
	
]

{ #category : 'do-its' }
SmalltalkEditor >> selectNodeRange: aNodeUnderCursor in: aMethodNode  [
	
	| range ranges |
	
	ranges := aMethodNode rangeForNode: aNodeUnderCursor ifAbsent: [ ^ self ].
	range := (aMethodNode isMultipleRanges: ranges) 
		ifTrue: [ ranges detect: [ :aRange | aRange includes: self startIndex ] ifNone: [ ^self ]]
		ifFalse: [ ranges ].
		
	self selectFrom: range first to: range last

]

{ #category : 'do-its' }
SmalltalkEditor >> selectNodeUnderCursorForInspectionIn: aMethodNode  [
	
	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self selectForInspection: nodeUnderCursor in: aMethodNode ]
		ifAbsent: nil
]

{ #category : 'new selection' }
SmalltalkEditor >> selectPrecedingIdentifier [
	"Invisibly select the identifier that ends at the end of the selection, if any."

	| string sep stop tok |
	tok := false.
	string := self privateCurrentString.
	stop := self stopIndex - 1.
	[stop > 0 and: [(string at: stop) isSeparator]] whileTrue: [stop := stop - 1].
	sep := stop.
	[sep > 0 and: [(string at: sep) tokenish]] whileTrue: [tok := true. sep := sep - 1].
	tok ifTrue: [self selectInvisiblyFrom: sep + 1 to: stop]
]

{ #category : 'accessing' }
SmalltalkEditor >> selectedClass [
	^ self codeProvider selectedClass
]

{ #category : 'accessing' }
SmalltalkEditor >> selectedMethod [
	^self codeProvider isEditingExistingMethod ifTrue: [
		self codeProvider currentCompiledMethod asMethodReference ].
]

{ #category : 'menu messages' }
SmalltalkEditor >> selectedSelector [
	"Try to make a selector out of the current text selection"

	^ self selectedString findSelector
]

{ #category : 'menu messages' }
SmalltalkEditor >> selectedSymbol [
	"Return the currently selected symbol, or nil if none.  Spaces, tabs and returns are ignored"

	| aString |
	self hasSelection ifFalse: [^ nil].
	aString := self selectedString withoutSeparators.
	aString isEmpty ifTrue: [^ nil].
	Symbol hasInterned: aString  ifTrue: [:sym | ^ sym].

	^ nil
]

{ #category : 'menu messages' }
SmalltalkEditor >> sendersOfIt [
	"Open a senders browser.
	If text selection defines a selector, take it. Otherwise, try finding selector under cursor. If this fails, consider the whole line."

	self selectedSelector ifNotNil: [ :selector |
		^ Smalltalk browseAllCallsOn: selector ].
	self
		withSelectorUnderCursorDo: [ :selector | Smalltalk browseAllCallsOn: selector ]
		ifBehavior: [:behavior | Smalltalk browseAllCallsOn: (Smalltalk bindingOf: behavior name)] 
		otherwise: [ self sendersOfItWhenErrorsParsing ]
]

{ #category : 'menu messages' }
SmalltalkEditor >> sendersOfItWhenErrorsParsing [

	"Open a senders browser on the selected selector"

	| aSelector |

	self lineSelectAndEmptyCheck: [^ self].
	(aSelector := self selectedSelector) ifNil: [^ morph flash].
	Smalltalk browseAllCallsOn: aSelector
]

{ #category : 'temp to inst var' }
SmalltalkEditor >> temporaryToInstanceVariable [

	self
		withNodeUnderCursorDo: [ :nodeUnderCursor |
			(nodeUnderCursor isTemp and: [nodeUnderCursor isArg not])
				ifTrue: [ (TemporaryToInstanceVariableApplier on: self for: nodeUnderCursor name) value ]
				ifFalse: [ morph flash ]]
		ifAbsent: [ morph flash ].
]

{ #category : 'running tests' }
SmalltalkEditor >> testSuiteForCategoryOf: aClass [

	| selectedSystemCategory |
	
	"If we are in a senders or implementors window, a MessageSet, the selectedSystemCategory is nil.
	In that case most of those cases the user wants to run the tests of the category of the selected method class - Hernan"
	selectedSystemCategory := self codeProvider selectedSystemCategory ifNil: [ aClass category ].
	^TestSuite forSystemCategoryTreeOf: selectedSystemCategory using: SystemOrganization 

]

{ #category : 'running tests' }
SmalltalkEditor >> testSuiteOf: aClass [

	^TestSuite forClass: aClass

]

{ #category : 'menu messages' }
SmalltalkEditor >> toggleBreakOnEntry [
	"Install or uninstall a halt-on-entry breakpoint on our method."

	self codeProvider toggleBreakOnEntry
]

{ #category : 'contextual rename' }
SmalltalkEditor >> withClassDefinitionNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock [

	| selectedClass methodNode |

	selectedClass := self codeProvider selectedClassOrMetaClass.
	methodNode := [ selectedClass methodNodeFor: model actualContents string noPattern: true ]
		on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.
]

{ #category : 'contextual rename' }
SmalltalkEditor >> withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock [

	| selectedClass methodNode |

	selectedClass := self codeProvider selectedClassOrMetaClass.
	[ 
		[ methodNode := selectedClass methodNodeFor: model actualContents string ]
			on: UndeclaredVariableWarning do: [ :ex | ex resume ]
	] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.
]

{ #category : 'temp to inst var' }
SmalltalkEditor >> withNodeUnderCursorDo: aDoBlock ifAbsent: anAbsentBlock [

	self 
		withMethodNodeAndClassDo: [ :currentMethodNode :currentClass |
			currentMethodNode 
				withParseNodeIncluding: self startIndex
				do: aDoBlock
				ifAbsent: anAbsentBlock ] 
		ifErrorsParsing: [ :arg1 | anAbsentBlock value ].
]

{ #category : 'do-its' }
SmalltalkEditor >> withReceiverRangeOf: aMessageNode in: aMethodNode selectorPosition: aSelectorPosition do: aBlock    [
	
	| receiverRange receiverRangeOrRanges messageNodeReceiver |
	
	"If aMessageNode receiver isNil it means that it is a cascade receiver so this imposes the question on how to inspect
	a cascade message send. We could inspect the result of sending all the messages up to the cursor but the problem is
	that when looking for the cascade receiver range it does not find it because it is a different node that the used in the source
	ranges... we could do the trick of looking for printString in the sourceRanges keys, but that is too much - Hernan"
	aMessageNode isCascade ifFalse: [ 
		messageNodeReceiver := aMessageNode receiver.
		messageNodeReceiver isMessageNode ifTrue: [ 
			^self withReceiverRangeOf: messageNodeReceiver in: aMethodNode selectorPosition: (messageNodeReceiver keywordPositionAt: 1) first do: aBlock ].
		
		receiverRangeOrRanges := aMethodNode rangeForNode: messageNodeReceiver ifAbsent: [ ^ self ].
		
		receiverRange := (aMethodNode isMultipleRanges: receiverRangeOrRanges)
			ifTrue: [ | closestRange |
				closestRange := receiverRangeOrRanges first.
				receiverRangeOrRanges do: [ :aRange |  (aRange last < aSelectorPosition and: [ aRange last > closestRange last ]) ifTrue: [ closestRange := aRange ]].
				closestRange ]
			ifFalse: [ receiverRangeOrRanges ].

		aBlock value: receiverRange ]
]

{ #category : 'menu messages' }
SmalltalkEditor >> withSelectorAt: aPosition do: aBlock ifBehavior: aBehaviorBlock otherwise: failBlock [
	| methodNode nodeAtRange nodeUnderCursor  |

	methodNode := self codeProvider
		methodNodeOf: model actualContents string
		ifErrorsParsing: [ :anError | ^ failBlock value ].

	aPosition < methodNode selectorLastPosition ifTrue: [ ^aBlock value: methodNode selector ].
	nodeAtRange := methodNode parseNodeIncluding: aPosition ifAbsent: [ ^ failBlock value ].
	nodeUnderCursor := nodeAtRange key.
	nodeUnderCursor isMessageNode ifTrue: [ ^aBlock value: nodeAtRange key selector key ].
	(nodeUnderCursor isLiteralNode and: [ nodeUnderCursor literalValue isSymbol ])
		ifTrue: [ ^aBlock value: nodeUnderCursor literalValue ].
	(nodeUnderCursor isLiteralVariableNode and: [ nodeUnderCursor isReferencingBehavior ])
		ifTrue: [ ^aBehaviorBlock value: nodeUnderCursor key value ].

	^ failBlock value.
]

{ #category : 'menu messages' }
SmalltalkEditor >> withSelectorUnderCursorDo: aBlock ifBehavior: aBehaviorBlock otherwise: failBlock [

	self withSelectorAt: self startIndex do: aBlock ifBehavior: aBehaviorBlock otherwise: failBlock
]
