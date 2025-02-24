'From Cuis7.3 [latest update: #7047] on 24 February 2025 at 1:20:07 pm'!

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 2/24/2025 13:16:29'!
classDefRefactoringsSubmenuSpec
	" 'Refactorings' SubMenu"

	^#(
		#(2010 	'-----')
		#(2020	'Rename... (R)' 						$R 	editor 	contextualRenameInClassDefinition 	saveAsIcon 	'Renames what is under cursor')
		#(2030 	'-----')
		#(2040	'Push Up Instance Variable' 			nil 	editor 	contextualPushUpInClassDefinition 	goTopIcon)
		#(2050	'Push Down Instance Variable' 		nil 	editor 	contextualPushDownInClassDefinition 	goBottomIcon)
	).! !

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 2/24/2025 13:09:49'!
methodRefactoringsSubmenuSpec
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
	).! !


!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'jmv 2/24/2025 13:05:41'!
contextualChangeSelectorUsing: aChangeSelectorApplier
	
	self ifSourceCodeRefactoringCanBeAppliedDo: [ self contextualChangeSelectorInMethodUsing: aChangeSelectorApplier ]! !

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'jmv 2/24/2025 13:05:59'!
contextualExtractAsParameter

	self ifSourceCodeRefactoringCanBeAppliedDo: [ self extractAsParameter ]! !

!SmalltalkEditor methodsFor: 'contextual push up/down inst. var' stamp: 'jmv 2/24/2025 13:13:07'!
contextualPushDownInClassDefinition 

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		self
			withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | 
				self apply: [ :aSelectedClass | 
					(PushDownInstanceVariableApplier 
						on: self codeProvider 
						for: self wordUnderCursor 
						at: aSelectedClass ) value ] inClassDefinitionOf: classDefinitionNode in: selectedClass ]
			ifErrorsParsing: [ :anError | morph flash ]]! !

!SmalltalkEditor methodsFor: 'contextual push up/down inst. var' stamp: 'jmv 2/24/2025 13:13:57'!
contextualPushUpInClassDefinition 

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		self
			withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | 
				self apply: [ :aSelectedClass | 
					(PushUpInstanceVariableApplier 
						on: self codeProvider 
						for: self wordUnderCursor 
						at: aSelectedClass) value ] inClassDefinitionOf: classDefinitionNode in: selectedClass ]
			ifErrorsParsing: [ :anError | morph flash ]]! !

!SmalltalkEditor methodsFor: 'inline method' stamp: 'jmv 2/24/2025 13:06:16'!
contextualInlineMethod

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		self
			withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualInlineMethodOf: methodNode in: selectedClass ]
			ifErrorsParsing: [ :anError | morph flash ]	]	! !

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 2/24/2025 13:17:48'!
defaultMenuSpec
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
					#(290	'Save & Run Test in Category (y)' 	$y 	editor 	acceptAndTestAll 		weatherFewCloudsIcon 'Saves method, runs it as test and then all tests in class category. Opens debugger if error.')
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
	].! !

!methodRemoval: SmalltalkEditor #refactoringsSubmenuSpec stamp: 'jmv 2/24/2025 13:19:49'!
SmalltalkEditor removeSelector: #refactoringsSubmenuSpec!
!methodRemoval: SmalltalkEditor #contextualRename stamp: 'jmv 2/24/2025 13:19:49'!
SmalltalkEditor removeSelector: #contextualRename!
!methodRemoval: SmalltalkEditor #ifEditingClassDefinitionDoOrWarn: stamp: 'jmv 2/24/2025 13:19:49'!
SmalltalkEditor removeSelector: #ifEditingClassDefinitionDoOrWarn:!
!methodRemoval: SmalltalkEditor #inClassDefinitionContextuallyApply: stamp: 'jmv 2/24/2025 13:19:49'!
SmalltalkEditor removeSelector: #inClassDefinitionContextuallyApply:!
!methodRemoval: SmalltalkEditor #informRefactoringCanOnlyBeAppliedInClassDefinition stamp: 'jmv 2/24/2025 13:19:49'!
SmalltalkEditor removeSelector: #informRefactoringCanOnlyBeAppliedInClassDefinition!
