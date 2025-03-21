'From Cuis7.3 [latest update: #7084] on 20 March 2025 at 9:29:44 pm'!

!TestSuite class methodsFor: 'instance creation' stamp: 'HAW 3/20/2025 21:08:51'!
forSystemCategoryTreeOf: aCategoryName  using: aSystemOrganizer

	| rootCategory |

	rootCategory := (aCategoryName findTokens: '-') first.
	
	^self forSystemCategoryNamed: rootCategory using: aSystemOrganizer ! !


!SmalltalkEditor methodsFor: 'running tests' stamp: 'HAW 3/20/2025 21:14:55'!
testSuiteForCategoryOf: aClass

	| selectedSystemCategory |
	
	"If we are in a senders or implementors window, a MessageSet, the selectedSystemCategory is nil.
	In that case most of those cases the user wants to run the tests of the category of the selected method class - Hernan"
	selectedSystemCategory := self codeProvider selectedSystemCategory ifNil: [ aClass category ].
	^TestSuite forSystemCategoryTreeOf: selectedSystemCategory using: SystemOrganization 
! !

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'HAW 3/20/2025 21:12:44'!
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
	].! !

