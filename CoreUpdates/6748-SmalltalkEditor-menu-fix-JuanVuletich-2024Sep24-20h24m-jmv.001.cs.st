'From Cuis7.1 [latest update: #6745] on 24 September 2024 at 8:34:52 pm'!

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 20:24:21'!
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
				ifTrue: [ #(270				'Accept (s)' 							$s 	editor 			acceptContents 						acceptIcon 							'Save') ]
				ifFalse: [ #(270 				'Accept' 							nil 	editor 			nil 						acceptIcon 							'Save') ].
			methodEntries := #(
				#(20 		'-----')
				#(30		'What to Show' 									nil 	nil 			3000 						preferencesIcon 							'Select alternative views of the method')
				#(40		'Toggle Break' 									nil 	model 			toggleBreakOnEntry 						debugIcon 							'Add o remove a Breakpoint')
				#(200 		'-----')
				#(210		'Refactorings' 									nil 	nil 			2000 						listAddIcon)
				#(280		'Accept & Run Test in Class (t)' 									$t 	editor 			acceptAndTest 						weatherFewCloudsIcon 							'Saves method, runs it as test and then all tests in class. Opens debugger if error.')
				#(290		'Accept & Run Test in Category (y)' 									$y 	editor 			acceptAndTestAll 						weatherFewCloudsIcon 							'Saves method, runs it as test and then all tests in class category. Opens debugger if error.')
				#(300		'Accept & Debug Test (r)' 									$r 	editor 			acceptAndDebugTest 						weatherFewCloudsIcon 							'Saves method and debugs it')
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
				#(170		'Senders of it (n)' 							$n 	editor 			sendersOfIt 						mailForwardIcon 							'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(180		'Implementors of it (m)' 							$m 	editor 			implementorsOfIt 						developmentIcon 							'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(190		'References to it (N)' 							$N 	editor 			referencesToIt 						addressBookIcon 							'References to it (selection is a class name, or cursor is over a class-list or message-list)')
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

