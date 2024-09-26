'From Cuis7.1 [latest update: #6749] on 26 September 2024 at 10:06:45 am'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 9/26/2024 09:40:15'!
toggleBreakOnEntry
	"Install or uninstall a halt-on-entry breakpoint on our method."

	self codeProvider toggleBreakOnEntry! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/26/2024 09:38:01'!
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
				#(40		'Toggle Break' 									nil 	editor 			toggleBreakOnEntry 						debugIcon 							'Add o remove a Breakpoint')
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


!BreakingMethodWrapper methodsFor: 'system primitives' stamp: 'jmv 9/26/2024 10:06:29'!
run: aSelector with: arguments in: aReceiver
	"I am sent by the virtual machine, if an instance of a class other
	than CompiledMethod is bound in a method dictionary.

	See SystemDictionary>>#recreateSpecialObjectsArray at index 50."


	"Get rid of that break point, we don't want debuggers popping up while debugging."
	self uninstall.

	"Halt so users gets their debugger."
	Smalltalk isDevelopmentEnvironmentPresent ifTrue: [
		Halt signal ].

	"Evaluate our method. Maybe in the debugger or as usual if user proceeded.
	Answer result."
	^aReceiver withArgs: arguments executeMethod: method! !

