'From Cuis7.1 [latest update: #6837] on 22 November 2024 at 10:32:34 am'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'jfr 8/19/2024 09:44:30'!
allMethodsWithSourceString: aString
matchCase: caseSensitive
scopeClass: scopeClass
includeSubclasses: includeSubclasses
includeSuperclasses: includeSuperclasses
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.  Search the class comments also"
	| list classCount adder classesToSearch |
	list := Set new.
	adder := [ :mrClass :mrSel |
		list add: (MethodReference class: mrClass selector: mrSel) ].
	
	"Determine classes to search"
	scopeClass
		ifNil:[classesToSearch := self allClasses]
		ifNotNil:[
			classesToSearch := Set new.
			classesToSearch add: scopeClass.
			(includeSuperclasses)
				ifTrue:[scopeClass allSuperclassesDo: [:aClass | classesToSearch add: aClass]].
			(includeSubclasses)
				ifTrue:[scopeClass allSubclassesDo: [:aClass | classesToSearch add: aClass]].
			classesToSearch := classesToSearch asArray].
	
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: classesToSearch size
		during: [ :aBarBlock |
			classCount := 0.
			classesToSearch do: [ :aClass |
				aBarBlock value: (classCount := classCount + 1).
				(Array
					with: aClass
					with: aClass class) do: [ :aClassOrMetaClass |
					aClassOrMetaClass selectorsDo: [ :aSelector |
						((aClassOrMetaClass sourceCodeAt: aSelector)
							findString: aString
							startingAt: 1
							caseSensitive: caseSensitive) > 0 ifTrue: [
							adder
								value: aClassOrMetaClass
								value: aSelector ]].
					(aClassOrMetaClass organization classComment
						findString: aString
						startingAt: 1
						caseSensitive: caseSensitive) > 0 ifTrue: [
						adder
							value: aClassOrMetaClass
							value: #Comment ]]]].
	^ list asArray sort.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jfr 8/19/2024 09:47:47'!
browseMethodsWithSourceString: aString
scopeClass: scopeClass
includeSubclasses: includeSubclasses
includeSuperclasses: includeSuperclasses
	"Smalltalk browseMethodsWithSourceString: 'SourceString' "
	"Launch a browser on all methods whose source code contains aString as a substring."

	| caseSensitive suffix selectString |
	Sensor controlKeyPressed ifTrue: [
		selectString := ((aString findTokens: '*') sorted: [ :a :b | a size > b size ]) first.
		^ self 
			browseMessageList: (self allMethodsSourceStringMatching: '*', aString, '*')
			name: 'Methods matched by ' , aString printString , ' (no ctrl for exact match)'
			autoHighlight: selectString
			allOccurrences: true
		].
	suffix := (caseSensitive := Sensor shiftPressed)
		ifTrue: [' (case-sensitive. Use ctrl for $* pattern match.)']
		ifFalse: [' (shift for case-sensitive. ctrl for $* pattern match.)'].
	^ self 
		browseMessageList: (self allMethodsWithSourceString: aString matchCase: caseSensitive scopeClass: scopeClass includeSubclasses: includeSubclasses includeSuperclasses: includeSuperclasses)
		name: 'Methods containing ' , aString printString , suffix
		autoHighlight: aString
		allOccurrences: true.! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jfr 8/19/2024 14:14:50'!
findMethodSourceContainingIt
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source).  Slow!!"
	| searchString selection scopeClass scopeClassName includeSubclasses includeSuperclasses labelList |
	
	searchString := StringRequestMorph
		request: 'Enter search text:'
		initialAnswer: self selectedString
		orCancel: [^self].
	
	scopeClass := self codeProvider selectedClass.
	scopeClassName := scopeClass name asString.
	includeSubclasses := false.
	includeSuperclasses := false.

	labelList := {
		'This method'. 
		'All methods in image'. } asOrderedCollection.
	(scopeClass notNil)
		ifTrue:[
			labelList  addAll: {
				'Class ', scopeClassName printString.
				'Class ', scopeClassName printString, ' and subclasses'.
				'Class ', scopeClassName printString, ', subclasses, and superclasses'.}].
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
	
	Smalltalk browseMethodsWithSourceString: searchString scopeClass: scopeClass includeSubclasses: includeSubclasses includeSuperclasses: includeSuperclasses! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 10:28:06'!
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
				ifTrue: [ #(270	'Save (s)' 				$s 	editor 	acceptContents 		acceptIcon 				'Save') ]
				ifFalse: [ #(270 	'Save' 					nil 	editor 	nil 						acceptIcon 				'Save') ].
			methodEntries := #(
				#(20 	'-----')
				#(30	'What to Show' 					nil 	nil 		3000 					preferencesIcon 		'Select alternative views of the method')
				#(40	'Toggle Break' 					nil 	editor 	toggleBreakOnEntry 	debugIcon 				'Add or remove a Breakpoint')
				#(200 	'-----')
				#(210	'Refactorings' 					nil 	nil 		2000 					listAddIcon)
				#(280	'Save & Run Test in Class (t)' 		$t 	editor 	acceptAndTest 			weatherFewCloudsIcon 'Saves method, runs it as test and then all tests in class. Opens debugger if error.')
				#(290	'Save & Run Test in Category (y)' 	$y 	editor 	acceptAndTestAll 		weatherFewCloudsIcon 'Saves method, runs it as test and then all tests in class category. Opens debugger if error.')
				#(300	'Save & Debug Test (r)' 			$r 	editor 	acceptAndDebugTest 	weatherFewCloudsIcon 'Saves method and debugs it')
			) ]
		ifFalse: [
			acceptEntry := self hasUnacceptedEdits
				ifTrue: [ #(270	'Save to file (s)' 			$s 	editor 	acceptContents 		acceptIcon 				'Save contents to a file') ]
				ifFalse: [ #(270 	'Save to file' 				nil 	editor 	nil 						acceptIcon 				'Save contents to a file') ]].
	^Array streamContents: [ :strm |
		strm nextPutAll:
			#(
				#(0 		'Smalltalk Editor' 				true)
				#(10		'Help... (h)' 					$h 	editor 	openHelp 				helpIcon 				'Open Help')
				#(50 		'-----')
				#(60		'Do it (d)' 					$d 	editor 	doIt 					doItIcon 				'Do it (selection is a valid expression)')
				#(70		'Print it (p)' 					$p 	editor 	printIt 					printIcon 				'Print it (selection is a valid expression)')
				#(80		'Inspect it (i)' 				$i 	editor 	inspectIt 				inspectIcon 			'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
				#(90		'Explore it (I)' 				$I 	editor 	exploreIt 				exploreIcon			'Inspect via Object Explorer')
				#(100		'Debug it (D)' 				$D 	editor 	debugIt 				debugIcon				'Debug it')
				#(110		'Profile it' 					nil 	editor 	profileIt 				clockIcon)
				#(120 		'-----')
				#(130		'Edit' 						nil 	nil 		1000 					textEditorIcon)
				#(140 		'-----')
				#(150		'Explain' 					nil 	editor 	explain 				helpIcon)
				#(160		'Browse it (b)' 				$b 	editor 	browseIt 				editFindReplaceIcon 	'Browse it (selection is a class name or cursor is over a class-list or message-list)')
				#(170		'Senders (n)' 				$n 	editor 	sendersOfIt 			mailForwardIcon 		'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(180		'Implementors (m)' 			$m 	editor 	implementorsOfIt 		developmentIcon 		'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(190		'References (N)' 				$N 	editor 	referencesToIt 			addressBookIcon 		'References to it (selection is a class name, or cursor is over a class-list or message-list)')
				#(220 		'-----')
				#(230		'Find with Scope (e)' 			$e 	editor 	findMethodSourceContainingIt scriptIcon 				'Find with scope') "Used to be #methodSourceContainingIt"
				#(240		'Method Strings with it (E)' 	$E	editor 	methodStringsContainingIt 	genericTextIcon 		'Method strings containing it')
				#(250		'Find in class comments' 		nil 	editor 	classCommentsContainingIt 	chatIcon)
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

