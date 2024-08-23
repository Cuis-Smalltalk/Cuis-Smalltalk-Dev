'From Cuis7.1 [latest update: #6625] on 23 August 2024 at 6:31:20 pm'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 8/23/2024 17:57:19'!
allImplementorsOf: aSelector subTo: aClass
	"Answer a sorted Collection of all the methods that implement the message
	aSelector in, or below the given class."

	| aSet cls |
	aSet := Set new.
	cls := aClass theNonMetaClass.
	cls withAllSubclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	cls class withAllSubclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	^aSet asArray! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 8/23/2024 17:57:08'!
allImplementorsOf: aSelector superTo: aClass
	"Answer a sorted Collection of all the methods that implement the message
	aSelector in, or above the given class."

	| aSet cls |
	aSet := Set new.
	cls := aClass theNonMetaClass.
	cls withAllSuperclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	cls class withAllSuperclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	^aSet asArray! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/23/2024 18:09:01'!
browseAllImplementorsOf: selector subTo: aClass
	"Create and schedule a message browser on each method in or below the given class
	that implements the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put: localTo: Dictionary."

	aClass ifNil: [ ^self inform: 'no class selected' ].
	^self
		browseMessageList: (self allImplementorsOf: selector subTo: aClass)
		name: 'Implementors of ' , selector, ' in ', aClass name, ' and subclasses'! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/23/2024 18:08:53'!
browseAllImplementorsOf: selector superTo: aClass
	"Create and schedule a message browser on each method in or above the given class
	that implements the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put: localTo: Dictionary."

	aClass ifNil: [ ^self inform: 'no class selected' ].
	^self
		browseMessageList: (self allImplementorsOf: selector superTo: aClass)
		name: 'Implementors of ' , selector, ' in ', aClass name, ' and superclasses'! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/23/2024 16:48:47'!
browseImplementorsOfMessages
	"Present a menu of all messages sent by the currently selected message. 
	Open a message set browser of all implementors of the message chosen."

	self getSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/23/2024 17:59:00'!
browseLocalImplementorsOfMessages
	"Present a menu of all messages sent by the currently selected message. 
	Open a message set browser of all implementors of the message chosen in, above, or below
	the selected class.
	Do nothing if no message is chosen."
	self getSelectorAndSendQuery: #browseAllImplementorsOf:localTo:
		to: Smalltalk
		with: { model selectedClass }! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/23/2024 17:08:17'!
browseLocalSenders
	"Browse the senders of the selected method."
	
	model selectedMessageName ifNotNil: [ :selector |
		Smalltalk browseAllCallsOn: selector localTo: model selectedClass  ]! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/23/2024 18:02:07'!
browseSubImplementors
	"Browse the senders of the selected method."
	
	model selectedMessageName ifNotNil: [ :selector |
		Smalltalk browseAllImplementorsOf: selector subTo: model selectedClass  ]! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/23/2024 18:02:16'!
browseSuperImplementors
	"Browse the senders of the selected method."
	
	model selectedMessageName ifNotNil: [ :selector |
		Smalltalk browseAllImplementorsOf: selector superTo: model selectedClass  ]! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 8/23/2024 17:56:48'!
allImplementorsOf: aSelector localTo: aClass
	"Answer a sorted Collection of all the methods that implement the message
	aSelector in, above, or below the given class."

	| aSet cls |
	aSet := Set new.
	cls := aClass theNonMetaClass.
	cls withAllSuperAndSubclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	cls class withAllSuperAndSubclassesDo: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [ aSet add: (MethodReference class: class selector: aSelector) ] ].
	^aSet asArray! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/23/2024 16:45:51'!
browseAllCallsOn: aLiteral localTo: aClass
	"Create and schedule a message browser on each method in or below the given class that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	aClass ifNil: [ ^ self inform: 'no selected class' ].
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral)
				name: 'Users of ' , aLiteral key , ' in the full hierarchy of ' , aClass name
				autoHighlight: aLiteral key
				allOccurrences: true ]
		ifFalse: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral)
				name: 'Senders of ' , aLiteral , ' in the full hierarchy of ' , aClass name
				autoHighlight: aLiteral
				allOccurrences: true ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/23/2024 17:58:01'!
browseAllImplementorsOf: selector localTo: aClass
	"Create and schedule a message browser on each method in, above, or below the given class
	that implements the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put: localTo: Dictionary."

	aClass ifNil: [ ^self inform: 'no class selected' ].
	^self
		browseMessageList: (self allImplementorsOf: selector localTo: aClass)
		name: 'Implementors of ' , selector, ' in the full hierarchy of ', aClass name! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 8/23/2024 18:28:27'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList textConstructor |
	textConstructor := [ :string :backgroundColor | 
		string asText addAttribute: (TextBackgroundColor color: backgroundColor) ].

	aList := #(
		(10	'browse'				browseMethodFull									'view this method in a browser'								browseMethodFullAndReplaceMe)
		(11	'senders' 				browseSendersOfMessages									'browse senders of...'								browseSenders)
		(16	'implementors'				browseImplementorsOfMessages									'browse implementors of...' 								browseImplementors)
		(12	'versions'				browseVersions									'browse versions')), 

		((Preferences at: #decorateBrowserButtons)
			ifTrue: [
				{{13	. 'inheritance'.	 #methodInheritance. 	'Browse Method Inheritance

', (textConstructor value: 'green' value: `Color green muchLighter`),': sends to super
', (textConstructor value: 'tan' value: `Color tan`), ': has override(s)
', (textConstructor value: 'mauve' value: `Color blue muchLighter`), ': both of the above
', (textConstructor value: 'pink' value: `Color red muchLighter`), ': is an override but doesn''t call super
', (textConstructor value: 'pinkish tan' value: `Color r: 0.94 g: 0.823 b: 0.673`), ': has override(s), also is an override but doesn''t call super
'. #methodInheritanceAndReplaceMe }}]
			ifFalse: [
				{#(13	'inheritance'		 methodInheritance	 'browse method inheritance'. #methodInheritanceAndReplaceMe)}]),

		#(
		(12	'hierarchy'				browseHierarchy									'browse class hierarchy'								browseHierarchyAndReplaceMe)
		(10	'inst vars'				browseInstVarRefs									'inst var refs...')
		(11	'class vars'				browseClassVarRefs									'class var refs...')
		(10	'show...'				offerWhatToShowMenu									'menu of what to show in lower pane')).

	^ aList! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/23/2024 17:03:33'!
browseSenders
	"Browse the senders of the selected method."
	
	model selectedMessageName ifNotNil: [ :selector |
		Smalltalk browseAllCallsOn: selector ]! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/23/2024 18:11:36'!
methodInheritance
	"Create and schedule a method browser on the inheritance of implementors."

	| list aClassNonMeta isMeta theClassOrMeta aClass sel |
	aClass := model selectedClassOrMetaClass.
	sel := model selectedMessageName.
	aClass ifNil: [ ^ self ].
	sel ifNil: [ ^ self ].
	aClassNonMeta := aClass theNonMetaClass.
	isMeta := aClassNonMeta ~~ aClass.
	list := OrderedCollection new.
	aClass allSuperclasses reverseDo: [ :cl |
		(cl includesSelector: sel) ifTrue: [
			list addLast: (MethodReference class: cl selector: sel) ]].
	aClassNonMeta
		allSubclassesWithLevelDo: [ :cl :level |
			theClassOrMeta := isMeta
				ifTrue: [ cl class ]
				ifFalse: [ cl ].
			(theClassOrMeta includesSelector: sel) ifTrue: [
				list addLast: (MethodReference class: theClassOrMeta selector: sel) ]]
		startingLevel: 0.
	^ Smalltalk
		browseMessageList: list
		name: 'Implementors of ' , sel, ' in ', aClass name, ' and super / sub classes'.! !


!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 8/23/2024 17:36:49'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class List' 					false)
		#(10		'browse full (b)' 							$b 	window 			browseMethodFull 									editFindReplaceIcon)
		#(20		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 									goTopIcon)
		#(30		'browse protocol (p)' 							$p 	window 			browseFullProtocol 									spreadsheetIcon)
		#(40 		'-----')
		#(50		'fileOut' 							nil 	model 			fileOutClass 									fileOutIcon)
		#(60		'run tests (t)' 							$t 	model 			runClassTests 									weatherFewCloudsIcon)
		#(70 		'-----')
		#(80		'refactorings' 							nil 	nil 			1000 									listAddIcon)
		#(90		'copy class...' 							nil 	model 			copyClass 									copyIcon)
		#(100		'remove class (x)' 							$x 	model 			removeClass 									deleteIcon)
		#(110 		'-----')
		#(120		'show classes hierarchically' 							nil 	model 			enableListClassesHierarchically 									goTopIcon)
		#(130		'show classes alphabetically' 							nil 	model 			enableListClassesAlphabetically 									sendReceiveIcon)
		#(140		'show definition' 							nil 	model 			editClass 									editFindReplaceIcon)
		#(150		'show comment' 							nil 	model 			editComment 									editFindReplaceIcon)
		#(160 		'-----')
		#(170		'class refs (N)' 							$N 	window 			browseClassRefs 									classIcon)
		#(180 		'-----')
		#(190		'more' 							nil 	nil 			2000 									listAddIcon)

		"Shortcut only. No menu entry."
		#(991		nil 							$r 	model 			recent)

		" 'refactorings' SubMenu"
		#(1010		'rename class... (R)' 							$R 	model 			renameClass 									saveAsIcon)
		#(1020		'insert superclass...' 							nil 	model 			insertSuperclass 									saveAsIcon)
		#(1030 		'-----')
		#(1040		'add inst var...' 							nil 	model 			addInstVar 									listAddIcon)
		#(1050		'rename inst var...' 							nil 	model 			renameInstVar 									saveAsIcon)
		#(1060		'remove inst var...' 							nil 	model 			removeInstVar 									deleteIcon)
		#(1070		'remove all unreferenced inst var...' 						
											nil 	model 			removeAllUnreferencedInstVar 									deleteIcon)
		#(1080		'push up inst var...' 							nil 	model 			pushUpInstanceVariable 									goTopIcon)
		#(1090		'push down inst var...' 							nil 	model 			pushDownInstanceVariable 									goBottomIcon)
		#(1100 		'-----')
		#(1110		'rename global...' 							nil 	model 			renameGlobal 									saveAsIcon)

		" 'more' SubMenu"
		#(2010		'unsent methods' 							nil 	window 			browseUnusedMethods 									junkIcon
					'browse all methods defined by this class that have no senders')
		#(2020		'unreferenced inst vars' 							nil 	window 			showUnreferencedInstVars 									junkIcon
					'show a list of all instance variables that are not referenced in methods')
		#(2030		'unreferenced class vars' 							nil 	window 			showUnreferencedClassVars 									junkIcon
					'show a list of all class variables that are not referenced in methods')
		#(2040		'subclass template' 							nil 	model 			makeNewSubclass 									classIcon
					'put a template into the code pane for defining of a subclass of this class')
		#(2050 		'-----')
		#(2060		'sample instance' 							nil 	window 			makeSampleInstance 									instanceIcon
					'give me a sample instance of this class, if possible')
		#(2070		'inspect instances' 							nil 	window 			inspectInstances 									inspectIcon
					'open an inspector on all the extant instances of this class')
		#(2080		'inspect subinstances' 							nil 	window 			inspectSubInstances 									inspectIcon
					'open an inspector on all the extant instances of this class and of all of its subclasses')
		#(2090 		'-----')
		#(2100		'create inst var accessors' 							nil 	model 			createInstVarAccessors 									sendReceiveIcon
					'compile instance-variable access methods for any instance variables that do not yet have them')
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 8/23/2024 17:37:25'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message List' 					false)
		#(10		'what to show...' 							nil 	window 			offerWhatToShowMenu 									preferencesIcon)
		#(20		'toggle break on entry' 							nil 	model 			toggleBreakOnEntry 									debugIcon)
		#(30 		'-----')
		#(40		'browse full (b)' 							$b 	window 			browseMethodFull 									editFindReplaceIcon)
		#(50		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 									goTopIcon)
		#(60		'browse method (O)' 							$O 	window 			openSingleMessageBrowser 									scriptIcon)
		#(70 		'-----')
		#(80		'fileOut' 							nil 	model 			fileOutMessage 									fileOutIcon)
		#(90		'run test (t)' 							$t 	model 			runMethodTest 									weatherFewCloudsIcon)
		#(100		'debug test (r)' 							$r 	model 			debugMethodTest 									weatherFewCloudsIcon)
		#(110		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 									exploreIcon)
		#(120 		'-----')
		#(130		'refactorings' 							nil 	nil 			1000 									listAddIcon)
		#(140		'senders of... (n)' 							nil 	window 			browseSendersOfMessages 									mailForwardIcon)
		#(150		'implementors of... (m)' 							nil 	window 			browseImplementorsOfMessages 									developmentIcon)
		#(160		'inheritance (i)' 							$i 	window 			methodInheritance 									goDownIcon)
		#(170		'versions (v)' 							$v 	window 			browseVersions 									clockIcon)
		#(180 		'-----')
		#(190		'remove method (x)' 							$x 	model 			removeMessage 									deleteIcon)
		#(200		'remove method in hierarchy' 							nil 	model 			removeMessageInHierarchy 									deleteIcon)
		#(210		'more' 							nil 	nil 			2000 									listAddIcon)

		"Shortcut only. No menu entry."
		#(991		nil 							$m 	window 			browseImplementors)
		#(992		nil 							$n 	window 			browseSenders)
		#(993		nil 							$c 	model 			copySelector)

		" 'refactorings' SubMenu"
		#(1010		'rename... (R)' 											$R 	model 			renameSelector 											saveAsIcon)
		#(1020		'change keyword order...' 											nil 	model 			changeKeywordOrder 											sendReceiveIcon)
		#(1030		'add parameter... (A)' 											$A 	model 			addParameter 											listAddIcon)
		#(1040		'remove parameter... (S)' 											$S 	model 			removeParameter 											listRemoveIcon)
		#(1050 		'-----')
		#(1060		'inline method... (F)' 											$F 	model 			inlineMethod 											saveAsIcon)
		#(1070		'move to instance/class method' 											nil 	model 			moveToInstanceOrClassMethod 											changesIcon)
		#(1080		'push up' 											nil 	model 			pushUpSelector 											goTopIcon)
		#(1090		'add in superclass as subclassResponsability' 											nil 	model 			addAsSubclassResponsabilitySelector 											goTopIcon)
		#(1100		'push down to subclasses' 											nil 	model 			pushDownSelectorToSubclasses 											goBottomIcon)
		#(1110		'push down to one subclass' 											nil 	model 			pushDownSelectorToOneSubclass 											goBottomIcon)

		" 'more' SubMenu"
		#(2010		'toggle diffing (D)' 								$D 	model 			toggleDiffing 											switchIcon)
		#(2020 		'-----')
		#(2030		'local senders of...' 								nil 	window 			browseLocalSendersOfMessages 											mailForwardIcon)
		#(2040		'local implementors of...' 								nil 	window 			browseLocalImplementorsOfMessages 											classIcon)
		#(2050 		'-----')
		#(2060		'sample instance' 								nil 	window 			makeSampleInstance 											instanceIcon)
		#(2070		'inspect instances' 								nil 	window 			inspectInstances 											inspectIcon)
		#(2080		'inspect subinstances' 								nil 	window 			inspectSubInstances 											inspectIcon)
		#(2090 		'-----')
		#(2100		'change category...' 								nil 	model 			changeCategory 											saveAsIcon)
		#(2110		'select method category (C)' 								$C 	model 			showHomeCategory 											editFindReplaceIcon)
		#(2120		'change sets with this method' 								nil 	window 			findMethodInChangeSets 											changesIcon)
		#(2130		'revert to previous version' 								nil 	model 			revertToPreviousVersion 											undoIcon)
	)! !


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 8/23/2024 17:37:34'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Class List' 						false)
			#(10 		'delete class from change set (d)' 								$d 	model 			forgetClass 						warningIcon)
			#(20 		'remove class from system (x)' 								$x 	model 			removeClass 						deleteIcon)
			#(30 		'-----')
			#(40 		'browse full (b)' 								$b 	window 			browseMethodFull 						editFindReplaceIcon)
			#(50 		'browse hierarchy (h)' 								$h 	window 			browseHierarchy 						goTopIcon)
			#(60 		'browse protocol (p)' 								$p 	window 			browseFullProtocol 						spreadsheetIcon)
			#(70 		'-----')		
			#(80 		'class refs (N)' 								$N 	window 			browseClassRefs 						classIcon)
	).
! !

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 8/23/2024 16:54:08'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the message list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Message List' 						true)
			#(10 		'delete method from change set (d)' 									$d 	model 			forget 									warningIcon)
			#(20 		'remove method from system (x)' 									$x 	model 			removeMessage 									deleteIcon)
			#(30 		'-----')
			#(40 		'browse full (b)' 									$b 	window 			browseMethodFull 									editFindReplaceIcon)
			#(50 		'browse hierarchy (h)' 									$h 	window 			browseHierarchy 									goTopIcon)
			#(60 		'browse method (O)' 									$O 	window 			openSingleMessageBrowser 									scriptIcon)
			#(80 		'-----')		
			#(90 		'fileOut (o)' 									$o 	model 			fileOutMessage 									fileOutIcon)
			#(100 		'-----')		
			#(110		'senders of... (n)' 									nil 	window 			browseSendersOfMessages 									mailForwardIcon)
			#(120		'implementors of... (m)' 									nil 	window 			browseImplementorsOfMessages 									developmentIcon)
			#(130 		'inheritance (i)' 									$i 	window 			methodInheritance 									goDownIcon)
			#(140 		'versions (v)' 									$v 	window 			browseVersions 									clockIcon)

			"Shortcut only. No menu entry."
			#(991		nil 									$m 	window 			browseImplementors)
			#(992		nil 									$n 	window 			browseSenders)
	).
! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 8/23/2024 17:38:06'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Set' 					false)
		#(10		'what to show...' 							nil 	window 			offerWhatToShowMenu 											preferencesIcon)
		#(20		'toggle break on entry' 							nil 	model 			toggleBreakOnEntry 											debugIcon)
		#(30 		'-----')
		#(40		'browse full (b)' 							$b 	window 			browseMethodFull 											editFindReplaceIcon)
		#(50		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 											goTopIcon)
		#(60		'browse method (O)' 							$O 	window 			openSingleMessageBrowser 											scriptIcon)
		#(70		'browse protocol (p)' 							$p 	window 			browseFullProtocol 											spreadsheetIcon)
		#(80 		'-----')
		#(90		'fileOut' 							nil 	model 			fileOutMessage 											fileOutIcon)
		#(100		'run test (t)' 							$t 	model 			runMethodTest 											weatherFewCloudsIcon)
		#(110		'debug test (r)' 							$r 	model 			debugMethodTest 											weatherFewCloudsIcon)
		#(120		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 											exploreIcon)
		#(130 		'-----')
		#(140		'senders of... (n)' 							nil 	window 			browseSendersOfMessages 											mailForwardIcon)
		#(150		'local senders of...' 							nil 	window 			browseLocalSendersOfMessages 											mailForwardIcon)
		#(160		'implementors of... (m)' 							nil 	window 			browseImplementorsOfMessages 											developmentIcon)
		#(170		'local implementors of...' 							nil 	window 			browseLocalImplementorsOfMessages 											developmentIcon)
		#(180		'inheritance (i)' 							$i 	window 			methodInheritance 											goDownIcon)
		#(190		'versions (v)' 							$v 	window 			browseVersions 											clockIcon)
		#(200 		'-----')
		#(210		'remove method (x)' 							$x 	model 			removeMessage 											deleteIcon)
		#(220		'more' 							nil 	nil 			2000 											listAddIcon)

		"Shortcut only. No menu entry."
		#(991		nil 							$m 	window 			browseImplementors)
		#(992		nil 							$n 	window 			browseSenders)
		#(993		nil 							$c 	model 			copySelector)

		" 'more' SubMenu"
		#(2010		'toggle diffing (D)' 								$D 	model 			toggleDiffing 										switchIcon)
		#(2020 		'-----')
		#(2030		'sample instance' 								nil 	window 			makeSampleInstance 										instanceIcon)
		#(2040		'inspect instances' 								nil 	window 			inspectInstances 										inspectIcon)
		#(2050		'inspect subinstances' 								nil 	window 			inspectSubInstances 										inspectIcon)
		#(2060 		'-----')
		#(2070 		'remove from this browser' 								nil 	model 			removeMessageFromBrowser 										listRemoveIcon)
		#(2080 		'filter message list...' 								nil 	window 			filterMessageList 										findIcon)
		#(2090		'change category...' 								nil 	model 			changeCategory 										saveAsIcon)
		#(2100		'change sets with this method' 								nil 	window 			findMethodInChangeSets 										changesIcon)
		#(2110		'revert to previous version' 								nil 	model 			revertToPreviousVersion 										undoIcon)
		#(2120 		'-----')
		#(2130 		'sort by class hierarchy' 								nil 	model 			sortByClassHierarchy 										classIcon)
		#(2140 		'sort by class name' 								nil 	model 			sortByClassName 										classIcon)
		#(2150 		'sort by selector' 								nil 	model 			sortBySelector 										scriptIcon)
		#(2160 		'sort by date' 								nil 	model 			sortByDate 										dateIcon)
		#(2170 		'reverse sort' 								nil 	model 			sortReverse 										redoIcon)
	).
! !

!methodRemoval: CodeWindow #browseSendersOfMethod stamp: 'jmv 8/23/2024 18:29:50'!
CodeWindow removeSelector: #browseSendersOfMethod!
!methodRemoval: CodeWindow #browseMessages stamp: 'jmv 8/23/2024 18:29:43'!
CodeWindow removeSelector: #browseMessages!
!methodRemoval: CodeWindow #browseInstVarDefs stamp: 'jmv 8/23/2024 18:29:57'!
CodeWindow removeSelector: #browseInstVarDefs!
!methodRemoval: CodeWindow #browseAllMessages stamp: 'jmv 8/23/2024 18:30:04'!
CodeWindow removeSelector: #browseAllMessages!
!methodRemoval: CodeWindow #browseLocalImplementors stamp: 'jmv 8/23/2024 18:30:18'!
CodeWindow removeSelector: #browseLocalImplementors!
!methodRemoval: CodeWindow #browseClassVariables stamp: 'jmv 8/23/2024 18:30:20'!
CodeWindow removeSelector: #browseClassVariables!
!methodRemoval: SystemDictionary #browseInstVarDefs: stamp: 'jmv 8/23/2024 18:30:27'!
SystemDictionary removeSelector: #browseInstVarDefs:!
!methodRemoval: SystemDictionary #browseAllStoresInto:from: stamp: 'jmv 8/23/2024 18:30:34'!
SystemDictionary removeSelector: #browseAllStoresInto:from:!
