'From Cuis7.1 [latest update: #6413] on 27 May 2024 at 4:27:31 pm'!

!ChangeSorter methodsFor: 'access' stamp: 'jmv 5/27/2024 11:57:32'!
csIsForBase
	^ myChangeSet isForBaseSystem! !


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/27/2024 15:54:34'!
changeSetListMenuSpec
	"Spec for the menu and shortcut keys the changesets list.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	| answer |
	answer := #(
			#(0 		'Change Set' 						true)
			#(10 		'File out and remove (o)' 							$o 	model 			fileOutAndRemove 							nil 				fileOutIcon)
			#(20 		'File out and keep (k)' 							$k 	model 			fileOutAndKeep 							nil 				fileOutIcon)
			#(30 		'-----')
			#(40 		'Rename change set (R)' 					 		$R 	model 			rename 							csIsForBase 				saveAsIcon)
			#(50 		'Destroy change set (x)' 			 				$x 	model 			remove 							csIsForBase 				warningIcon)
	).
	model currentCanHavePreambleAndPostscript ifTrue: [
		model currentHasPreamble
			ifTrue: [ answer  := answer, #(
				#(60 		'-----')
				#(70 		'Edit preamble (p)' 					$p 	model 			addPreamble 							nil 				textEditorIcon)
				#(80 		'Remove preamble' 					nil 	model 			removePreamble 							nil 				listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(60 		'-----')
				#(70 		'Add preamble (p)' 					$p 	model 			addPreamble 							nil 				listAddIcon) )].
		model currentHasPostscript
			ifTrue: [ answer  := answer, #(
				#(90 		'Edit postscript...' 					nil 	model 			editPostscript 							nil 				textEditorIcon)
				#(100 		'Remove postscript' 					nil 	model 			removePostscript 							nil 				listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(90 		'Add postscript...' 					nil 	model 			editPostscript 							nil 				listAddIcon) )].
	].

	answer  := answer, #(
		#(110 		'-----')
		#(120 		'conflicts with other change sets' 								nil 	window			browseMethodConflicts 							nil 				emblemImportantIcon
			'Browse all methods that occur both in this change set and in at least one other change set.')
		#(130 		'-----')
		#(140 		'trim history' 								nil 	model			trimHistory 							csIsForBase 				clockIcon
			'Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes.
 NOTE: can cause confusion if later filed in over an earlier version of these changes.')
		#(150 		'view affected class categories' 								nil 	model 			viewAffectedClassCategories 								nil 			packageIcon
			'Show class categories affected by any contained change')
	).
	^answer! !

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/27/2024 16:00:14'!
classListMenuSpec
	"Spec for the menu and shortcut keys the class list.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	^#(
			#(0 		'Class List' 						false)
			#(10 		'delete class from change set (d)' 								$d 	model 			forgetClass 						nil 	warningIcon)
			#(20 		'remove class from system (x)' 								$x 	model 			removeClass 						nil 	deleteIcon)
			#(30 		'-----')
			#(40 		'browse full (b)' 								$b 	window 			browseMethodFull 						nil 	editFindReplaceIcon)
			#(50 		'browse hierarchy (h)' 								$h 	window 			browseHierarchy 						nil 	goTopIcon)
			#(60 		'browse protocol (p)' 								$p 	window 			browseFullProtocol 						nil 	spreadsheetIcon)
			#(70 		'-----')		
			#(80 		'inst var refs...' 								nil 	window 			browseInstVarRefs 						nil 	instanceIcon)
			#(90 		'inst var defs...' 								nil 	window 			browseInstVarDefs 						nil 	instanceIcon)
			#(100 		'class var refs...' 								nil 	window 			browseClassVarRefs 						nil 	classIcon)
			#(110 		'class vars' 								nil 	window 			browseClassVariables 						nil 	classIcon)
			#(120 		'class refs (N)' 								$N 	window 			browseClassRefs 						nil 	classIcon)
	).
! !

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/27/2024 16:12:37'!
messageListMenuSpec
	"Spec for the menu and shortcut keys the message list.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	^#(
			#(0 		'Message List' 						true)
			#(10 		'delete method from change set (d)' 									$d 	model 			forget 								nil 	warningIcon)
			#(20 		'remove method from system (x)' 									$x 	model 			removeMessage 								nil 	deleteIcon)
			#(30 		'-----')
			#(40 		'browse full (b)' 									$b 	window 			browseMethodFull 								nil 	editFindReplaceIcon)
			#(50 		'browse hierarchy (h)' 									$h 	window 			browseHierarchy 								nil 	goTopIcon)
			#(60 		'browse method (O)' 									$O 	window 			openSingleMessageBrowser 								nil 	scriptIcon)
			#(70 		'browse protocol (p)' 									$p 	window 			browseFullProtocol 								nil 	spreadsheetIcon)
			#(80 		'-----')		
			#(90 		'fileOut (o)' 									$o 	model 			fileOutMessage 								nil 	fileOutIcon)
			#(100 		'-----')		
			#(110 		'senders of... (n)' 									$n 	window 			browseSendersOfMessages 								nil 	mailForwardIcon)
			#(120 		'implementors of... (m)' 									$m 	window 			browseMessages 								nil 	developmentIcon)
			#(130 		'inheritance (i)' 									$i 	window 			methodInheritance 								nil 	goDownIcon)
			#(140 		'versions (v)' 									$v 	window 			browseVersions 								nil 	clockIcon)
	).
! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 5/27/2024 16:08:48'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| changeSetList classList changeSetListPane classListPane
	  messageList upperPanes labelBackground |
	labelBackground := Theme current background.
	model myChangeSet ifNil: [
		"Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	changeSetList := (PluggableListMorphByItem
				withModel: model
				listGetter: #changeSetList
				indexGetter: #currentChangeSetName
				indexSetter: #showChangeSetNamed:)
					menuNshortcutsKey: #changeSetListMenuSpec;
					autoDeselect: false.
	changeSetListPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Change Set name');
		addMorphUseAll: changeSetList.

	classList := (PluggableListMorphByItem
				withModel: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:)
					menuNshortcutsKey: #classListMenuSpec.
	classListPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes := LayoutMorph newRow.
	upperPanes
		addMorph: changeSetListPane proportionalWidth: 0.6;
		addAdjusterAndMorph: classListPane proportionalWidth: 0.4.
	
	messageList := (PluggableListMorphByItem
				withModel: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:)
					menuNshortcutsKey: #messageListMenuSpec.
	messageList := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Methods');
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString! !

!methodRemoval: ChangeSorterWindow #classListKey:from: stamp: 'jmv 5/27/2024 16:25:18'!
ChangeSorterWindow removeSelector: #classListKey:from:!
!methodRemoval: ChangeSorterWindow #messageListKey:from: stamp: 'jmv 5/27/2024 16:25:29'!
ChangeSorterWindow removeSelector: #messageListKey:from:!
!methodRemoval: ChangeSorterWindow #classListMenu stamp: 'jmv 5/27/2024 16:25:50'!
ChangeSorterWindow removeSelector: #classListMenu!
!methodRemoval: ChangeSorterWindow #changeSetMenu stamp: 'jmv 5/27/2024 16:22:32'!
ChangeSorterWindow removeSelector: #changeSetMenu!
!methodRemoval: ChangeSorterWindow #messageMenu stamp: 'jmv 5/27/2024 16:22:32'!
ChangeSorterWindow removeSelector: #messageMenu!
!methodRemoval: ChangeSorterWindow #changeSetListKey:from: stamp: 'jmv 5/27/2024 16:24:47'!
ChangeSorterWindow removeSelector: #changeSetListKey:from:!

!ChangeSorterWindow reorganize!
('GUI building' buildMorphicWindow initialExtent windowColor)
('menu commands' browseMethodConflicts browseVersions)
('menu & kbd shortcuts' changeSetListMenuSpec classListMenuSpec messageListMenuSpec)
!

