'From Cuis7.1 [latest update: #6430] on 4 June 2024 at 6:06:04 am'!

!Morph methodsFor: 'events' stamp: 'jmv 6/4/2024 06:04:58'!
keyboardShortcut: aKeyboardEvent
	"Handle a keyboard shortcut.
	See coment at Morph >> #getMenu
	"
	| menuNshortcutsSpecs shortcutCharacter target widgetKey |
	widgetKey := self menuNshortcutsKey ifNil: [ ^false ].
	menuNshortcutsSpecs := self menuNshortcutsSpec: widgetKey for: self.
	menuNshortcutsSpecs ifEmpty: [ ^false ].
	menuNshortcutsSpecs valuesDo: [ :itemSpec |
		itemSpec size >= 5 ifTrue: [
			shortcutCharacter := itemSpec third.
			shortcutCharacter = aKeyboardEvent keyCharacter ifTrue: [
				target := itemSpec fourth
					ifNotNil: [ :targetGetter | self perform: targetGetter ]
					ifNil: [ self ].
				itemSpec fifth
					ifNotNil: [ :selector |
						target perform: selector.
						^true ]
					ifNil: [
						^false ]
				]
			]
		].
	^false! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 6/4/2024 06:05:16'!
addItemTo: topMenu fromSpec: itemSpec submenus: submenusDictionary
	"
	See coment at Morph >> #getMenu
	"
	| submenuId menu label target selector iconSymbol helpText menuItem |
	label := itemSpec second.
	label ifNil: [ ^self ]. "Only a keyboard shortcut, not a real menu item."
	submenuId := itemSpec first // 1000.
	menu := submenuId = 0 ifTrue: [ topMenu ] ifFalse: [ submenusDictionary at: submenuId ].
	target := itemSpec fourth
		ifNotNil: [ :targetGetter | self perform: targetGetter ]
		ifNil: [ self ].
	selector := itemSpec fifth.
	iconSymbol := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
	helpText := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
	menuItem := label isSymbol
		ifTrue: [ menu addUpdating: label target: target action: selector argumentList: #() ]
		ifFalse: [ menu add: label target: target action: selector argumentList: #() ].
	selector ifNil: [
		menuItem isEnabled: false ].
	iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
	helpText ifNotNil: [ menuItem setBalloonText: helpText ].! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 6/4/2024 06:01:56'!
getMenu
	"Answer the right-click (contextual) menu."
	"Spec for the menu and shortcut keys.
	Each entry is
		#(itemId 			'menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- itemId is a number (integer or float). It specifies the position of the item in the menu, and is also an id, allowing for overrides and deletions.
		- label is a string or symbol. Symbol means self updating menu entry. nil means just keyboard shortcut, no menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Typically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
			If a Number (multiple of 1000), indicates a subMenu. ItemsIds must be in range. See BrowserWindow>>#classListMenuSpec.
			If nil, the menu entry is created but disabled. The keyboard shortcut is disabled too.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"

	| widgetKey menuNshortcutsSpecs itemSpec menu submenus |
	widgetKey := self menuNshortcutsKey ifNil: [ ^nil ].
	menuNshortcutsSpecs := self menuNshortcutsSpec: widgetKey for: self.
	menuNshortcutsSpecs ifEmpty: [ ^nil ].
	menu := MenuMorph new.
	submenus := nil.
	menuNshortcutsSpecs keys sorted do: [ :key |
		itemSpec := menuNshortcutsSpecs at: key.
		itemSpec size <= 2 ifTrue: [
				self addLineTo: menu fromSpec: itemSpec submenus: submenus ]
			ifFalse: [ itemSpec size = 3 ifTrue: [
				self addTitleTo: menu fromSpec: itemSpec ]
			ifFalse: [ itemSpec fifth isNumber ifTrue: [
				submenus ifNil: [submenus := Dictionary new ].
				self addSubmenuTo: menu fromSpec: itemSpec submenus: submenus ]
			ifFalse: [
				self addItemTo: menu fromSpec: itemSpec submenus: submenus ]]]].
	^ menu! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 6/4/2024 06:01:47'!
menuNshortcutsSpec: aWidgetKey for: aWidget
	"menuSpec is the Spec for the menu and shortcut keys.
	Each entry is
		#(itemId 			'menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- itemId is a number. It specifies the position of the item in the menu, and is also an id, allowing for overrides and deletions.
		- label is a string or symbol. Symbol means self updating menu entry. nil means just keyboard shortcut, no menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Typically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
			If a Number (multiple of 1000), indicates a subMenu. ItemsIds must be in range. See BrowserWindow>>#classListMenuSpec.
			If nil, the menu entry is created but disabled. The keyboard shortcut is disabled too.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	"Several nested morphs may provide their own menu items and keyboard shortcuts for submorphs.
	A morph could also provide its own."

	| menuNshortcutsSpecs |

	menuNshortcutsSpecs := Dictionary new.
	self withAllOwnersReverseDo: [ :o |
		o ownMenuNshortcutsSpecs: aWidgetKey for: aWidget do: [ :menuItemSpec |
			(menuItemSpec size = 2 and: [ 		menuItemSpec second first = $X ])
				"#(12 		'XXX') means 'remove menu item numbered 12'"
				ifTrue: [ menuNshortcutsSpecs removeKey: menuItemSpec first ifAbsent: nil ]
				ifFalse: [ menuNshortcutsSpecs at: menuItemSpec first put: menuItemSpec ]]].
	^ menuNshortcutsSpecs! !


!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:02:46'!
classListMenuSpec
	"Spec for the menu and shortcut keys the class list.
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
		#(80		'rename class... (R)' 							$R 	model 			renameClass 									saveAsIcon)
		#(90		'refactorings' 							nil 	nil 			1000 									listAddIcon)
		#(100		'copy class...' 							nil 	model 			copyClass 									copyIcon)
		#(110		'remove class (x)' 							$x 	model 			removeClass 									deleteIcon)
		#(120 		'-----')
		#(130		'show classes hierarchically' 							nil 	model 			enableListClassesHierarchically 									goTopIcon)
		#(140		'show classes alphabetically' 							nil 	model 			enableListClassesAlphabetically 									sendReceiveIcon)
		#(150		'show definition' 							nil 	model 			editClass 									editFindReplaceIcon)
		#(160		'show comment' 							nil 	model 			editComment 									editFindReplaceIcon)
		#(170 		'-----')
		#(180		'inst var refs...' 							nil 	window 			browseInstVarRefs 									instanceIcon)
		#(190		'inst var defs...' 							nil 	window 			browseInstVarDefs 									instanceIcon)
		#(200 		'-----')
		#(210		'class var refs...' 							nil 	window 			browseClassVarRefs 									classIcon)
		#(220		'class vars' 							nil 	window 			browseClassVariables 									classIcon)
		#(230		'class refs (N)' 							$N 	window 			browseClassRefs 									classIcon)
		#(240 		'-----')
		#(250		'more' 							nil 	nil 			2000 									listAddIcon)
		#(260		nil 							$r 	model 			recent) "Shortcut only. No menu entry."

		" 'refactorings' SubMenu"
		#(1010		'insert superclass...' 							nil 	model 			insertSuperclass 									saveAsIcon)
		#(1020 		'-----')
		#(1030		'add inst var...' 							nil 	model 			addInstVar 									listAddIcon)
		#(1040		'rename inst var...' 							nil 	model 			renameInstVar 									saveAsIcon)
		#(1050		'remove inst var...' 							nil 	model 			removeInstVar 									deleteIcon)
		#(1060		'remove all unreferenced inst var...' 						
											nil 	model 			removeAllUnreferencedInstVar 									deleteIcon)
		#(1070		'push up inst var...' 							nil 	model 			pushUpInstanceVariable 									goTopIcon)
		#(1080		'push down inst var...' 							nil 	model 			pushDownInstanceVariable 									goBottomIcon)
		#(1090 		'-----')
		#(1100		'add inst var...' 							nil 	model 			addInstVar 									listAddIcon)

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


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:02:53'!
classListMenuSpec
	"Spec for the menu and shortcut keys the class list.
	See coment at Morph >> #getMenu
	"
	^self model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Class List' 			false)
				#(10		'definition' 					nil 	model 			editClass 						editFindReplaceIcon)
				#(20		'comment' 					nil 	model 			editComment 						editFindReplaceIcon)
				#(30 		'-----')
				#(40		'browse full (b)' 					$b 	window 			browseMethodFull 						editFindReplaceIcon)
				#(50		'class refs (N)' 					$N 	window 			browseClassRefs 						classIcon)
				#(60 		'-----')
				#(70		'fileIn' 					nil 	model 			fileInClass 						updateIcon)
				#(80		'fileOut' 					nil 	model 			fileOutClass 						fileOutIcon)
				#(90 		'-----')
				#(100		'rename...' 					nil 	model 			renameClass 						saveAsIcon)
				#(110		'remove' 					nil 	model 			removeClass 						listRemoveIcon)
				#(120 		'-----')
				#(130		'remove existing' 					nil 	model 			removeClass 						deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Class List' 			false)
				#(10		'definition' 					nil 	model 			editClass 						editFindReplaceIcon)
				#(20		'comment' 					nil 	model 			editComment 						editFindReplaceIcon)
				#(30 		'-----')
				#(80		'fileOut' 					nil 	model 			fileOutClass 						fileOutIcon)
				#(90 		'-----')
				#(100		'rename...' 					nil 	model 			renameClass 						saveAsIcon)
				#(110		'remove' 					nil 	model 			removeClass 						listRemoveIcon)
				#(120 		'-----')
				#(130		'remove existing' 					nil 	model 			removeClass 						deleteIcon)
			) ].! !


!ChangeListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:02:58'!
listMenuSpec
	"Spec for the menu and shortcut keys the changes list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Change List' 									true)
		#(10		'fileIn selections' 									nil 	model 			fileInSelections 										nil
					'import the selected items into the image')
		#(20		'fileOut selections...	' 								nil 	model 			fileOutSelections 										nil
		 			'create a new file containing the selected items')
		#(30		'fileOut current version of selections...' 					nil 					model 			fileOutCurrentVersionsOfSelections 										nil
 					'create a new file containing the current (in-image) counterparts of the selected methods')
		#(40 		'-----')
		#(50		'compare to current' 									nil 	model 			compareToCurrentVersion 										nil
 					'open a separate window which shows the text differences between the on-file version and the in-image version.')
		#(60		'toggle diffing (D)' 									$D 	model 			toggleDiffing 										nil
 					'start or stop showing diffs in the code pane.')
		#(70 		'-----')
		#(80		'select new methods' 									nil 	model 			selectNewMethods 										nil
 					'select methods in the file that do not currently exist in the image')
		#(90		'select changes for absent classes' 									nil 	model 			selectAllForAbsentClasses 										nil
					'select methods in the file for classes that are not defined in the image')
		#(100		'select all changes for this class' 									nil 	model 			selectAllForThisClass 										nil
					'select all methods in the file that belong to the currently-selected class')
		#(110		'select unchanged methods' 									nil 	model 			selectUnchangedMethods 										nil
					'select methods in the file whose in-image versions are the same as their in-file counterparts')
		#(120		'select methods equivalent to current' 									nil 	model 			selectEquivalentMethods 										nil
					'select methods in the file whose in-image versions have the same behavior as their in-file counterparts')
		#(130		'select methods older than current' 									nil 	model 			selectMethodsOlderThanCurrent 										nil
					'select methods in the file that are older than the one currently in the image')
		#(140		'select removals of sent methods' 									nil 	model 			selectRemovalsOfSent 										nil
					'select all method removals of methods that have some sender in the image')
		#(150 		'-----')
		#(160		'select all (a)' 									$a 	model 			selectAll 										nil
					'select all the items in the list')
		#(170		'deselect all (A)' 									$A 	model 			deselectAll 										nil
					'deselect all the items in the list')
		#(180		'invert selections' 									nil 	model 			invertSelections 										nil
					'select every item that is not currently selected, and deselect every item that *is* currently selected')
		#(190 		'-----')
		#(200		'browse class and method' 									nil 	window 			browseMethodFull 										nil
					'open a full browser showing the selected method')
		#(210		'browse all versions of single selection' 									nil 	window 			browseVersions 										nil
					'open a version browser showing the versions of the currently selected method')
		#(220		'browse current versions of selections' 									nil 	window 			browseCurrentVersionsOfSelections 										nil
					'open a message-list browser showing the current (in-image) counterparts of the selected methods')
		#(230		'destroy current methods of selections' 									nil 	model 			destroyCurrentCodeOfSelections 										nil
					'remove (*destroy*) the in-image counterparts of all selected methods')
		#(240 		'-----')
		#(250		'remove doIts' 									nil 	model 			removeDoIts 										nil
					'remove all items that are doIts rather than definitions')
		#(260		'remove older versions' 									nil 	model 			removeOlderMethodVersions 										nil
					'remove all but the most recent versions of methods in the list')
		#(270		'remove up-to-date versions' 									nil 	model 			removeUpToDate 										nil
					'remove all items whose code is the same as the counterpart in-image code')
		#(280		'remove empty class comments' 									nil 	model 			removeEmptyClassComments 										nil
					'remove all empty class comments')
		#(290		'remove selected items' 									nil 	model 			removeSelections 										nil
					'remove the selected items from the change-list')
		#(300		'remove unselected items' 									nil 	model 			removeNonSelections 										nil
					'remove all the items not currently selected from the change-list')
	)! !


!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:03:04'!
classCommentVersionsMenuSpec
	"Spec for the menu and shortcut keys for a class comment version in the versions list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Versions' 							true)
		#(10		'compare to current' 							nil 	model 			compareToCurrentVersion 										nil
					'compare selected version to the current version')
		#(20		'revert to selected version' 							nil 	model 			fileInSelections 										nil
					'resubmit the selected version, so that it becomes the current version')
		#(30 		'-----')
		#(40		'toggle diffing (D)' 							$D 	model 			toggleDiffing 										nil
					'toggle whether or not diffs should be shown here')
		#(50		'update list' 							nil 	model 			reformulateList 										nil
					'reformulate the list of versions, in case it somehow got out of synch with reality')
		#(60 		'-----')
		#(70		'help...' 							nil 	model 			offerVersionsHelp 										nil
					'provide an explanation of the use of this tool')
	)! !

!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:03:10'!
methodVersionsMenuSpec
	"Spec for the menu and shortcut keys for a method version in the versions list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
				#(0 		'Versions' 									true)
	).
	model listIndex > 0 ifTrue: [
		(model list size > 1 ) ifTrue: [
			answer := answer, #(
				#(10		'compare to current' 									nil 	model 			compareToCurrentVersion 									nil
							'compare selected version to the current version')
				#(20		'compare to version...' 									nil 	model 			compareToOtherVersion 									nil
							'compare selected version to another selected version'))].
		answer := answer, #(
				#(30		'revert to selected version (z)' 									$z 	model 			fileInSelections 									nil
							'resubmit the selected version, so that it becomes the current version'))].
	answer := answer, #(
				#(40		'edit current method (O)' 									$O 	window 			openSingleMessageBrowser 									nil
							'open a single-message browser on the current version of this method')
				#(50 		'-----')
				#(60		'toggle diffing (D)' 									$D 	model 			toggleDiffing 									nil
							'toggle whether or not diffs should be shown here')
				#(70		'update list' 									nil 	model 			reformulateList 									nil
							'reformulate the list of versions, in case it somehow got out of synch with reality')
				#(80 		'-----')
				#(90		'senders (n)' 									$n 	window 			browseSenders 									nil
							'browse all senders of this selector')
				#(100		'implementors (m)' 									$m 	window 			browseImplementors 									nil
							'browse all implementors of this selector')
				#(110 		'-----')
				#(120		'help...' 									nil 	model 			offerVersionsHelp 									nil
							'provide an explanation of the use of this tool')
	).
	^answer! !


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:03:16'!
changeSetListMenuSpec
	"Spec for the menu and shortcut keys the changesets list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
			#(0 		'Change Set' 						true)
			#(10 		'File out and remove (o)' 							$o 	model 			fileOutAndRemove 								fileOutIcon)
			#(20 		'File out and keep (k)' 							$k 	model 			fileOutAndKeep 								fileOutIcon)
			#(30 		'-----')
			#(40 		'Rename change set (R)' 					 		$R 	model 			rename 								saveAsIcon)
			#(50 		'Destroy change set (x)' 			 				$x 	model 			remove 								warningIcon)
	).
	model currentCanHavePreambleAndPostscript ifTrue: [
		model currentHasPreamble
			ifTrue: [ answer  := answer, #(
				#(60 		'-----')
				#(70 		'Edit preamble (p)' 						$p 	model 			addPreamble 								textEditorIcon)
				#(80 		'Remove preamble' 						nil 	model 			removePreamble 								listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(60 		'-----')
				#(70 		'Add preamble (p)' 						$p 	model 			addPreamble 								listAddIcon) )].
		model currentHasPostscript
			ifTrue: [ answer  := answer, #(
				#(90 		'Edit postscript...' 						nil 	model 			editPostscript 								textEditorIcon)
				#(100 		'Remove postscript' 						nil 	model 			removePostscript 								listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(90 		'Add postscript...' 						nil 	model 			editPostscript 								listAddIcon) )].
	].

	answer  := answer, #(
		#(110 		'-----')
		#(120 		'conflicts with other change sets' 								nil 	window			browseMethodConflicts 								emblemImportantIcon
			'Browse all methods that occur both in this change set and in at least one other change set.')
		#(130 		'-----')
		#(140 		'trim history' 								nil 	model			trimHistory 								clockIcon
			'Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes.
 NOTE: can cause confusion if later filed in over an earlier version of these changes.')
		#(150 		'view affected class categories' 								nil 	model 			viewAffectedClassCategories 								packageIcon
			'Show class categories affected by any contained change')
	).
	model csIsForBase ifFalse: [ "Maybe disable a few: Copy them the same as above, but without selector."
		answer := answer, #(
			#(40 		'Rename change set (R)' 					 		$R 	model 			nil 								saveAsIcon)
			#(50 		'Destroy change set (x)' 			 				$x 	model 			nil 								warningIcon)
			#(140 		'trim history' 							nil 	model			nil 								clockIcon)) ].
	^answer! !

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:03:21'!
classListMenuSpec
	"Spec for the menu and shortcut keys the class list.
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
			#(80 		'inst var refs...' 								nil 	window 			browseInstVarRefs 						instanceIcon)
			#(90 		'inst var defs...' 								nil 	window 			browseInstVarDefs 						instanceIcon)
			#(100 		'class var refs...' 								nil 	window 			browseClassVarRefs 						classIcon)
			#(110 		'class vars' 								nil 	window 			browseClassVariables 						classIcon)
			#(120 		'class refs (N)' 								$N 	window 			browseClassRefs 						classIcon)
	).
! !

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:03:26'!
messageListMenuSpec
	"Spec for the menu and shortcut keys the message list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Message List' 						true)
			#(10 		'delete method from change set (d)' 									$d 	model 			forget 								warningIcon)
			#(20 		'remove method from system (x)' 									$x 	model 			removeMessage 								deleteIcon)
			#(30 		'-----')
			#(40 		'browse full (b)' 									$b 	window 			browseMethodFull 								editFindReplaceIcon)
			#(50 		'browse hierarchy (h)' 									$h 	window 			browseHierarchy 								goTopIcon)
			#(60 		'browse method (O)' 									$O 	window 			openSingleMessageBrowser 								scriptIcon)
			#(70 		'browse protocol (p)' 									$p 	window 			browseFullProtocol 								spreadsheetIcon)
			#(80 		'-----')		
			#(90 		'fileOut (o)' 									$o 	model 			fileOutMessage 								fileOutIcon)
			#(100 		'-----')		
			#(110 		'senders of... (n)' 									$n 	window 			browseSendersOfMessages 								mailForwardIcon)
			#(120 		'implementors of... (m)' 									$m 	window 			browseMessages 								developmentIcon)
			#(130 		'inheritance (i)' 									$i 	window 			methodInheritance 								goDownIcon)
			#(140 		'versions (v)' 									$v 	window 			browseVersions 								clockIcon)
	).
! !


!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:03:33'!
contextStackMenuSpec
	"Spec for the menu and shortcut keys for call stack list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10		'fullStack (f)' 						$f 	window 			fullStack 						nil 	'show the complete call stack')
		#(20		'proceed (p)' 						$p 	window 			proceed 						nil 	'close the debugger and proceed')
		#(30		'restart (r)' 						$r 	window 			restart 						nil 	'reset this context to its start')
		#(40		'step into send (e)' 						$e 	window 			send 						nil 	'step into message sends')
		#(50		'step over send (t)' 						$t 	window 			doStep 						nil 	'step over message sends')
		#(60		'step into block (T)' 						$T 	window 			stepIntoBlock 						nil 	'step into a code block')
		#(70		'run to cursor' 						nil 	window 			runToCursor 						nil 	'run to cursor')
		#(80		'where (w)' 						$w 	window 			where 						nil 	'select current pc range')
		#(90		'peel to first like this' 						nil 	window 			peelToFirst 						nil 	'peel stack back to the second occurance of selected message')
		#(100 		'-----')
		#(110		'return entered value' 						nil 	window 			returnValue 						nil 	'lets you enter an expression and return method answering result')
		#(120		'toggle break on entry' 						nil 	model 			toggleBreakOnEntry 						nil 	'install or uninstall a breakpoint on method entry')
	)! !

!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:03:39'!
contextVariablesListMenuSpec
	"Spec for the menu and shortcut keys for the list of variables in this context.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10		'inspect (i)' 						$i 	window 			inspectContextSelection 							inspectIcon)
		#(20		'explore (I)' 						$I 	window 			exploreContextSelection 							exploreIcon)
		#(30 		'-----')
		#(40		'browser full (b)' 						$b 	window 			contextClassBrowse 							editFindReplaceIcon)
		#(50		'browser hierarch (h)' 						$h 	window 			contextClassHierarchy 							goTopIcon)
		#(60		'browser protocol (p)' 						$p 	window 			contextClassProtocol 							spreadsheetIcon)
	)! !

!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:03:44'!
receiverVariablesListMenuSpec
	"Spec for the menu and shortcut keys for the list of instance variables in the receiver.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10		'inspect (i)' 						$i 	window 			inspectReceiverSelection 							inspectIcon)
		#(20		'explore (I)' 						$I 	window 			exploreReceiverSelection 							exploreIcon)
		#(30 		'-----')
		#(40		'browser full (b)' 						$b 	window 			receiverClassBrowse 							editFindReplaceIcon)
		#(50		'browser hierarch (h)' 						$h 	window 			receiverClassHierarchy 							goTopIcon)
		#(60		'browser protocol (p)' 						$p 	window 			receiverClassProtocol 							spreadsheetIcon)
	)! !


!FileListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:03:50'!
directoryTreeMenuSpec
	"Spec for the menu and shortcut keys for our tree nodes.
	See coment at Morph >> #getMenu
	"
	| showHiddenTogleAndLabel |
	showHiddenTogleAndLabel := model showHidden asMenuItemTextPrefix, 'show hidden folders and files'.
	^{
		{10. 'delete directory...'. 								nil. 	#model. 			#deleteDirectory. 							#warningIcon.
			'Delete the selected directory'}.
		{20. 	showHiddenTogleAndLabel. 							nil. 	#model. 			#toggleShowHidden. 							nil.
			'show or hide files and directories that begin with a dot'}.
		{30. 	'show Cuis top as tree root'. 							nil. 	#window. 			#useCuisTopAsTreeRoot. 							nil.
			'use the default Cuis top directory'}.
		{40. 'show OS roots as tree root'. 								nil. 	#window. 			#useOSrootsAsTreeRoot. 							nil.
			'use the host OS root directories. Drives on Windows; "/" on Unix'}
	}! !


!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:03:56'!
fieldListMenuSpec
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
		#(10		'inspect (i)' 						$i 	window 			inspectSelection 							inspectIcon)
		#(20		'explore (I)' 						$I 	window 			exploreSelection 							exploreIcon)
		#(30		'copy to clipboard (c)' 						$c 	window 			copySelectionToClipboard 							copyIcon)
		#(40		'basic inspect' 						nil 	window 			inspectBasic 							inspectIcon)
		#(50		'references finder' 						nil 	window 			openReferencesFinder 							exploreIcon)
		#(60		'weight explorer' 						nil 	window 			openWeightExplorer 							exploreIcon)
	).
	(self model objectSpecificMenuSpecFrom: self) ifNotNil: [ :spec |
		answer := answer, spec ].
	answer := answer, #(
		#(210 		'-----')
		#(220		'browser full (b)' 						$b 	window 			browseMethodFull 							editFindReplaceIcon)
		#(230		'browser hierarch (h)' 						$h 	window 			browseHierarchy 							goTopIcon)
		#(240		'browser protocol (p)' 						$p 	window 			browseFullProtocol 							spreadsheetIcon)
	).
	^answer! !

!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:04:01'!
menuSpecForDictionary
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	^#(
		#(100 		'-----')
		#(110		'inspect key' 					nil 	window 			inspectSelectedKey 							editFindReplaceIcon)
		#(120		'senders of this key' 					nil 	window 			sendersOfSelectedKey 							mailForwardIcon)
		#(130		'add key' 					nil 	window 			addEntry 							listAddIcon)
		#(140		'rename key' 					nil 	window 			renameEntry 							saveAsIcon)
		#(150		'remove' 					nil 	model 			removeSelection 							listRemoveIcon)
	)! !

!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:04:06'!
menuSpecForSet
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	^#(
		#(100 		'-----')
		#(110		'remove' 			nil 	model 			removeSelection 					listRemoveIcon)
	)! !


!ObjectExplorerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:04:11'!
listMenuSpec
	"Spec for the menu and shortcut keys for our tree nodes.
	See coment at Morph >> #getMenu
	"
	| answer |
	model getCurrentSelection
		ifNil: [^#()]
		ifNotNil: [ :currSel |
			answer := #(
				#(10		'inspect (i)' 						$i 	window 			inspectSelection 							inspectIcon)
				#(20		'explore (I)' 						$I 	window 			exploreSelection 							exploreIcon)
				#(30		'copy to clipboard (c)' 						$c 	window 			copySelectionToClipboard 							copyIcon)
				#(40		'basic inspect' 						nil 	window 			inspectBasic 							inspectIcon)
				#(50		'references finder' 						nil 	window 			openReferencesFinder 							exploreIcon)
				#(60		'weight explorer' 						nil 	window 			openWeightExplorer 							exploreIcon)
				#(70 		'-----')
				#(80		'browser full (b)' 						$b 	window 			browseMethodFull 							editFindReplaceIcon)
				#(90		'browser hierarch (h)' 						$h 	window 			browseHierarchy 							goTopIcon)
				#(100		'browser protocol (p)' 						$p 	window 			browseFullProtocol 							spreadsheetIcon)).
			(currSel item is: #Morph) ifTrue: [ answer := answer, #(
				#(110 		'-----')
				#(120		'show morph halo' 						nil 	window 			addHaloToSelection 							editFindReplaceIcon)) ].
			model class == ReferencesExplorer ifTrue: [ answer := answer, #(
				#(210 		'-----')
				#(220		'rescan' 						nil 	window 			rescan 							editFindReplaceIcon)) ].
			answer := answer, #(
				#(310 		'-----')
				#(320		'monitor changes' 						nil 	window 			monitorChanges 							editFindReplaceIcon)).
			model isMonitoring ifTrue: [ answer := answer, #(
				#(410 		'-----')
				#(420		'stop monitoring all' 						nil 	window 			stopMonitoring 							editFindReplaceIcon)) ].
			^answer ].! !


!ProcessBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:04:15'!
processListMenuSpec
	"Spec for the menu and shortcut keys for the list of processes.
	See coment at Morph >> #getMenu
	"
	| rules answer |
	answer := #().
	model selectedProcess ifNotNil: [ :selectedProcess |
		rules := model class rulesFor: model selectedProcess.
		answer := answer, #(
				#(10		'inspect (i)' 						$i 	window 			inspectProcess 						inspectIcon)
				#(20		'explore (I)' 						$I 	window 			exploreProcess 						exploreIcon)
				#(30		'references finder (P)' 						$P 	window 			openReferencesFinder 						exploreIcon)).
		rules first ifTrue: [
			answer := answer, 
				#((40		'terminate (t)' 						$t 	model 			terminateProcess 						cancelIcon)).
			selectedProcess isSuspended ifTrue: [ answer := answer,
				#((50		'resume (r)' 						$r 	model 			resumeProcess 						mediaPlaybackStartIcon))]
			ifFalse: [ answer := answer, 
				#((50 		'suspend (s)' 						$s 	model 			suspendProcess 						chatIcon))]].
		rules second ifTrue: [ answer := answer, #(
				#(60 		'change priority (p)' 						$p 	window 			changePriority 						systemMonitorIcon)
				#(70 		'debug (d)' 						$d 	window 			debugProcess 						debugIcon))].
		(selectedProcess suspendingList isKindOf: Semaphore) ifTrue: [ answer := answer,
				#((80 		'signal Semaphore (S)' 						$S 	model 			signalSemaphore 						haloHelpIcon))].
		answer := answer, #(
				#(90		'full stack (k)' 						$k 	model 			moreStack 						systemIcon)
				#(100 		'-----'))].
	answer := answer, #(
				#(110		'find context... (f)' 						$f 	window 			findContext 						findIcon)
				#(120		'find again (g)' 						$g 	model 			nextContext 						systemIcon)
				#(130 		'-----')).
	answer := answer, (isStepping ifTrue: [
				#((140		'turn off auto-update (a)' 						$a 	window 			toggleAutoUpdate 						cancelIcon)) ]
			ifFalse: [
				#((140		'turn on auto-update (a)' 						$a 	window 			toggleAutoUpdate 						updateIcon)) ]).
	answer := answer, #(
				#(150		'update list (u)' 						$u 	model 			updateProcessList 						updateIcon)
				#(160 		'-----')).
	answer := answer, (CPUWatcher isMonitoring ifTrue: [
				#((170		'stop CPUWatcher' 						nil 	model 			stopCPUWatcher 						inspectIcon)) ]
			ifFalse: [
				#((170		'start CPUWatcher' 						nil 	model 			startCPUWatcher 						inspectIcon)) ]).
	answer := answer, (StackSizeWatcher isWatching ifTrue: [
				#((180		'stop StackSizeWatcher' 						nil 	model 			stopStackSizeWatcher 						inspectIcon)) ]
			ifFalse: [
				#((180		'start StackSizeWatcher' 						nil 	model 			startStackSizeWatcher 						inspectIcon)) ]).
	^answer! !

!ProcessBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:04:20'!
stackMenuSpec
	"Spec for the menu and shortcut keys for the list of stack frames.
	See coment at Morph >> #getMenu
	"
	^ #(
		#(10		'inspect context (c)' 					$c 	window 			inspectContext 					inspectIcon)
		#(20		'explore context (C)' 					$C 	window 			exploreContext 					exploreIcon)
		#(30		'inspect receiver (i)' 					$i 	window 			inspectReceiver 					inspectIcon)
		#(40		'explore receiver (I)' 					$I 	window 			exploreReceiver 					exploreIcon)
		#(50		'browse context (b)' 					$b 	window 			browseContext 					editFindReplaceIcon)
	).! !


!TestRunnerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 06:04:25'!
listMenuSpec
	"Spec for the menu and shortcut keys for the list of test classes.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
		#(0 		'Test Cases Set' 						false)
		#(10		'select all (a)' 					$a 	model 			selectAll 					selectAllIcon)
		#(20		'deselect all (A)' 					$A 	model 			deselectAll 					selectAllIcon)
		#(30		'invert selections' 					nil 	model 			invertSelections 					switchIcon)
		#(40		'filter (f)' 					$f 	model 			setFilter 					findIcon)
		#(50		'refresh (u)' 					$u 	model 			refreshTests 					refreshIcon)
		#(60 		'-----')).
	model isRunning ifTrue: [ answer := answer, #(
		#(70		'stop (l)' 					$l 	model 			interruptRun 					stopIcon))]
	ifFalse: [ answer := answer, #(
		#(70		'run (r)' 					$r 	model 			runTests 					playIcon)
		#(80		'run one (o)' 					$o 	model 			runOneTest 					playIcon)
		#(90		'run profiled (p)' 					$p 	model 			runProfiledTests 					playIcon))].
	model selectedSuite > 0 ifTrue: [ answer := answer, #(
		#(100 		'-----')
		#(110		'browse' 					nil 	window 			browseSelected 					editFindReplaceIcon))].
	answer := answer, #(
		#(100 		'-----')
		#(110		'log to Transcript' 					nil 	model 			showResult 					printerIcon)).
	^answer! !

