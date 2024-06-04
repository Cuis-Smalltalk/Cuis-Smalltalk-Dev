'From Cuis7.1 [latest update: #6433] on 4 June 2024 at 9:52:55 am'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:51:30'!
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

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:51:38'!
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
		#(70		'browse protocol (p)' 							$p 	window 			browseFullProtocol 									spreadsheetIcon)
		#(80		'browse method category (c)' 							$c 	window 			browseCategoryOfCurrentMethod 									editFindReplaceIcon)
		#(90 		'-----')
		#(100		'fileOut' 							nil 	model 			fileOutClass 									fileOutIcon)
		#(110		'run test (t)' 							$t 	model 			runMethodTest 									weatherFewCloudsIcon)
		#(120		'debug test (r)' 							$r 	model 			debugMethodTest 									weatherFewCloudsIcon)
		#(130		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 									exploreIcon)
		#(140 		'-----')
		#(150		'refactorings' 							nil 	nil 			1000 									listAddIcon)
		#(160		'senders of... (n)' 							$n 	window 			browseSendersOfMessages 									mailForwardIcon)
		#(170		'implementors of... (m)' 							$m 	window 			browseMessages 									developmentIcon)
		#(180		'inheritance (i)' 							$i 	window 			methodInheritance 									goDownIcon)
		#(190		'versions (v)' 							$v 	window 			browseVersions 									clockIcon)
		#(200 		'-----')
		#(210		'inst var refs...' 							nil 	window 			browseInstVarRefs 									instanceIcon)
		#(220		'inst var defs...' 							nil 	window 			browseInstVarDefs 									instanceIcon)
		#(230		'class var refs...' 							nil 	window 			browseClassVarRefs 									classIcon)
		#(240		'class variabless' 							nil 	window 			browseClassVariables 									classIcon)
		#(250		'class refs (N)' 							$N 	window 			browseClassRefs 									classIcon)
		#(260 		'-----')
		#(270		'remove method (x)' 							$x 	model 			removeMessage 									deleteIcon)
		#(280		'remove method in hierarchy' 							nil 	model 			removeMessageInHierarchy 									deleteIcon)
		#(290		'more' 							nil 	nil 			2000 									listAddIcon)

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
		#(2010		'toggle diffing (D)' 								$D 	model 			toggleDiffing 									switchIcon)
		#(2020		'implementors of sent messages' 								nil 	window 			browseAllMessages 									developmentIcon)
		#(2030 		'-----')
		#(2040		'local senders of...' 								nil 	window 			browseLocalSendersOfMessages 									mailForwardIcon)
		#(2050		'local implementors of...' 								nil 	window 			browseLocalImplementors 									classIcon)
		#(2060 		'-----')
		#(2070		'sample instance' 								nil 	window 			makeSampleInstance 									instanceIcon)
		#(2080		'inspect instances' 								nil 	window 			inspectInstances 									inspectIcon)
		#(2090		'inspect subinstances' 								nil 	window 			inspectSubInstances 									inspectIcon)
		#(2100		'change category...' 								nil 	model 			changeCategory 									saveAsIcon)
		#(2110		'show category (C)' 								$C 	model 			showHomeCategory 									packageIcon)
		#(2120		'change sets with this method' 								nil 	window 			findMethodInChangeSets 									changesIcon)
		#(2130		'revert to previous version' 								nil 	model 			revertToPreviousVersion 									undoIcon)
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:51:42'!
methodCategoriesListMenuSpec
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Categories' 					false)
		#(10		'fileOut (o)' 								$o 	model 			fileOutMessageCategories 										fileOutIcon)
		#(20 		'-----')
		#(30		'reorganize (r)' 								$r 	model 			editMessageCategories 										sendReceiveIcon)
		#(40		'alphabetize (a)' 								$a 	model 			alphabetizeMessageCategories 										fontXGenericIcon)
		#(50		'remove empty categories (e)' 								$e 	model 			removeEmptyCategories 										listRemoveIcon)
		#(60		'categorize all uncategorized (c)' 								$c 	model 			categorizeAllUncategorizedMethods 										packageIcon)
		#(70		'new category... (n)' 								$n 	model 			addCategory 										newIcon)
		#(80		'new extension category... (N)' 								$N 	model 			addExtensionCategory 										listAddIcon)
		#(90 		'-----')
		#(100		'rename... (R)' 								$R 	model 			renameCategory 										saveAsIcon)
		#(110		'remove (x)' 								$x 	model 			removeMessageCategory 										deleteIcon)
		#(120 		'-----')
		#(130		'run tests (t)' 								$t 	model 			runMessageCategoryTests 										weatherFewCloudsIcon)
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:51:47'!
systemCatSingletonMenuSpec
	"Spec for the menu and shortcut keys for the one-item system category list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class category' 					false)
		#(10		'find class... (f)' 						$f 	window 			findClass 									findIcon)
		#(20 		'-----')
		#(30		'browse all (B)' 						$B 	window 			browseAllClasses 									editFindReplaceIcon)
		#(40		'browse (b)' 						$b 	window 			openSystemCategoryBrowser 									packageIcon)
		#(50 		'-----')
		#(60		'fileOut (o)' 						$o 	model 			fileOutSystemCategory 									fileOutIcon)
		#(70 		'-----')
		#(80		'update (u)' 						$u 	model 			updateSystemCategories 									updateIcon)
		#(90		'rename... (R)' 						$R 	model 			renameSystemCategory 									saveAsIcon)
		#(100		'remove (x)' 						$x 	model 			removeSystemCategory 									deleteIcon)
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:51:56'!
systemCategoriesListMenuSpec
	"Spec for the menu and shortcut keys for the system categories list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class Categories' 					false)
		#(10		'find class... (f)' 						$f 	window 			findClass 									findIcon)
		#(20		'recent classes... (r)' 						$r 	model 			recent 									clockIcon)
		#(30 		'-----')
		#(40		'browse all (B)' 						$B 	window 			browseAllClasses 									editFindReplaceIcon)
		#(50		'browse (b)' 						$b 	window 			openSystemCategoryBrowser 									packageIcon)
		#(60 		'-----')
		#(70		'fileOut (o)' 						$o 	model 			fileOutSystemCategory 									fileOutIcon)
		#(80 		'-----')
		#(90		'reorganize' 						nil 	model 			editSystemCategories 									sendReceiveIcon)
		#(100		'alphabetize (A)' 						$A 	model 			alphabetizeSystemCategories 									fontXGenericIcon)
		#(110 		'-----')
		#(120		'update (u)' 						$u 	model 			updateSystemCategories 									updateIcon)
		#(130		'add item... (a)' 						$a 	model 			addSystemCategory 									newIcon)
		#(140		'rename... (R)' 						$R 	model 			renameSystemCategory 									saveAsIcon)
		#(150		'move to... (m)' 						$m 	model 			moveAllToOtherSystemCategory 									saveAsIcon)
		#(160		'remove (x)' 						$x 	model 			removeSystemCategory 									deleteIcon)
		#(170 		'-----')
		#(180		'move to top' 						nil 	model 			moveSystemCategoryTop 									goTopIcon)
		#(190		'move up' 						nil 	model 			moveSystemCategoryUp 									goUpIcon)
		#(200		'move down' 						nil 	model 			moveSystemCategoryDown 									goDownIcon)
		#(210		'move to bottom' 						nil 	model 			moveSystemCategoryBottom 									fileOutIcon)
		#(220 		'-----')
		#(230		'run tests (t)' 						$t 	model 			runSystemCategoryTests 									weatherFewCloudsIcon)
	)! !


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:52:10'!
codeFileListMenuSpec
	"Spec for the menu and shortcut keys for the single entry filename list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := 
			#(
				#(0 		'Code File' 			false)
				#(10		'find class... (f)' 					$f 	window 			findClass)
				#(20 		'-----') ).
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
			answer := answer, #(
				#(30		'fileIn' 					nil 	model 			fileIn) )].
	answer := answer, 
			#(
				#(40		'fileOut' 					nil 	model 			fileOut) ).
	self model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(50		'remove existing' 					nil 	model 			removeUnmodifiedClasses) )].
	^answer! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:52:15'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^ self model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Messages' 			false)
				#(10		'fileIn' 						nil 	model 			fileInMessage 							updateIcon)
				#(20		'fileOut' 						nil 	model 			fileOutMessage 							fileOutIcon)
				#(30 		'-----')
				#(40		'senders (n)' 						$n 	window 			browseSenders 							mailForwardIcon)
				#(50		'implementors (m)' 						$m 	window 			browseImplementors 							developmentIcon)
				#(60		'method inheritance (h)' 						$h 	window 			methodInheritance 							goDownIcon)
				#(70		'browse full (b)' 						$b 	window 			browseMethodFull 							editFindReplaceIcon)
				#(80		'versions (v)' 						nil 	window 			browseVersions 							clockIcon)
				#(90 		'-----')
				#(100		'remove method (x)' 						$x 	model 			removeMessage 							deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Messages' 			false)
				#(10		'fileOut' 						nil 	model 			fileOutMessage 							fileOutIcon)
				#(20 		'-----')
				#(30		'remove method (x)' 						$x 	model 			removeMessage 							deleteIcon)
			)].! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:52:18'!
methodCategoriesListMenuSpec
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := self model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Message Categories' 			false)
				#(10		'fileIn' 					nil 	model 			fileInMessageCategories 								updateIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Message Categories' 			false)
			)].
	answer := answer, 
			#(
				#(20		'fileOut' 					nil 	model 			fileOutMessageCategories 								fileOutIcon)
				#(30 		'-----')
				#(40		'reorganize' 					nil 	model 			editMessageCategories 								sendReceiveIcon)
				#(50 		'-----')
				#(60		'add item...' 					nil 	model 			addCategory 								newIcon)
				#(70 		'-----')
				#(80		'rename...' 					nil 	model 			renameCategory 								saveAsIcon)
				#(90		'remove' 					nil 	model 			removeMessageCategory 								listRemoveIcon)
			).
	self model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(100 		'-----')
				#(100		'remove existing' 					nil 	model 			removeUnmodifiedMethods 								deleteIcon)
		) ].
	^answer! !


!ChangeListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:52:23'!
listMenuSpec
	"Spec for the menu and shortcut keys for the changes list.
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


!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:52:26'!
listMenuSpec
	"Spec for the menu and shortcut keys for the versions list."
	^model classCommentIndicated
		ifTrue: [ self classCommentVersionsMenuSpec ]
		ifFalse: [ self methodVersionsMenuSpec ]! !


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:52:29'!
changeSetListMenuSpec
	"Spec for the menu and shortcut keys for the changesets list.
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

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:52:36'!
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
			#(80 		'inst var refs...' 								nil 	window 			browseInstVarRefs 						instanceIcon)
			#(90 		'inst var defs...' 								nil 	window 			browseInstVarDefs 						instanceIcon)
			#(100 		'class var refs...' 								nil 	window 			browseClassVarRefs 						classIcon)
			#(110 		'class vars' 								nil 	window 			browseClassVariables 						classIcon)
			#(120 		'class refs (N)' 								$N 	window 			browseClassRefs 						classIcon)
	).
! !

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 09:52:39'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the message list.
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

