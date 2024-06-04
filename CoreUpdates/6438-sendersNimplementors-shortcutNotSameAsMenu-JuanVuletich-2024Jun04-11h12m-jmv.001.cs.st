'From Cuis7.1 [latest update: #6436] on 4 June 2024 at 11:20:35 am'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 11:16:12'!
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
		#(160		'senders of... (n)' 							nil 	window 			browseSendersOfMessages 									mailForwardIcon)
		#(161		nil 							$n 	window 			browseSenders)
		#(170		'implementors of... (m)' 							nil 	window 			browseMessages 									developmentIcon)
		#(171		nil 							$m 	window 			browseImplementors)
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


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 11:19:02'!
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
			#(110		'senders of... (n)' 									nil 	window 			browseSendersOfMessages 								mailForwardIcon)
			#(111		nil 									$n 	window 			browseSenders)
			#(120		'implementors of... (m)' 									nil 	window 			browseMessages 								developmentIcon)
			#(121		nil 									$m 	window 			browseImplementors)		
			#(130 		'inheritance (i)' 									$i 	window 			methodInheritance 								goDownIcon)
			#(140 		'versions (v)' 									$v 	window 			browseVersions 								clockIcon)
	).
! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 11:15:51'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Set' 					false)
		#(10		'what to show...' 							nil 	window 			offerWhatToShowMenu 									preferencesIcon)
		#(20		'toggle break on entry' 							nil 	model 			toggleBreakOnEntry 									debugIcon)
		#(30 		'-----')
		#(40		'browse full (b)' 							$b 	window 			browseMethodFull 									editFindReplaceIcon)
		#(50		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 									goTopIcon)
		#(60		'browse method (O)' 							$O 	window 			openSingleMessageBrowser 									scriptIcon)
		#(70		'browse protocol (p)' 							$p 	window 			browseFullProtocol 									spreadsheetIcon)
		#(90 		'-----')
		#(100		'fileOut' 							nil 	model 			fileOutMessage 									fileOutIcon)
		#(110		'run test (t)' 							$t 	model 			runMethodTest 									weatherFewCloudsIcon)
		#(120		'debug test (r)' 							$r 	model 			debugMethodTest 									weatherFewCloudsIcon)
		#(130		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 									exploreIcon)
		#(140 		'-----')
		#(160		'senders of... (n)' 							nil 	window 			browseSendersOfMessages 									mailForwardIcon)
		#(161		nil 							$n 	window 			browseSenders)
		#(170		'implementors of... (m)' 							nil 	window 			browseMessages 									developmentIcon)
		#(171		nil 							$m 	window 			browseImplementors)
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
		#(290		'more' 							nil 	nil 			2000 									listAddIcon)

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
		#(2100 		'remove from this browser' 								nil 	model 			removeMessageFromBrowser 									listRemoveIcon)
		#(2110 		'filter message list...' 								nil 	window 			filterMessageList 									findIcon)
		#(2120		'change category...' 								nil 	model 			changeCategory 									saveAsIcon)
		#(2130		'change sets with this method' 								nil 	window 			findMethodInChangeSets 									changesIcon)
		#(2140		'revert to previous version' 								nil 	model 			revertToPreviousVersion 									undoIcon)
		#(2150 		'-----')
		#(2160 		'sort by class hierarchy' 								nil 	model 			sortByClassHierarchy 									classIcon)
		#(2170 		'sort by class name' 								nil 	model 			sortByClassName 									classIcon)
		#(2180 		'sort by selector' 								nil 	model 			sortBySelector 									scriptIcon)
		#(2190 		'sort by date' 								nil 	model 			sortByDate 									dateIcon)
		#(2200 		'reverse sort' 								nil 	model 			sortReverse 									redoIcon)
	).
! !


!ProtocolBrowser reorganize!
('accessing' labelString selectedMessageName:)
('private' categoriesList categoriesListIndex categoriesListIndex: hierarchyForClass: initListFrom:highlighting: messageList on: protocolFor: selectedCategory selectiveClassList selectiveClassListIndex selectiveClassListIndex: updateCategoriesList)
('initialization' initialize)
!


!MethodSetWindow reorganize!
('GUI building' buildLowerPanes buildMorphicMessageList buildMorphicWindow createCodePaneMorph windowColor)
('menu commands' filterMessageList)
('updating' disableCodePaneEditing editSelectionChanged enableCodePaneEditing update:)
('menu & kbd shortcuts' messageListMenuSpec)
!


!ProtocolBrowserWindow reorganize!
('GUI building' buildCategoriesList buildMorphicWindow buildSelectiveClassList)
('menu building' protocolClassListMenu)
('keyboard shortcuts' protocolClassListKey:from:)
('menu commands' browseCategoryOfCurrentMethod)
('menu & kbd shortcuts' messageListMenuSpec)
!

