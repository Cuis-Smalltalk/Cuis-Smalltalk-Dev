'From Cuis7.1 [latest update: #6436] on 4 June 2024 at 11:12:16 am'!

!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 11:03:47'!
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
		#(161		nil 							$n 	window 			browseSenders 									mailForwardIcon)
		#(170		'implementors of... (m)' 							nil 	window 			browseMessages 									developmentIcon)
		#(171		nil 							$m 	window 			browseImplementors 									developmentIcon)
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


!ProtocolBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 10:43:36'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^ super messageListMenuSpec, #(
		#(80		'browse method category (c)' 							$c 	window 			browseCategoryOfCurrentMethod 									editFindReplaceIcon)
		#(2100 		'XXremove from this browser')
		#(2110 		'XXfilter message list...'))! !


!MethodSetWindow methodsFor: 'GUI building' stamp: 'jmv 6/4/2024 10:35:53'!
buildMorphicMessageList
	"Build a morphic message list, with #messageList as its list-getter"

	^(PluggableListMorph
		withModel: model
		listGetter: #messageList
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:)
			menuNshortcutsKey: #messageListMenuSpec.! !

!methodRemoval: MethodSetWindow class #messageSetMenuOptions stamp: 'jmv 6/4/2024 10:58:33'!
MethodSetWindow class removeSelector: #messageSetMenuOptions!
!methodRemoval: MethodSetWindow class #messageSetMenu2Options stamp: 'jmv 6/4/2024 10:58:33'!
MethodSetWindow class removeSelector: #messageSetMenu2Options!
!methodRemoval: MethodSetWindow #openMessageListMenu stamp: 'jmv 6/4/2024 10:58:33'!
MethodSetWindow removeSelector: #openMessageListMenu!
!methodRemoval: MethodSetWindow #browseCategoryOfCurrentMethod stamp: 'jmv 6/4/2024 10:58:33'!
MethodSetWindow removeSelector: #browseCategoryOfCurrentMethod!
!methodRemoval: MethodSetWindow #messageListMenu2 stamp: 'jmv 6/4/2024 10:58:33'!
MethodSetWindow removeSelector: #messageListMenu2!
!methodRemoval: MethodSetWindow #messageListKey:from: stamp: 'jmv 6/4/2024 10:58:33'!
MethodSetWindow removeSelector: #messageListKey:from:!
!methodRemoval: MethodSetWindow #messageListMenu stamp: 'jmv 6/4/2024 10:58:33'!
MethodSetWindow removeSelector: #messageListMenu!
!methodRemoval: MethodSetWindow #addExtraMenu2ItemsTo: stamp: 'jmv 6/4/2024 10:58:33'!
MethodSetWindow removeSelector: #addExtraMenu2ItemsTo:!
!methodRemoval: MethodSetWindow #openMessageListMenu2 stamp: 'jmv 6/4/2024 10:58:33'!
MethodSetWindow removeSelector: #openMessageListMenu2!
!methodRemoval: ProtocolBrowser #growable stamp: 'jmv 6/4/2024 10:58:33'!
ProtocolBrowser removeSelector: #growable!
!methodRemoval: MethodSet #growable stamp: 'jmv 6/4/2024 10:58:33'!
MethodSet removeSelector: #growable!
