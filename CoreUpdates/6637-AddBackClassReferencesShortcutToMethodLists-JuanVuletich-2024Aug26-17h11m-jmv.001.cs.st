'From Cuis7.1 [latest update: #6636] on 26 August 2024 at 5:11:31 pm'!

!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 8/26/2024 17:07:30'!
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
		#(994		nil 							$N 	window 			browseClassRefs)

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

