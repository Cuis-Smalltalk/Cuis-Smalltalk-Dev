'From Cuis7.3 [latest update: #6996] on 22 January 2025 at 8:32:52 am'!

!MethodSet methodsFor: 'message functions' stamp: 'jmv 1/22/2025 08:31:36'!
fileOutAllMethods
	"Put a description of the selected message on a file"

	DirectoryEntry fileOutsDirQuery: 'methods.st' writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self messageList do: [ :methodRef |
			methodRef actualClass
				printMethodChunk: methodRef methodSymbol
				withPreamble: true
				on: fileStream
				moveSource: false
				toFile: 0 ]].! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 1/22/2025 08:29:35'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Set' 				false)
		#(10	'Browse Full (b)' 				$b 	window browseMethodFull 						editFindReplaceIcon)
		#(20	'Browse Hierarchy (h)' 		$h 	window browseHierarchy 							goTopIcon)
		#(30	'Browse Method (O)' 			$O 	window openSingleMessageBrowser 				scriptIcon)
		#(40	'Browse Protocol (p)' 			$p 	window browseFullProtocol 						spreadsheetIcon)
		#(45	'Browse Package' 			nil 	window browsePackage 							packageIcon)
		#(50 	'-----')
		#(60	'File Out' 					nil 	model 	fileOutMessage 							fileOutIcon)
		#(61	'File Out All' 					nil 	model 	fileOutAllMethods 							fileOutIcon)
		#(70	'Run Test (t)' 				$t 	model 	runMethodTest 							weatherFewCloudsIcon)
		#(80	'Debug Test (r)' 				$r 	model 	debugMethodTest 							weatherFewCloudsIcon)
		#(90	'Inspect CompiledMethod' 	nil 	model 	inspectCompiledMethod 					exploreIcon)
		#(100 	'-----')
		#(110	'Senders of... (n)' 			nil 	window browseSendersOfMessages 				mailForwardIcon)
		#(120	'Local Senders of...' 			nil 	window browseLocalSendersOfMessages 			mailForwardIcon)
		#(130	'Implementors of... (m)' 		nil 	window browseImplementorsOfMessages 			developmentIcon)
		#(140	'Local Implementors of...' 		nil 	window browseLocalImplementorsOfMessages 	developmentIcon)
		#(150	'Inheritance (i)' 				$i 	window methodInheritance 						goDownIcon)
		#(160	'Versions (v)' 				$v 	window browseVersions 							clockIcon)
		#(170 	'-----')
		#(180	'Remove Method (x)' 			$x 	model 	removeMessage 							deleteIcon)
		#(190	'more' 						nil 	nil 		2000 										listAddIcon)

		"Shortcut only. No menu entry."
		#(991	nil 							$m 	window browseImplementors)
		#(992	nil 							$n 	window browseSenders)
		#(993	nil 							$c 	model 	copySelector)
		#(994	nil 							$N 	window browseClassRefs)

		" 'more' SubMenu"
		#(2010	'Sample instance' 			nil 	window makeSampleInstance 						instanceIcon)
		#(2020	'Inspect instances' 			nil 	window inspectInstances 							inspectIcon)
		#(2030	'Inspect subinstances' 		nil 	window inspectSubInstances 						inspectIcon)
		#(2040 	'-----')
		#(2050 	'Remove from this Browser' 	nil 	model 	removeMessageFromBrowser 				listRemoveIcon)
		#(2060 	'Filter message list...' 			nil 	window filterMessageList 							findIcon)
		#(2070	'Change category...' 			nil 	model 	changeCategory 							saveAsIcon)
		#(2080	'Change sets with this Method' nil 	window findMethodInChangeSets 					changesIcon)
		#(2090	'Revert to previous Version' 	nil 	model 	revertToPreviousVersion 					undoIcon)
		#(2100 	'-----')
		#(2110 	'Sort by class Hierarchy' 		nil 	model 	sortByClassHierarchy 						classIcon)
		#(2120 	'Sort by class Name' 			nil 	model 	sortByClassName 							classIcon)
		#(2130 	'Sort by Selector' 				nil 	model 	sortBySelector 								scriptIcon)
		#(2140 	'Sort by Date' 				nil 	model 	sortByDate 								dateIcon)
		#(2150 	'Reverse sort' 				nil 	model 	sortReverse 								redoIcon)
	).
! !

