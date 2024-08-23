'From Cuis7.1 [latest update: #6624] on 23 August 2024 at 4:34:05 pm'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 8/23/2024 16:32:27'!
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
		#(90 		'-----')
		#(100		'fileOut' 							nil 	model 			fileOutMessage 									fileOutIcon)
		#(110		'run test (t)' 							$t 	model 			runMethodTest 									weatherFewCloudsIcon)
		#(120		'debug test (r)' 							$r 	model 			debugMethodTest 									weatherFewCloudsIcon)
		#(130		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 									exploreIcon)
		#(140 		'-----')
		#(150		'refactorings' 							nil 	nil 			1000 									listAddIcon)
		#(160		'senders of... (n)' 							nil 	window 			browseSendersOfMessages 									mailForwardIcon)
		#(170		'implementors of... (m)' 							nil 	window 			browseMessages 									developmentIcon)
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
		#(2110		'select method category (C)' 								$C 	model 			showHomeCategory 									editFindReplaceIcon)
		#(2120		'change sets with this method' 								nil 	window 			findMethodInChangeSets 									changesIcon)
		#(2130		'revert to previous version' 								nil 	model 			revertToPreviousVersion 									undoIcon)
	)! !


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 8/23/2024 16:33:10'!
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
			#(80 		'-----')		
			#(90 		'fileOut (o)' 									$o 	model 			fileOutMessage 								fileOutIcon)
			#(100 		'-----')		
			#(110		'senders of... (n)' 									nil 	window 			browseSendersOfMessages 								mailForwardIcon)
			#(120		'implementors of... (m)' 									nil 	window 			browseMessages 								developmentIcon)
			#(130 		'inheritance (i)' 									$i 	window 			methodInheritance 								goDownIcon)
			#(140 		'versions (v)' 									$v 	window 			browseVersions 								clockIcon)

			"Shortcut only. No menu entry."
			#(991		nil 									$m 	window 			browseImplementors)
			#(992		nil 									$n 	window 			browseSenders)
	).
! !

