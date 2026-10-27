'From Cuis7.9 [latest update: #8290] on 27 October 2026 at 2:08:02 pm'!

!Browser methodsFor: 'message functions' stamp: 'jmv 27/Oct/2026, 5:04:49 pm (UTC)'!
removeSelectedMethod
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected method should be removed from the system. If  so,
	remove it. If the Preference 'confirmMethodRemoves' is set to false,
	the confirmer is bypassed."
	| messageName confirmation |
	selectedMessage ifNil: [ ^self ].
	messageName := self selectedMessageName.
	confirmation := Smalltalk confirmRemovalOf: messageName on: self selectedClassOrMetaClass.
	confirmation = 3
		ifTrue: [^ self].
	self selectedClassOrMetaClass removeSelector: self selectedMessageName.
	self reformulateList.
	self changed: #messageList.
	self setClassOrganizer.
	"In case organization not cached"
	confirmation = 2
		ifTrue: [Smalltalk browseAllCallsOn: messageName]! !

!Browser methodsFor: 'message functions' stamp: 'jmv 27/Oct/2026, 5:05:56 pm (UTC)'!
removeSelectedMethodInHierarchy
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If so,  
	remove it. If the Preference 'confirmMethodRemoves' is set to false, the 
	confirmer is bypassed."
	
	| messageName confirmation selectedClass |
	selectedMessage ifNil: [ ^self ].
	
	messageName := self selectedMessageName.
	selectedClass := self selectedClassOrMetaClass.
	confirmation := Smalltalk confirmRemovalOf: messageName on: selectedClass andSubclasses: true.
	confirmation = 3
		ifTrue: [^ self].

	selectedClass withAllSubclassesDo: [ :eachClass | eachClass removeSelector: messageName ].
	
	self reformulateList.
	self changed: #messageList.
	self setClassOrganizer.
	confirmation = 2
		ifTrue: [Smalltalk browseAllCallsOn: messageName]! !


!CodeFileBrowser methodsFor: 'removing' stamp: 'jmv 27/Oct/2026, 5:04:52 pm (UTC)'!
removeSelectedMethod
	| messageName |
	selectedMessage ifNil: [ ^self ].
	messageName := self selectedMessageName.
	(self selectedClass confirmRemovalOf: messageName)
		ifFalse: [^ false].
	self selectedClassOrMetaClass removeMethod: self selectedMessageName.
	self reformulateList.
	self setClassOrganizer.
	"In case organization not cached"
	self changed: #messageList! !


!MethodSet methodsFor: 'message functions' stamp: 'jmv 27/Oct/2026, 5:07:20 pm (UTC)'!
removeSelectedMethod
	"Remove the selected message from the system. 1/15/96 sw"
	| messageName confirmation |
	selectedMessage ifNil: [ ^self ].
	messageName := self selectedMessageName.
	confirmation := Smalltalk confirmRemovalOf: messageName on: self selectedClassOrMetaClass.
	confirmation = 3
		ifTrue: [^ self].
	self selectedClassOrMetaClass removeSelector: messageName.
	self deleteFromMessageList: self selection.
	self reformulateList.
	confirmation = 2
		ifTrue: [Smalltalk browseAllCallsOn: messageName]! !


!ChangeSorter methodsFor: 'message list' stamp: 'jmv 27/Oct/2026, 5:07:23 pm (UTC)'!
removeSelectedMethod
	"Remove the selected msg from the system. Real work done by the 
	parent, a ChangeSorter"
	| confirmation sel |
	currentSelector
		ifNotNil: [
			confirmation := Smalltalk
				confirmRemovalOf: (sel := self selectedMessageName)
				on: self selectedClassOrMetaClass.
			confirmation = 3
				ifTrue: [^ self].
			self selectedClassOrMetaClass removeSelector: sel.
			self update.
			confirmation = 2
				ifTrue: [Smalltalk browseAllCallsOn: sel]]! !


!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 27/Oct/2026, 5:06:11 pm (UTC)'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message List' 						false)
		#(10	'Browse Full (b)' 							$b 	window 	browseMethodFull 							editFindReplaceIcon)
		#(20	'Browse Hierarchy (h)' 					$h 	window 	browseHierarchy 							goTopIcon)
		#(30	'Browse Method (O)' 						$O 	window 	openSingleMessageBrowser 				scriptIcon)
		#(35	'Browse Package (p)' 						$p 	window 	browsePackage 							packageIcon)
		#(40 	'-----')
		#(50	'File Out (o)' 								$o 	model 		fileOutMethod 								fileOutIcon)
		#(60	'Run Test (t)' 							$t 	model 		runMethodTest 							weatherFewCloudsIcon)
		#(70	'Debug Test (r)' 							$r 	model 		debugMethodTest 							weatherFewCloudsIcon)
		#(80	'Inspect CompiledMethod' 				nil 	model 		inspectCompiledMethod 					exploreIcon)
		#(90 	'-----')
		#(100	'Refactorings' 							nil 	nil 			1000 										listAddIcon)
		#(110	'Senders of... (n)' 						nil 	window 	browseSendersOfMessages 				mailForwardIcon)
		#(120	'Implementors of... (m)' 					nil 	window 	browseImplementorsOfMessages 			developmentIcon)
		#(130	'Inheritance (i)' 							$i 	window 	methodInheritance 						goDownIcon)
		#(140	'Versions (v)' 							$v 	window 	browseVersions 							clockIcon)
		#(150 	'-----')
		#(160	'Remove Method (x)' 						$x 	model 		removeSelectedMethod 					deleteIcon)
		#(170	'Remove Method in Hierarchy' 			nil 	model 		removeSelectedMethodInHierarchy 		deleteIcon)
		#(180	'More...' 									nil 	nil 			2000 										listAddIcon)

		"Keyboard shortcut only. No menu entry."
		#(991	nil 										$m 	window 	browseImplementors)
		#(992	nil 										$n 	window 	browseSenders)
		#(993	nil 										$c 	model 		copySelector)

		" 'refactorings' SubMenu"
		#(1010	'Rename... (R)' 							$R 	model 		renameSelector 							saveAsIcon)
		#(1020	'Change keyword Order...' 				nil 	model 		changeKeywordOrder 						sendReceiveIcon)
		#(1030	'Add Parameter... (A)' 					$A 	model 		addParameter 								listAddIcon)
		#(1040	'Remove Parameter... (S)' 				$S 	model 		removeParameter 							listRemoveIcon)
		#(1050 	'-----')
		#(1060	'Inline Method... (F)' 						$F 	model 		inlineMethod 								saveAsIcon)
		#(1070	'Move to Instance/Class Method' 			nil 	model 		moveToInstanceOrClassMethod 			changesIcon)
		#(1080	'Push Up' 								nil 	model 		pushUpSelector 							goTopIcon)
		#(1090	'Add in Superclass as subclassResponsibility' nil model 		addAsSubclassResponsibilitySelector 		goTopIcon)
		#(1100	'Push Down to Subclasses' 				nil 	model 		pushDownSelectorToSubclasses 			goBottomIcon)
		#(1110	'Push Down to One Subclass' 				nil 	model 		pushDownSelectorToOneSubclass 			goBottomIcon)

		" 'more' SubMenu"
		#(2010	'Local Senders of...' 						nil 	window 	browseLocalSendersOfMessages 			mailForwardIcon)
		#(2020	'Local Implementors of...' 					nil 	window 	browseLocalImplementorsOfMessages 	classIcon)
		#(2030 	'-----')
		#(2040	'Sample Instance' 						nil 	window 	makeSampleInstance 						instanceIcon)
		#(2050	'Inspect Instances' 						nil 	window 	inspectInstances 							inspectIcon)
		#(2060	'Inspect Subinstances' 					nil 	window 	inspectSubInstances 						inspectIcon)
		#(2070 	'-----')
		#(2080	'Change Category...' 						nil 	model 		changeCategory 							saveAsIcon)
		#(2090	'Select method Category (C)' 				$C 	model 		showHomeCategory 						editFindReplaceIcon)
		#(2100	'Change sets with this Method' 			nil 	window 	findMethodInChangeSets 					changesIcon)
		#(2110	'Revert to previous Version' 				nil 	model 		revertToPreviousVersion 					undoIcon)
	)! !


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 27/Oct/2026, 5:06:46 pm (UTC)'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^ model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Messages' 				false)
				#(10	'File In' 					nil 	model 		fileInMessage 				updateIcon)
				#(20	'File Out' 				nil 	model 		fileOutMethod 				fileOutIcon)
				#(30 	'-----')
				#(40	'Senders (n)' 			$n 	window 	browseSenders 			mailForwardIcon)
				#(50	'Implementors (m)' 		$m 	window 	browseImplementors 		developmentIcon)
				#(60	'Method Inheritance (h)' 	$h 	window 	methodInheritance 		goDownIcon)
				#(70	'Browse Full (b)' 			$b 	window 	browseMethodFull 			editFindReplaceIcon)
				#(80	'Versions (v)' 			nil 	window 	browseVersions 			clockIcon)
				#(90 	'-----')
				#(100	'Remove Method (x)' 		$x 	model 		removeSelectedMethod 	deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Messages' 				false)
				#(10	'File Out' 				nil 	model 		fileOutMethod 				fileOutIcon)
				#(20 	'-----')
				#(30	'Remove Method (x)' 		$x 	model 		removeSelectedMethod 	deleteIcon)
			)].! !


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 27/Oct/2026, 5:06:53 pm (UTC)'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the message list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Message List' 						true)
			#(10 	'Delete method from Change Set (d)' 	$d 	model 		forget 								warningIcon)
			#(20 	'Remove method from System (x)' 	$x 	model 		removeSelectedMethod 			deleteIcon)
			#(30 	'-----')
			#(40 	'Browse Full (b)' 						$b 	window 	browseMethodFull 					editFindReplaceIcon)
			#(50 	'Browse Hierarchy (h)' 				$h 	window 	browseHierarchy 					goTopIcon)
			#(60 	'Browse Method (O)' 					$O 	window 	openSingleMessageBrowser 		scriptIcon)
			#(80 	'-----')
			#(90 	'File Out (o)' 							$o 	model 		fileOutMethod 						fileOutIcon)
			#(100 	'-----')		
			#(110	'Senders of... (n)' 					nil 	window 	browseSendersOfMessages 		mailForwardIcon)
			#(120	'Implementors of... (m)' 				nil 	window 	browseImplementorsOfMessages 	developmentIcon)
			#(130 	'Inheritance (i)' 						$i 	window 	methodInheritance 				goDownIcon)
			#(140 	'Versions (v)' 						$v 	window 	browseVersions 					clockIcon)

			"Keyboard shortcut only. No menu entry."
			#(991	nil 									$m 	window 	browseImplementors)
			#(992	nil 									$n 	window 	browseSenders)
	).
! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 27/Oct/2026, 5:06:59 pm (UTC)'!
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
		#(60	'File Out' 					nil 	model 	fileOutMethod 								fileOutIcon)
		#(61	'File Out All' 					nil 	model 	fileOutAllMethods 							fileOutIcon)
		#(70	'Run Test (t)' 				$t 	model 	runMethodTest 							weatherFewCloudsIcon)
		#(80	'Debug Test (r)' 				$r 	model 	debugMethodTest 							weatherFewCloudsIcon)
		#(90	'Inspect CompiledMethod' 	nil 	model 	inspectCompiledMethod 					exploreIcon)
		#(100 	'-----')
		#(103	'Refactorings' 				nil 	nil 		1000 										listAddIcon)
		#(110	'Senders of... (n)' 			nil 	window browseSendersOfMessages 				mailForwardIcon)
		#(120	'Local Senders of...' 			nil 	window browseLocalSendersOfMessages 			mailForwardIcon)
		#(130	'Implementors of... (m)' 		nil 	window browseImplementorsOfMessages 			developmentIcon)
		#(140	'Local Implementors of...' 		nil 	window browseLocalImplementorsOfMessages 	developmentIcon)
		#(150	'Inheritance (i)' 				$i 	window methodInheritance 						goDownIcon)
		#(160	'Versions (v)' 				$v 	window browseVersions 							clockIcon)
		#(170 	'-----')
		#(180	'Remove Method (x)' 			$x 	model 	removeSelectedMethod 					deleteIcon)
		#(190	'more' 						nil 	nil 		2000 										listAddIcon)

		"Keyboard shortcut only. No menu entry."
		#(991	nil 							$m 	window browseImplementors)
		#(992	nil 							$n 	window browseSenders)
		#(993	nil 							$c 	model 	copySelector)
		#(994	nil 							$N 	window browseClassRefs)

		" 'Refactoring' SubMenu"
		#(1010	'Rename... (R)' 							$R 	model 		renameSelector 							saveAsIcon)
		#(1020	'Change keyword Order...' 				nil 	model 		changeKeywordOrder 						sendReceiveIcon)
		#(1030	'Add Parameter... (A)' 					$A 	model 		addParameter 								listAddIcon)
		#(1040	'Remove Parameter... (S)' 				$S 	model 		removeParameter 							listRemoveIcon)
		#(1050 	'-----')
		#(1060	'Inline Method... (F)' 						$F 	model 		inlineMethod 								saveAsIcon)
		#(1070	'Move to Instance/Class Method' 			nil 	model 		moveToInstanceOrClassMethod 			changesIcon)
		#(1080	'Push Up' 								nil 	model 		pushUpSelector 							goTopIcon)
		#(1090	'Add in Superclass as subclassResponsibility' nil model 		addAsSubclassResponsibilitySelector 		goTopIcon)
		#(1100	'Push Down to Subclasses' 				nil 	model 		pushDownSelectorToSubclasses 			goBottomIcon)
		#(1110	'Push Down to One Subclass' 				nil 	model 		pushDownSelectorToOneSubclass 			goBottomIcon)

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

!methodRemoval: ChangeSorter #removeMessage stamp: 'jmv 27/Oct/2026, 5:07:31 pm (UTC)'!
ChangeSorter removeSelector: #removeMessage!
!methodRemoval: MethodSet #removeMessage stamp: 'jmv 27/Oct/2026, 5:07:31 pm (UTC)'!
MethodSet removeSelector: #removeMessage!
!methodRemoval: CodeFileBrowser #removeMessage stamp: 'jmv 27/Oct/2026, 5:07:30 pm (UTC)'!
CodeFileBrowser removeSelector: #removeMessage!
!methodRemoval: CodeFileBrowser #removeMethod stamp: 'jmv 27/Oct/2026, 5:04:56 pm (UTC)'!
CodeFileBrowser removeSelector: #removeMethod!
!methodRemoval: Browser #removeMessageInHierarchy stamp: 'jmv 27/Oct/2026, 5:06:15 pm (UTC)'!
Browser removeSelector: #removeMessageInHierarchy!
!methodRemoval: Browser #removeMethod stamp: 'jmv 27/Oct/2026, 5:04:54 pm (UTC)'!
Browser removeSelector: #removeMethod!
!methodRemoval: Browser #removeSelectedMethodnHierarchy stamp: 'jmv 27/Oct/2026, 5:06:00 pm (UTC)'!
Browser removeSelector: #removeSelectedMethodnHierarchy!
!methodRemoval: Browser #removeMessage stamp: 'jmv 27/Oct/2026, 5:07:30 pm (UTC)'!
Browser removeSelector: #removeMessage!
