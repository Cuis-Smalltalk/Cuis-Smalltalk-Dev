'From Cuis7.1 [latest update: #6779] on 17 October 2024 at 12:14:47 pm'!

!Browser methodsFor: 'class functions' stamp: 'jmv 10/17/2024 12:12:50'!
fileOutClassAlphabetically
	"Print a description of the selected class onto a file whose name is the 
	class name followed by .st.
	Sort methods alphabetically."

	selectedClassName ifNotNil: [ self selectedClass fileOutAlphabetically ]! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 12:09:05'!
fileOutAlphabetically
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is alphabetical, igoring categories."

	DirectoryEntry fileOutsDirectory // (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream timeStamp.
			self sharedPools size > 0 ifTrue: [
				self shouldFileOutPools
					ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
			self fileOutOn: fileStream moveSource: false toFile: 0 initializing: true sortMethodsByCategory: false ]].! !


!Browser methodsFor: 'class functions' stamp: 'jmv 10/17/2024 12:12:57'!
fileOutClass
	"Print a description of the selected class onto a file whose name is the 
	class name followed by .st."

	selectedClassName ifNotNil: [ self selectedClass fileOut ]! !


!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 10/17/2024 12:12:40'!
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
		#(51		'fileOut (alphabetically)' 							nil 	model 			fileOutClassAlphabetically 									fileOutIcon)
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

