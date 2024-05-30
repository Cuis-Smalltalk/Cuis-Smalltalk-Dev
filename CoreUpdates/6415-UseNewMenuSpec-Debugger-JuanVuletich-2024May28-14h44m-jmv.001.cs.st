'From Cuis7.1 [latest update: #6414] on 28 May 2024 at 4:02:03 pm'!

!DebuggerWindow methodsFor: 'menu commands' stamp: 'jmv 5/28/2024 15:58:43'!
contextClassBrowse

	BrowserWindow
		fullOnClass: model contextVariablesInspector selectedClassOrMetaClass! !

!DebuggerWindow methodsFor: 'menu commands' stamp: 'jmv 5/28/2024 15:57:09'!
contextClassProtocol

	ProtocolBrowserWindow
		openFullProtocolForClass: model contextVariablesInspector selectedClassOrMetaClass! !

!DebuggerWindow methodsFor: 'menu commands' stamp: 'jmv 5/28/2024 15:53:36'!
inspectContextSelection

	model contextVariablesInspector selectionIndex = 0 ifTrue: [^ self flash].
	^ model contextVariablesInspector selection inspect! !

!DebuggerWindow methodsFor: 'menu commands' stamp: 'jmv 5/28/2024 15:53:44'!
inspectReceiverSelection
	model receiverInspector selectionIndex = 0 ifTrue: [^ self flash].
	^ model receiverInspector  selection inspect! !

!DebuggerWindow methodsFor: 'menu commands' stamp: 'jmv 5/28/2024 15:58:37'!
receiverClassBrowse

	BrowserWindow
		fullOnClass: model receiverInspector selectedClassOrMetaClass! !

!DebuggerWindow methodsFor: 'menu commands' stamp: 'jmv 5/28/2024 15:57:21'!
receiverClassProtocol

	ProtocolBrowserWindow
		openFullProtocolForClass: model receiverInspector selectedClassOrMetaClass! !

!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/28/2024 15:13:26'!
contextStackMenuSpec
	"Spec for the menu and shortcut keys for a class comment version in the versions list.
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
		#(10		'fullStack (f)' 						$f 	window 			fullStack 						nil 	nil 	'show the complete call stack')
		#(20		'proceed (p)' 						$p 	window 			proceed 						nil 	nil 	'close the debugger and proceed')
		#(30		'restart (r)' 						$r 	window 			restart 						nil 	nil 	'reset this context to its start')
		#(40		'step into send (e)' 						$e 	window 			send 						nil 	nil 	'step into message sends')
		#(50		'step over send (t)' 						$t 	window 			doStep 						nil 	nil 	'step over message sends')
		#(60		'step into block (T)' 						$T 	window 			stepIntoBlock 						nil 	nil 	'step into a code block')
		#(70		'run to cursor' 						nil 	window 			runToCursor 						nil 	nil 	'run to cursor')
		#(80		'where (w)' 						$w 	window 			where 						nil 	nil 	'select current pc range')
		#(90		'peel to first like this' 						nil 	window 			peelToFirst 						nil 	nil 	'peel stack back to the second occurance of selected message')
		#(100 		'-----')
		#(110		'return entered value' 						nil 	window 			returnValue 						nil 	nil 	'lets you enter an expression and return method answering result')
		#(120		'toggle break on entry' 						nil 	model 			toggleBreakOnEntry 						nil 	nil 	'install or uninstall a breakpoint on method entry')
	)! !

!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/28/2024 15:56:01'!
contextVariablesListMenuSpec
	"Spec for the menu and shortcut keys for the list of variables in this context.
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
		#(10		'inspect (i)' 						$i 	window 			inspectContextSelection 							nil 	inspectIcon)
		#(20		'explore (I)' 						$I 	window 			exploreContextSelection 							nil 	exploreIcon)
		#(30 		'-----')
		#(40		'browser full (b)' 						$b 	window 			contextClassBrowse 							nil 	editFindReplaceIcon)
		#(50		'browser hierarch (h)' 						$h 	window 			contextClassHierarchy 							nil 	goTopIcon)
		#(60		'browser protocol (p)' 						$p 	window 			contextClassProtocol 							nil 	spreadsheetIcon)
	)! !

!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/28/2024 15:55:58'!
receiverVariablesListMenuSpec
	"Spec for the menu and shortcut keys for the list of instance variables in the receiver.
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
		#(10		'inspect (i)' 						$i 	window 			inspectReceiverSelection 							nil 	inspectIcon)
		#(20		'explore (I)' 						$I 	window 			exploreReceiverSelection 							nil 	exploreIcon)
		#(30 		'-----')
		#(40		'browser full (b)' 						$b 	window 			receiverClassBrowse 							nil 	editFindReplaceIcon)
		#(50		'browser hierarch (h)' 						$h 	window 			receiverClassHierarchy 							nil 	goTopIcon)
		#(60		'browser protocol (p)' 						$p 	window 			receiverClassProtocol 							nil 	spreadsheetIcon)
	)! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 5/28/2024 15:59:15'!
contextVariableInspector

	^ contextVariableInspector ifNil: [
		contextVariableInspector := (PluggableListMorph
			withModel: model contextVariablesInspector 
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:)
				menuNshortcutsKey: #contextVariablesListMenuSpec ].		! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 5/28/2024 15:59:10'!
receiverInspector

	^ receiverInspector ifNil: [
		receiverInspector := (PluggableListMorph
			withModel: model receiverInspector
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:)
				menuNshortcutsKey: #receiverVariablesListMenuSpec ].! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 5/28/2024 15:15:17'!
stackList

	^ stackList ifNil: [
		stackList := (PluggableListMorph
			withModel: model 
			listGetter: #contextStackList
			indexGetter: #contextStackIndex
			indexSetter: #toggleContextStackIndex:)
				menuNshortcutsKey: #contextStackMenuSpec ].
! !

!methodRemoval: DebuggerWindow #exploreSelectionIn: stamp: 'jmv 5/28/2024 16:00:36'!
DebuggerWindow removeSelector: #exploreSelectionIn:!
!methodRemoval: DebuggerWindow #browseClassRefsIn: stamp: 'jmv 5/28/2024 16:00:39'!
DebuggerWindow removeSelector: #browseClassRefsIn:!
!methodRemoval: DebuggerWindow #browseHierarchyIn: stamp: 'jmv 5/28/2024 16:00:38'!
DebuggerWindow removeSelector: #browseHierarchyIn:!
!methodRemoval: DebuggerWindow #inspectorKey:from: stamp: 'jmv 5/28/2024 15:59:35'!
DebuggerWindow removeSelector: #inspectorKey:from:!
!methodRemoval: DebuggerWindow #contextStackKey:from: stamp: 'jmv 5/28/2024 15:14:50'!
DebuggerWindow removeSelector: #contextStackKey:from:!
!methodRemoval: DebuggerWindow #contextFieldListMenu stamp: 'jmv 5/28/2024 15:59:19'!
DebuggerWindow removeSelector: #contextFieldListMenu!
!methodRemoval: DebuggerWindow #receiverFieldListMenu stamp: 'jmv 5/28/2024 15:59:19'!
DebuggerWindow removeSelector: #receiverFieldListMenu!
!methodRemoval: DebuggerWindow #browseMethodFullIn: stamp: 'jmv 5/28/2024 16:00:37'!
DebuggerWindow removeSelector: #browseMethodFullIn:!
!methodRemoval: DebuggerWindow #contextStackMenu stamp: 'jmv 5/28/2024 15:14:58'!
DebuggerWindow removeSelector: #contextStackMenu!
!methodRemoval: DebuggerWindow #inspectSelectionIn: stamp: 'jmv 5/28/2024 16:00:34'!
DebuggerWindow removeSelector: #inspectSelectionIn:!
!methodRemoval: DebuggerWindow #browseFullProtocolIn: stamp: 'jmv 5/28/2024 16:00:38'!
DebuggerWindow removeSelector: #browseFullProtocolIn:!

!DebuggerWindow reorganize!
('GUI building' buildLowerPanes buildMorphicWindow contextVariableInspector contextVariableInspectorText createCodePaneMorph customButtonRow customButtonSpecs initialExtent optionalButtonTuples receiverInspector receiverInspectorText stackList windowColor)
('menu commands' contextClassBrowse contextClassHierarchy contextClassProtocol exploreContextSelection exploreReceiverSelection inspectContextSelection inspectReceiverSelection receiverClassBrowse receiverClassHierarchy receiverClassProtocol returnValue shiftClickPressedOn:at:)
('open/close' closeView delete)
('user interface' ifOkToChangeCodePaneDo: okToChangeCodePane okToChangeContextVariableInspectorText okToChangeDueTo: okToChangeReceiverInspectorText)
('context stack menu' createMethod doStep fullStack peelToFirst proceed restart runToCursor send stepIntoBlock where)
('notification actions' registerNotificationActions)
('menu & kbd shortcuts' contextStackMenuSpec contextVariablesListMenuSpec receiverVariablesListMenuSpec)
!

