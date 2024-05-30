'From Cuis7.1 [latest update: #6414] on 28 May 2024 at 4:38:57 pm'!

!Inspector methodsFor: 'user interface support' stamp: 'jmv 5/28/2024 16:12:30'!
objectSpecificMenuSpecFrom: aWindow

	^nil! !


!DictionaryInspector methodsFor: 'user interface support' stamp: 'jmv 5/28/2024 16:13:06'!
objectSpecificMenuSpecFrom: aWindow

	^aWindow menuSpecForDictionary! !


!SetInspector methodsFor: 'user interface support' stamp: 'jmv 5/28/2024 16:13:26'!
objectSpecificMenuSpecFrom: aWindow

	^aWindow menuSpecForSet! !


!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/28/2024 16:33:29'!
fieldListMenuSpec
	"Spec for the menu and shortcut keys for the list of variables in our object.
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
	| answer |
	answer := #(
		#(10		'inspect (i)' 						$i 	window 			inspectSelection 							nil 	inspectIcon)
		#(20		'explore (I)' 						$I 	window 			exploreSelection 							nil 	exploreIcon)
		#(30		'copy to clipboard (c)' 						$c 	window 			copySelectionToClipboard 							nil 	copyIcon)
		#(40		'basic inspect' 						nil 	window 			inspectBasic 							nil 	inspectIcon)
		#(50		'references finder' 						nil 	window 			openReferencesFinder 							nil 	exploreIcon)
		#(60		'weight explorer' 						nil 	window 			openWeightExplorer 							nil 	exploreIcon)
	).
	(self model objectSpecificMenuSpecFrom: self) ifNotNil: [ :spec |
		answer := answer, spec ].
	answer := answer, #(
		#(210 		'-----')
		#(220		'browser full (b)' 						$b 	window 			browseMethodFull 							nil 	editFindReplaceIcon)
		#(230		'browser hierarch (h)' 						$h 	window 			browseHierarchy 							nil 	goTopIcon)
		#(240		'browser protocol (p)' 						$p 	window 			browseFullProtocol 							nil 	spreadsheetIcon)
	).
	^answer! !

!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/28/2024 16:19:55'!
menuSpecForDictionary
	"Spec for the menu and shortcut keys for the list of variables in our object.
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
		#(100 		'-----')
		#(110		'inspect key' 					nil 	window 			inspectSelectedKey 							nil 	findIcon)
		#(120		'senders of this key' 					nil 	window 			sendersOfSelectedKey 							nil 	mailForwardIcon)
		#(130		'add key' 					nil 	window 			addEntry 							nil 	listAddIcon)
		#(140		'rename key' 					nil 	window 			renameEntry 							nil 	saveAsIcon)
		#(150		'remove' 					nil 	model 			removeSelection 							nil 	listRemoveIcon)
	)! !

!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/28/2024 16:17:59'!
menuSpecForSet
	"Spec for the menu and shortcut keys for the list of variables in our object.
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
		#(100 		'-----')
		#(110		'remove' 			nil 	model 			removeSelection 					nil 	listRemoveIcon)
	)! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 5/28/2024 16:32:31'!
buildList

	| list |
	
	list := (PluggableListMorph
			withModel: model 
			listGetter: #fieldList
			indexGetter: #selectionIndex
			indexSetter: #toggleIndex:)
				menuNshortcutsKey: #fieldListMenuSpec.
	list doubleClickSelector: #inspectSelection.
	^list! !

!methodRemoval: InspectorWindow #basicMenuOptions stamp: 'jmv 5/28/2024 16:36:30'!
InspectorWindow removeSelector: #basicMenuOptions!
!methodRemoval: InspectorWindow #fieldListMenu stamp: 'jmv 5/28/2024 16:36:20'!
InspectorWindow removeSelector: #fieldListMenu!
!methodRemoval: InspectorWindow #menuOptionsForBrowsing stamp: 'jmv 5/28/2024 16:36:33'!
InspectorWindow removeSelector: #menuOptionsForBrowsing!
!methodRemoval: InspectorWindow #inspectorKey:from: stamp: 'jmv 5/28/2024 16:35:19'!
InspectorWindow removeSelector: #inspectorKey:from:!
!methodRemoval: InspectorWindow #browseClassRefs stamp: 'jmv 5/28/2024 16:35:34'!
InspectorWindow removeSelector: #browseClassRefs!
!methodRemoval: InspectorWindow #menuOptionsForDictionary stamp: 'jmv 5/28/2024 16:36:53'!
InspectorWindow removeSelector: #menuOptionsForDictionary!
!methodRemoval: InspectorWindow #menuOptionsForSet stamp: 'jmv 5/28/2024 16:36:54'!
InspectorWindow removeSelector: #menuOptionsForSet!
!methodRemoval: SetInspector #suggestObjectSpecificMenuItemsFor:from: stamp: 'jmv 5/28/2024 16:36:47'!
SetInspector removeSelector: #suggestObjectSpecificMenuItemsFor:from:!
!methodRemoval: DictionaryInspector #suggestObjectSpecificMenuItemsFor:from: stamp: 'jmv 5/28/2024 16:36:48'!
DictionaryInspector removeSelector: #suggestObjectSpecificMenuItemsFor:from:!
!methodRemoval: Inspector #suggestObjectSpecificMenuItemsFor:from: stamp: 'jmv 5/28/2024 16:38:15'!
Inspector removeSelector: #suggestObjectSpecificMenuItemsFor:from:!

!InspectorWindow reorganize!
('GUI building' buildContentsText buildEvaluatorText buildList buildMorphicWindow initialExtent)
('menu commands' addEntry browseFullProtocol browseHierarchy browseMethodFull copySelectionToClipboard exploreSelection inspectBasic inspectSelectedKey inspectSelection openReferencesFinder openWeightExplorer renameEntry sendersOfSelectedKey)
('menu & kbd shortcuts' fieldListMenuSpec menuSpecForDictionary menuSpecForSet)
!

