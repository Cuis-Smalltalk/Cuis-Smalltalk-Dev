'From Cuis7.1 [latest update: #6414] on 29 May 2024 at 11:07:25 am'!

!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 5/29/2024 10:56:12'!
isMonitoring
	^monitorList isEmptyOrNil not! !


!ObjectExplorerWindow methodsFor: 'menu commands' stamp: 'jmv 5/29/2024 10:50:22'!
addHaloToSelection
	model getCurrentSelection item addHalo! !

!ObjectExplorerWindow methodsFor: 'menu commands' stamp: 'jmv 5/29/2024 10:54:57'!
monitorChanges
	"Start stepping and watching the given wrapper for changes."

	model getCurrentSelection ifNotNil: [ :wrapper |
		model monitorList at: wrapper put: wrapper asString.
		self startSteppingStepTime: 200 ]! !

!ObjectExplorerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/29/2024 11:05:49'!
listMenuSpec
	"Spec for the menu and shortcut keys for our tree nodes.
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
	model getCurrentSelection
		ifNil: [^#()]
		ifNotNil: [ :currSel |
			answer := #(
				#(10		'inspect (i)' 						$i 	window 			inspectSelection 							nil 	inspectIcon)
				#(20		'explore (I)' 						$I 	window 			exploreSelection 							nil 	exploreIcon)
				#(30		'copy to clipboard (c)' 						$c 	window 			copySelectionToClipboard 							nil 	copyIcon)
				#(40		'basic inspect' 						nil 	window 			inspectBasic 							nil 	inspectIcon)
				#(50		'references finder' 						nil 	window 			openReferencesFinder 							nil 	exploreIcon)
				#(60		'weight explorer' 						nil 	window 			openWeightExplorer 							nil 	exploreIcon)
				#(70 		'-----')
				#(80		'browser full (b)' 						$b 	window 			browseMethodFull 							nil 	editFindReplaceIcon)
				#(90		'browser hierarch (h)' 						$h 	window 			browseHierarchy 							nil 	goTopIcon)
				#(100		'browser protocol (p)' 						$p 	window 			browseFullProtocol 							nil 	spreadsheetIcon)).
			(currSel item is: #Morph) ifTrue: [ answer := answer, #(
				#(110 		'-----')
				#(120		'show morph halo' 						nil 	window 			addHaloToSelection 							nil 	editFindReplaceIcon)) ].
			model class == ReferencesExplorer ifTrue: [ answer := answer, #(
				#(210 		'-----')
				#(220		'rescan' 						nil 	window 			rescan 							nil 	editFindReplaceIcon)) ].
			answer := answer, #(
				#(310 		'-----')
				#(320		'monitor changes' 						nil 	window 			monitorChanges 							nil 	editFindReplaceIcon)).
			model isMonitoring ifTrue: [ answer := answer, #(
				#(410 		'-----')
				#(420		'stop monitoring all' 						nil 	window 			stopMonitoring 							nil 	editFindReplaceIcon)) ].
			^answer ].! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 5/29/2024 11:06:54'!
buildMorphicWindow

	| textMorph |
	listMorph := (HierarchicalListMorph
			withModel: model
			listGetter: #getList
			indexGetter: #getCurrentSelection
			indexSetter: #noteNewSelection:)
				menuNshortcutsKey: #listMenuSpec.
	listMorph autoDeselect: false.
	listMorph doubleClickSelector: #inspectSelection.
	textMorph := ((TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false;
			emptyTextDisplayMessage: 'Smalltalk expression (self is selected item)').
	self layoutMorph
		addMorph: listMorph proportionalHeight: 0.8;
		addAdjusterAndMorph: textMorph proportionalHeight: 0.2.
	self setLabel: model rootObject shortPrintString.
! !

!methodRemoval: ObjectExplorerWindow #genericMenu stamp: 'jmv 5/29/2024 10:57:24'!
ObjectExplorerWindow removeSelector: #genericMenu!
!methodRemoval: ObjectExplorerWindow #explorerKey:from: stamp: 'jmv 5/29/2024 11:06:09'!
ObjectExplorerWindow removeSelector: #explorerKey:from:!
!methodRemoval: ObjectExplorerWindow #monitor: stamp: 'jmv 5/29/2024 10:55:05'!
ObjectExplorerWindow removeSelector: #monitor:!
!methodRemoval: ObjectExplorer #basicMonitorList stamp: 'jmv 5/29/2024 10:57:28'!
ObjectExplorer removeSelector: #basicMonitorList!

!ObjectExplorerWindow reorganize!
('GUI building' buildMorphicWindow expandAll initialExtent)
('menu commands' addHaloToSelection browseClassRefs browseFullProtocol browseHierarchy browseMethodFull copySelectionToClipboard exploreSelection inspectBasic inspectSelection monitorChanges openWeightExplorer stopMonitoring)
('menu commands - reference finder' objectsToSkipDuringRescan openReferencesFinder rescan)
('stepping' stepAt:)
('menu & kbd shortcuts' listMenuSpec)
!

