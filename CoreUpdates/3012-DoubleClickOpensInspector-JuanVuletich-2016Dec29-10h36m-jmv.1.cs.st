'From Cuis 5.0 of 7 November 2016 [latest update: #3011] on 29 December 2016 at 11:02:24 am'!
!classDefinition: #HierarchicalListMorph category: #'Morphic-Views'!
PluggableScrollPane subclass: #HierarchicalListMorph
	instanceVariableNames: 'selectedMorph getListSelector keystrokeActionSelector autoDeselect autoExpand sortingSelector getSelectionSelector setSelectionSelector menuGetter mainView highlightedMorph doubleClickSelector '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!Inspector methodsFor: 'user commands' stamp: 'jmv 12/29/2016 10:58:58'!
inspectSelection
	self selection inspect! !


!ObjectExplorer methodsFor: 'user commands' stamp: 'jmv 12/29/2016 11:01:35'!
inspectSelection
	self object inspect! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 12/29/2016 10:55:06'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition

	doubleClickSelector ifNil: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	^ self model perform: doubleClickSelector! !

!HierarchicalListMorph methodsFor: 'initialization' stamp: 'jmv 12/29/2016 10:55:42'!
doubleClickSelector: aSymbol
	doubleClickSelector _ aSymbol! !


!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 12/29/2016 10:41:28'!
doubleClickSelector: aSymbol
	doubleClickSelector _ aSymbol! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 12/29/2016 10:53:49'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |
	aMouseButtonEvent hand newKeyboardFocus: self.
	itemMorph _ self itemFromPoint: localEventPosition.
	itemMorph ifNil: [ ^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].
	self highlightedMorph: itemMorph.
	(itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)))
		ifTrue: [ ^self toggleExpandedState: itemMorph event: aMouseButtonEvent ]. 
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 12/29/2016 10:59:15'!
buildMorphicWindow
	"Open a full morphic debugger with the given label"

	| upperMorph receiverInspector receiverInspectorText contextVariableInspector contextVariableInspectorText bottomMorph |

	upperMorph _ PluggableListMorph
		model: model 
		listGetter: #contextStackList
		indexGetter: #contextStackIndex
		indexSetter: #toggleContextStackIndex:
		mainView: self
		menuGetter: #contextStackMenu
		keystrokeAction: #contextStackKey:from:.

	receiverInspector _ PluggableListMorph
			model: model receiverInspector
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #receiverFieldListMenu
			keystrokeAction: #inspectorKey:from:.
	receiverInspector doubleClickSelector: #inspectSelection.
	receiverInspectorText _ TextModelMorph
			textProvider: model receiverInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.
	contextVariableInspector _ PluggableListMorph
			model: model contextVariablesInspector 
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #contextFieldListMenu
			keystrokeAction: #inspectorKey:from:.
	contextVariableInspector doubleClickSelector: #inspectSelection.
	contextVariableInspectorText _ TextModelMorph
			textProvider: model contextVariablesInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.

	bottomMorph _ LayoutMorph newRow.
	bottomMorph
		addMorph: receiverInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: receiverInspectorText proportionalWidth: 0.3;
		addAdjusterAndMorph: contextVariableInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: contextVariableInspectorText proportionalWidth: 0.3.

	self layoutMorph
		addMorph: upperMorph proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55;
		addAdjusterAndMorph: bottomMorph proportionalHeight: 0.2! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 12/29/2016 11:00:06'!
buildMorphicWindow
	" Inspector openOn: SystemOrganization "
	| contentsText list upperRow evaluatorText label |
	"Build widgets. We'll assemble them below."
	list _ PluggableListMorph
			model: model 
			listGetter: #fieldList
			indexGetter: #selectionIndex
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #fieldListMenu
			keystrokeAction: #inspectorKey:from:.
	list doubleClickSelector: #inspectSelection.
	contentsText _ TextModelMorph
			textProvider: model
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.
	evaluatorText _ (TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false.
	"Upper row has two widgets, side by side: the list of variables at the left side, and the variable contents pane at the right side."
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: list proportionalWidth: 0.3;
		addAdjusterAndMorph: contentsText proportionalWidth: 0.7.
	"Inspector Window has two rows: At the top, the one we just bult. Below it, the evaluation pane."
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.7;
		addAdjusterAndMorph: evaluatorText proportionalHeight: 0.3.
	"Set label"
	label _ [model object printStringLimitedTo: 64]
		on: UnhandledError
		do: [:ex | ex return: model object class printString, ' (printing failed)'].
	(label includesSubString: model object  class name)
		ifFalse: [label _ model object  class name, ': ', label].
	self setLabel: label! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 12/29/2016 11:00:46'!
buildMorphicWindow

	| textMorph |
	listMorph _ HierarchicalListMorph
			model: model
			listGetter: #getList
			indexGetter: #getCurrentSelection
			indexSetter: #noteNewSelection:
			mainView: self
			menuGetter: #genericMenu
			keystrokeAction: #explorerKey:from:.
	listMorph autoDeselect: false.
	listMorph doubleClickSelector: #inspectSelection.
	textMorph _ (TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false.
	self layoutMorph
		addMorph: listMorph proportionalHeight: 0.8;
		addAdjusterAndMorph: textMorph proportionalHeight: 0.2.
	self setLabel: (model rootObject printStringLimitedTo: 64)! !

!ObjectExplorerWindow methodsFor: 'menu commands' stamp: 'jmv 12/29/2016 10:50:28'!
openWeightExplorer
	"Create and schedule a Weight Explorer on the receiver's model's currently selected object."

	^WeightTracer openExplorerOn: model object! !


!Inspector reorganize!
('accessing' baseFieldList context: fieldList i1 i2 object object: selectedClass selectedClassOrMetaClass update wantsSteps)
('selecting' accept: contentsIsString replaceSelectionValue: selectedSlotName selection selectionIndex selectionPrintString selectionUnmodifiable toggleIndex:)
('code' doItContext doItReceiver)
('private' printStringErrorText)
('stepping' stepAt:)
('contents' acceptedStringOrText)
('initialization' initialize inspect:)
('user interface support' autoCompleterClassFor: editorClassFor:)
('user commands' inspectSelection)
!


!ObjectExplorer reorganize!
('accessing' doItContext doItReceiver getList object parentObject rootObject rootObject: selector)
('menus' selectedClass)
('monitoring' basicMonitorList monitorList stepAt: stopMonitoring)
('user interface support' autoCompleterClassFor: editorClassFor:)
('user commands' inspectSelection)
!

!classDefinition: #HierarchicalListMorph category: #'Morphic-Views'!
PluggableScrollPane subclass: #HierarchicalListMorph
	instanceVariableNames: 'selectedMorph getListSelector keystrokeActionSelector autoDeselect autoExpand sortingSelector getSelectionSelector setSelectionSelector menuGetter mainView highlightedMorph doubleClickSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!ObjectExplorerWindow reorganize!
('GUI building' buildMorphicWindow expandAll initialExtent)
('building menus' genericMenu)
('keyboard shortcuts' explorerKey:from:)
('menu commands' browseClassRefs browseFullProtocol browseHierarchy browseMethodFull exploreSelection inspectBasic inspectSelection monitor: openWeightExplorer stopMonitoring)
('menu commands - reference finder' objectsToSkipDuringRescan openReferencesFinder rescan)
('stepping' stepAt:)
!

