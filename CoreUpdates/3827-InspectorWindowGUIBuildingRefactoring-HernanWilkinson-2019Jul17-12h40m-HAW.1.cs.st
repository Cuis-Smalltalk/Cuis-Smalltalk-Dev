'From Cuis 5.0 [latest update: #3826] on 17 July 2019 at 5:45:34 pm'!

!InspectorWindow methodsFor: 'GUI building' stamp: 'HAW 7/17/2019 17:34:49'!
buildContentsText

	| contentsText |

	contentsText _ TextModelMorph
			textProvider: model
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.
			
	^contentsText! !

!InspectorWindow methodsFor: 'GUI building' stamp: 'HAW 7/17/2019 17:35:31'!
buildEvaluatorText

	| evaluatorText |
	
	evaluatorText _ (TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false;
			emptyTextDisplayMessage: 'Smalltalk expression'.
	
	^evaluatorText ! !

!InspectorWindow methodsFor: 'GUI building' stamp: 'HAW 7/17/2019 17:34:02'!
buildList

	| list |
	
	list _ PluggableListMorph
			model: model 
			listGetter: #fieldList
			indexGetter: #selectionIndex
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #fieldListMenu
			keystrokeAction: #inspectorKey:from:.
	list doubleClickSelector: #inspectSelection.
	
	^list! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'HAW 7/17/2019 17:35:57'!
buildMorphicWindow
	" Inspector openOn: SystemOrganization "
	| contentsText list upperRow evaluatorText label |

	"Build widgets. We'll assemble them below."
	list _ self buildList.
	contentsText _ self buildContentsText.
	evaluatorText _ self buildEvaluatorText.
	
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


!InspectorWindow reorganize!
('GUI building' buildContentsText buildEvaluatorText buildList buildMorphicWindow initialExtent)
('menu building' fieldListMenu)
('menu commands' addEntry browseClassRefs browseFullProtocol browseHierarchy browseMethodFull copySelectionToClipboard exploreSelection inspectBasic inspectSelection openReferencesFinder openWeightExplorer renameEntry sendersOfSelectedKey)
('keyboard shortcuts' inspectorKey:from:)
('accessing' classDefinitionChangedFrom:to: model:)
!

