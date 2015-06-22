'From Cuis 4.2 of 25 July 2013 [latest update: #2381] on 21 June 2015 at 2:58:36.021206 pm'!

!TextModelMorph class methodsFor: 'instance creation' stamp: 'jmv 6/21/2015 14:53'!
textProvider: aTextProvider textGetter: textGetter textSetter: textSetter selectionGetter: selectionGetter
	| newModel answer |
	answer _ self new.
	newModel _ PluggableTextModel on: aTextProvider.
	newModel textGetter: textGetter textSetter: textSetter selectionGetter: selectionGetter.
	aTextProvider addDependent: newModel.
	answer model: newModel.
	^answer! !


!TextModelMorph class methodsFor: 'instance creation' stamp: 'jmv 6/21/2015 14:57'!
textProvider: aTextProvider
	^ self
		textProvider: aTextProvider
		textGetter: nil! !

!TextModelMorph class methodsFor: 'instance creation' stamp: 'jmv 6/21/2015 14:55'!
textProvider: aTextProvider textGetter: getTextSel
	^ self
		textProvider: aTextProvider
		textGetter: getTextSel
		textSetter: nil! !

!TextModelMorph class methodsFor: 'instance creation' stamp: 'jmv 6/21/2015 14:54'!
textProvider: aTextProvider textGetter: getTextSel textSetter: setTextSel
	^ self
		textProvider: aTextProvider
		textGetter: getTextSel
		textSetter: setTextSel
		selectionGetter: nil! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 6/21/2015 14:57'!
buildMorphicAnnotationsPane

	| aTextMorph |
	aTextMorph _ TextModelMorph
		textProvider: model
		textGetter: #annotation.
	model when: #annotationChanged send: #refetch to: aTextMorph model.
	aTextMorph
		askBeforeDiscardingEdits: false;
		hideScrollBarsIndefinitely.
	^aTextMorph! !

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 6/21/2015 14:55'!
buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
.
	^TextModelMorph
		textProvider: model
		textGetter: #acceptedContents
		textSetter: #contents:notifying:
		selectionGetter: #contentsSelection! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/21/2015 14:54'!
buildMorphicCommentPane
	"Construct the pane that shows the class comment."

	| commentPane |
	commentPane _ BrowserCommentTextMorph
				textProvider: model
				textGetter: #classCommentText
				textSetter: #newClassComment:.
	^ commentPane! !


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'jmv 6/21/2015 14:55'!
buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"

"MessageNames openMessageNames"

	| selectorListView firstRow searchButton secondRow |
	textMorph _ TextModelMorph
		textProvider: model
		textGetter: #searchString 
		textSetter: #searchString:
		selectionGetter: #contentsSelection.
	textMorph textMorph setProperty: #alwaysAccept toValue: true.
	textMorph askBeforeDiscardingEdits: false.
	textMorph acceptOnCR: true.
	textMorph setTextColor: Color brown.
	textMorph hideScrollBarsIndefinitely.

	searchButton _ PluggableButtonMorph new 
		model: textMorph textMorph;
		label: 'Search';
		action: #acceptContents.
	searchButton setBalloonText: 'Type some letters into the pane at right, and then press this Search button (or hit RETURN) and all method selectors that match what you typed will appear in the list pane below.  Click on any one of them, and all the implementors of that selector will be shown in the right-hand pane, and you can view and edit their code without leaving this tool.'.

	firstRow _ LayoutMorph newRow.
	firstRow color: self windowColor.
	firstRow
		addMorph: searchButton proportionalWidth: 0.25;
		addMorph: textMorph proportionalWidth: 0.75.

	selectorListView _ PluggableListMorph
		model: model
		listGetter: #selectorList
		indexGetter: #selectorListIndex
		indexSetter: #selectorListIndex:
		mainView: self
		menuGetter: #selectorListMenu
		keystrokeAction: #selectorListKey:from:.
	secondRow _  LayoutMorph newRow.
	secondRow
		addMorph: selectorListView proportionalWidth: 0.5;
		addAdjusterAndMorph: self buildMorphicMessageList proportionalWidth: 0.5.

	self layoutMorph
		addMorph: firstRow fixedHeight: self defaultButtonPaneHeight+4;
		addAdjusterAndMorph: secondRow proportionalHeight: 0.5;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.5.
	model changed: #editSelection! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 6/21/2015 14:55'!
buildMorphicWindow
	"Open a full morphic debugger with the given label"

	| upperMorph bottomMorph1 bottomMorph2 bottomMorph3 bottomMorph4 bottomMorph |

	upperMorph _ PluggableListMorph
		model: model 
		listGetter: #contextStackList
		indexGetter: #contextStackIndex
		indexSetter: #toggleContextStackIndex:
		mainView: self
		menuGetter: #contextStackMenu
		keystrokeAction: #contextStackKey:from:.

	bottomMorph1 _ PluggableListMorph
			model: model receiverInspector
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #receiverFieldListMenu
			keystrokeAction: #inspectorKey:from:.
	bottomMorph2 _ TextModelMorph
			textProvider: model receiverInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.
	bottomMorph3 _ PluggableListMorph
			model: model contextVariablesInspector 
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #contextFieldListMenu
			keystrokeAction: #inspectorKey:from:.
	bottomMorph4 _ TextModelMorph
			textProvider: model contextVariablesInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.

	bottomMorph _ LayoutMorph newRow.
	bottomMorph
		addMorph: bottomMorph1 proportionalWidth: 0.2;
		addAdjusterAndMorph: bottomMorph2 proportionalWidth: 0.3;
		addAdjusterAndMorph: bottomMorph3 proportionalWidth: 0.2;
		addAdjusterAndMorph: bottomMorph4 proportionalWidth: 0.3.

	self layoutMorph
		addMorph: upperMorph proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55;
		addAdjusterAndMorph: bottomMorph proportionalHeight: 0.2! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 6/21/2015 14:55'!
morphicFileContentsPane

	^TextModelMorph
		textProvider: model
		textGetter: #acceptedContents 
		textSetter: #put:
		selectionGetter: #contentsSelection! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 6/21/2015 14:55'!
buildMorphicWindow
	" Inspector openOn: SystemOrganization "
	| contentsText list upperRow evaluatorText |
	"Build widgets. We'll assemble them below."
	list _ PluggableListMorph
			model: model 
			listGetter: #fieldList
			indexGetter: #selectionIndex
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #fieldListMenu
			keystrokeAction: #inspectorKey:from:.
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
	self setLabel: ([model object printStringLimitedTo: 64] on: Error do: [ model object class name, ' (printing failed)']).
	"This is needed due to the way we update some windows when stuff changes. A cleaner approach is needed here!!"
	self setUpdatablePanesFrom: #(fieldList)! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 6/21/2015 14:55'!
createTextPaneExtent: answerExtent acceptBoolean: acceptBoolean
	"create the textPane"
	| result |

	self flag: #todo. "Integrate this method with the Theme system. --cbr"

	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval.
	result morphExtent: answerExtent.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	result morphExtent: extent-(28@62).
	self addMorph: result position: 14@25.
	^ result! !

!methodRemoval: TextModelMorph class #textProvider:textGetter:textSetter:selectionGetter:allowStyler:!
TextModelMorph class removeSelector: #textProvider:textGetter:textSetter:selectionGetter:allowStyler:!
