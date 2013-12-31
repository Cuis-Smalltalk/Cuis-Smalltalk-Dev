'From Cuis 4.0 of 21 April 2012 [latest update: #1348] on 3 August 2012 at 1:31:06 pm'!

!TextModelMorph class methodsFor: 'instance creation' stamp: 'jmv 8/3/2012 13:24'!
textProvider: aTextProvider textGetter: textGetter textSetter: textSetter selectionGetter: selectionGetter allowStyler: aBoolean

	| newModel answer |
	answer _ self new.
	(aBoolean and: [
		Preferences syntaxHighlightingAsYouType and: [ aTextProvider is: #ShoutEnabled ]])
			ifTrue: [ answer styler: SHTextStylerST80 new ].

	newModel _ PluggableTextModel on: aTextProvider.
	newModel textGetter: textGetter textSetter: textSetter selectionGetter: selectionGetter.
	aTextProvider addDependent: newModel.

	answer model: newModel.
	answer autoCompleter: newModel autoCompleter.
	^answer! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 8/3/2012 13:11'!
annotationForClassDefinitionFor: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Class definition for ';
			nextPutAll: aClass name;
			nextPutAll: '. '.
		aClass theNonMetaClass selectors size printOn: strm.
		strm nextPutAll: ' instance methods. '.
		aClass theMetaClass selectors size printOn: strm.
		strm nextPutAll: ' class methods. '.
		aClass theNonMetaClass linesOfCode printOn: strm.
		strm nextPutAll: ' total lines of code.' ]! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 8/3/2012 13:27'!
buildMorphicAnnotationsPane

	| aTextMorph |
	aTextMorph _ TextModelMorph
		textProvider: model
		textGetter: #annotation
		textSetter: nil
		selectionGetter: nil
		allowStyler: false.
	model when: #annotationChanged send: #redrawNeeded to: aTextMorph.
	aTextMorph
		askBeforeDiscardingEdits: false;
		hideScrollBarsIndefinitely.
	^aTextMorph! !

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 8/3/2012 13:25'!
buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
.
	^TextModelMorph
		textProvider: model
		textGetter: #acceptedContents
		textSetter: #contents:notifying:
		selectionGetter: #contentsSelection
		allowStyler: true! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 8/3/2012 13:27'!
buildMorphicCommentPane
	"Construct the pane that shows the class comment."

	| commentPane |
	commentPane _ BrowserCommentTextMorph
				textProvider: model
				textGetter: #classCommentText
				textSetter: #newClassComment:
				selectionGetter: nil
				allowStyler: false.
	^ commentPane! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 8/3/2012 13:26'!
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
			selectionGetter: #contentsSelection
			allowStyler: true.
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
			selectionGetter: #contentsSelection
			allowStyler: true.

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


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 8/3/2012 13:25'!
morphicFileContentsPane

	^TextModelMorph
		textProvider: model
		textGetter: #acceptedContents 
		textSetter: #put:
		selectionGetter: #contentsSelection
		allowStyler: true! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/3/2012 13:25'!
createTextPaneExtent: answerExtent acceptBoolean: acceptBoolean
	"create the textPane"
	| result |
	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval
				allowStyler: true.
	result morphExtent: answerExtent.
	result borderWidth: 1; borderColor: Color lightGray.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	self addMorph: result.
	result bounds: (14@25 corner: 257@84).
	^ result! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 8/3/2012 13:25'!
buildMorphicWindow
	" Inspector openAsMorphOn: SystemOrganization "
	| contentsText list upperRow evaluatorText |
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
			selectionGetter: #contentsSelection
			allowStyler: true.
	evaluatorText _ (TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false.
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: list proportionalWidth: 0.3;
		addAdjusterAndMorph: contentsText proportionalWidth: 0.7.
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.7;
		addAdjusterAndMorph: evaluatorText proportionalHeight: 0.3.
	self setLabel: ([model object printStringLimitedTo: 64] on: Error do: [ model object class name, ' (printing failed)']).
	self setUpdatablePanesFrom: #(fieldList)! !


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'jmv 8/3/2012 13:29'!
buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"

"MessageNames openMessageNames"

	| selectorListView firstRow searchButton secondRow |
	textMorph _ TextModelMorph
		textProvider: model
		textGetter: #searchString 
		textSetter: #searchString:
		selectionGetter: #contentsSelection
		allowStyler: false.
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


!TextModelMorph class methodsFor: 'instance creation' stamp: 'jmv 8/3/2012 13:25'!
textProvider: aTextProvider
	^ self
		textProvider: aTextProvider
		textGetter: nil
		textSetter: nil
		selectionGetter: nil
		allowStyler: true! !

!TextModelMorph class methodsFor: 'instance creation' stamp: 'jmv 8/3/2012 13:25'!
textProvider: aTextProvider textGetter: getTextSel
	^ self
		textProvider: aTextProvider
		textGetter: getTextSel
		textSetter: nil
		selectionGetter: nil
		allowStyler: true! !

!TextModelMorph class methodsFor: 'instance creation' stamp: 'jmv 8/3/2012 13:25'!
textProvider: aTextProvider textGetter: getTextSel textSetter: setTextSel
	^ self
		textProvider: aTextProvider
		textGetter: getTextSel
		textSetter: setTextSel
		selectionGetter: nil
		allowStyler: true! !

!methodRemoval: TextModelMorph class #textProvider:textGetter:textSetter:selectionGetter:!
TextModelMorph class removeSelector: #textProvider:textGetter:textSetter:selectionGetter:!
