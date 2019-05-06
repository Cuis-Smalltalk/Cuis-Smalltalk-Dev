'From Cuis 5.0 of 7 November 2016 [latest update: #3716] on 5 May 2019 at 6:48:52 pm'!

!CodeProvider methodsFor: 'annotation' stamp: 'pb 5/5/2019 18:18:13'!
annotation
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |

	((aSelector _ self selectedMessageName) == nil or: [(aClass _ self selectedClassOrMetaClass) == nil])
		ifTrue: [^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass! !


!Browser methodsFor: 'annotation' stamp: 'pb 5/5/2019 18:19:46'!
annotation
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |
	(aClass _ self selectedClassOrMetaClass)
		ifNil: [^ ''].
	self editSelection == #editComment
		ifTrue: [^ self annotationForSelector: #Comment ofClass: aClass].
	self editSelection == #editClass
		ifTrue: [^ self annotationForSelector: #Definition ofClass: aClass].
	(aSelector _ self selectedMessageName)
		ifNil: [^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass! !


!ChangeList methodsFor: 'viewing access' stamp: 'pb 5/5/2019 18:19:33'!
annotation
	"Answer the string to be shown in an annotation pane.  Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact."

	| change count selectedCount ann1 ann2 |
	change _ self currentChange.
	
	change isNil ifTrue: [
		count _ listSelections size.
		selectedCount _ listSelections count: [ :flag | flag ].
		^ 'Total items: ', count printString, ' - Selected items: ', selectedCount printString ].

	change changeType == #classDefinition ifTrue: [
		ann1 _ change isMetaClassChange ifTrue: [ 'Metaclass' ] ifFalse: [ 'Class' ].
		ann2 _ (Smalltalk includesKey: change changeClassName) ifTrue: [ ' already exists' ] ifFalse: [ ' not in system' ].
		^ann1, ann2 ].
	
	(self selectedMessageName isNil or: [self selectedClassOrMetaClass isNil])
		ifTrue: [^ ''].

	^ change methodSelector notNil
		ifFalse: [ super annotation]
		ifTrue: [
			(self isNewMethod: change)
				ifTrue: [
					String streamContents: [ :strm | | sel |
						sel _ change methodSelector.
						strm
							nextPutAll: change changeClassName;
							nextPutAll: ' >> ';
							nextPutAll: sel;
							nextPutAll: ' is not present in the system. It has '.
							count _ Smalltalk numberOfImplementorsOf: sel.
							count = 1
								ifTrue: [strm nextPutAll: '1 implementor']
								ifFalse: [count printOn: strm. strm nextPutAll: ' implementors' ].
							strm nextPutAll: ' and '.
							count _ Smalltalk numberOfSendersOf: sel.
							count = 1
								ifTrue: [strm nextPutAll: '1 sender.']
								ifFalse: [count printOn: strm. strm nextPutAll: ' senders.' ].
						]
					]
				ifFalse: [
					'current version: ', super annotation]]! !


!TestRunner methodsFor: 'updating' stamp: 'pb 5/5/2019 18:31:36'!
refreshTR
	self updateErrors: TestResult new.
	self updateFailures: TestResult new.
	self displayPassFail: ''.
	self displayDetails: ''! !

!TestRunner methodsFor: 'initialization' stamp: 'pb 5/5/2019 18:31:29'!
initialize

	result := TestResult new.
	passFail := ''.
	details := ''.
	failures := OrderedCollection new.
	errors := OrderedCollection new.
	tests := self gatherTestNames.
	selectedSuite := 0.
	selectedFailureTest := 0.
	selectedErrorTest := 0.
	selectedSuites := tests collect: [:ea | true].
	running := nil.
	runSemaphore := Semaphore new! !


!CodePackage methodsFor: 'naming' stamp: 'pb 5/5/2019 17:59:16'!
packageName: aString
	packageName _ aString.
	description _ ''.
	featureSpec _ FeatureSpec new.
	featureSpec provides: (Feature name: packageName version: 1 revision: 0).
	hasUnsavedChanges _ self includesAnyCode.
	"But reset revision if it was incremented because of marking it dirty!!"
	featureSpec provides name: packageName version: 1 revision: 0! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:13:05'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames upperRow  description summary backColor labelBackground |
	backColor := self textBackgroundColor.	
	labelBackground := Theme current background.
	
	dirtyFlags := PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: backColor.
	dirtyFlags := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: ' Unsaved?');
		addMorphUseAll: dirtyFlags.

	names := PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: backColor.
	names := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: ' Package Name');
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: backColor.
	fileNames := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: ' File Name');
		addMorphUseAll: fileNames.

	upperRow := LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: names proportionalWidth: 0.27;
		addAdjusterAndMorph: fileNames proportionalWidth: 0.6.
		
	description := (TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:) emptyTextDisplayMessage: 'Please enter a description for this package'.

	summary := (TextModelMorph
		textProvider: model
		textGetter: #summary) emptyTextDisplayMessage: 'Package summary (No package selected?)'.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: self buildButtonPane fixedHeight: Theme current buttonPaneHeight;
		addAdjusterAndMorph: summary fixedHeight: 60;
		addAdjusterAndMorph: description proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.15.
	self setLabel: 'Installed Packages'! !


!CodeWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:24:20'!
buildMorphicAnnotationsPane

	| aTextMorph |
	aTextMorph _ (TextModelMorph
		textProvider: model
		textGetter: #annotation) emptyTextDisplayMessage: 'Class or method annotation (not selected?)'.
	model when: #annotationChanged send: #refetch to: aTextMorph model.
	model when: #decorateButtons send: #decorateButtons to: self.
	aTextMorph
		askBeforeDiscardingEdits: false;
		hideScrollBarsIndefinitely.
	^aTextMorph! !

!CodeWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:40:52'!
buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
	^ (TextModelMorph
		textProvider: model
		textGetter: #acceptedContents
		textSetter: #contents:notifying:
		selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Smalltalk code (nothing selected?)'! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:07:59'!
buildMorphicCommentPane
	"Construct the pane that shows the class comment."
	 ^ (BrowserCommentTextMorph
		textProvider: model
		textGetter: #classCommentText
		textSetter: #newClassComment:) emptyTextDisplayMessage: 'Please enter a comment for this class'.! !


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:07:23'!
buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"

"MessageNames openMessageNames"

	| selectorListView firstRow searchButton secondRow |
	textMorph _ (TextModelMorph
		textProvider: model
		textGetter: #searchString 
		textSetter: #searchString:
		selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Type here, then hit Search'.
	textMorph askBeforeDiscardingEdits: false.
	textMorph acceptOnCR: true.
	textMorph hideScrollBarsIndefinitely.

	searchButton _ PluggableButtonMorph new 
		model: textMorph textMorph;
		label: 'Search';
		action: #acceptContents.
	searchButton setBalloonText: 'Type some letters into the pane at right, and then press this Search button (or hit RETURN) and all method selectors that match what you typed will appear in the list pane below.  Click on any one of them, and all the implementors of that selector will be shown in the right-hand pane, and you can view and edit their code without leaving this tool.'.

	firstRow _ LayoutMorph newRow.
	firstRow color: self windowColor.
	firstRow
		doAdoptWidgetsColor;
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


!ChangeListWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:48:30'!
buildMorphicCodePane

	^(TextModelMorph
		textProvider: model
		textGetter: #acceptedContents) emptyTextDisplayMessage: 'Selection detail (no change selected?)'! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:47:09'!
buildMorphicWindow
	"Open a full morphic debugger with the given label"

	| bottomMorph |

	stackList _ PluggableListMorph
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
	receiverInspectorText _ (TextModelMorph
			textProvider: model receiverInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Receiver scope'.
	contextVariableInspector _ PluggableListMorph
			model: model contextVariablesInspector 
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #contextFieldListMenu
			keystrokeAction: #inspectorKey:from:.
	contextVariableInspector doubleClickSelector: #inspectSelection.
	contextVariableInspectorText _ (TextModelMorph
			textProvider: model contextVariablesInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Context scope'.

	bottomMorph _ LayoutMorph newRow.
	bottomMorph
		addMorph: receiverInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: receiverInspectorText proportionalWidth: 0.3;
		addAdjusterAndMorph: contextVariableInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: contextVariableInspectorText proportionalWidth: 0.3.

	self layoutMorph
		addMorph: stackList proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55;
		addAdjusterAndMorph: bottomMorph proportionalHeight: 0.2! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:44:07'!
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
			askBeforeDiscardingEdits: false;
			emptyTextDisplayMessage: 'Smalltalk expression'.
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


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:42:14'!
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
	textMorph _ ((TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false;
			emptyTextDisplayMessage: 'Smalltalk expression (self is selected item)').
	self layoutMorph
		addMorph: listMorph proportionalHeight: 0.8;
		addAdjusterAndMorph: textMorph proportionalHeight: 0.2.
	self setLabel: (model rootObject printStringLimitedTo: 64)! !


!ProcessBrowserWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:41:54'!
buildMorphicWindow
	"Create a pluggable version of me, answer a window"
	| aTextMorph list1 list2 upperRow |
	list1 _ PluggableListMorph
				model: model
				listGetter: #processNameList
				indexGetter: #processListIndex
				indexSetter: #processListIndex:
				mainView: self
				menuGetter: #processListMenu
				keystrokeAction: #processListKey:from:.
	list2 _ PluggableListMorph
				model: model
				listGetter: #stackNameList
				indexGetter: #stackListIndex
				indexSetter: #stackListIndex:
				mainView: self
				menuGetter: #stackListMenu
				keystrokeAction: #stackListKey:from:.
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: list1 proportionalWidth: 0.5;
		addAdjusterAndMorph: list2 proportionalWidth: 0.5.
	aTextMorph _ (TextModelMorph
				textProvider: model
				textGetter: #selectedMethod) emptyTextDisplayMessage: 'Method source (not selected?)'.
	aTextMorph askBeforeDiscardingEdits: false.
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: aTextMorph proportionalHeight: 0.5.
	self setLabel: 'Process Browser'! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:32:14'!
buildDetailsText
	detailsText _ (TextModelMorph
		textProvider: model
		textGetter: #details) emptyTextDisplayMessage: 'Test run details (no results to display)'.
	detailsText hideScrollBarsIndefinitely.
	^detailsText! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'pb 5/5/2019 18:33:10'!
buildPassFailText
	passFailText _ (TextModelMorph
		textProvider: model
		textGetter: #passFail) emptyTextDisplayMessage: 'Pass/Fail summary (no results to display)'.
	passFailText hideScrollBarsIndefinitely.
	^ passFailText! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'pb 5/5/2019 18:39:46'!
createTextPaneAcceptOnCR: acceptBoolean
	"create the textPane"
	| result |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	result _ (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			result emptyTextDisplayMessage: msg ].
	result
		hasUnacceptedEdits: true;
		acceptOnCR: acceptBoolean;
		escAction: [ self cancelClicked ];
		morphExtent: `18 @ 5` * self sizeUnit.
	self
		addMorph: result
		position: `1 @ 2` * self sizeUnit.
	^ result.! !

