'From Cuis 4.0 of 21 April 2012 [latest update: #1288] on 23 May 2012 at 8:07:33 pm'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 5/23/2012 19:30'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin |

	f _ self fontToUse.
	center _ bounds center.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ bounds width-labelMargin-labelMargin-1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ bounds left + labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f height // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			in: (x@y extent: bounds extent - (labelMargin*2-2@4))
			font: f
			color: Theme current buttonLabel ]! !


!SystemWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 18:17'!
buttonColor

	^Theme current buttonColorFrom: self widgetsColor! !

!SystemWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 18:59'!
textBackgroundColor

	^Theme current paneBackgroundFrom: self widgetsColor! !


!Theme methodsFor: 'other options' stamp: 'jmv 5/23/2012 19:33'!
embossedButtonLabels
	"Currently only apply to rounded buttons!!"

	^true! !

!Theme methodsFor: 'shout' stamp: 'jmv 5/23/2012 20:07'!
italizeArguments

	^true! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 5/23/2012 18:06'!
createAcceptButton
	"create the [accept] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current acceptButton;
		 label: 'Accept';
		 action: #acceptClicked.
	result bounds: (29@90 corner: 122@117).
	self addMorph: result.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 5/23/2012 18:08'!
createCancelButton
	"create the [cancel] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current cancelButton;
		 label: 'Cancel';
		 action: #cancelClicked.
	result bounds: (149@90 corner: 242@117).
	self addMorph: result.
	^ result! !

!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'jmv 5/23/2012 19:20'!
drawOn: aCanvas
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas
				roundRect: bounds
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ].
	aCanvas
		fillRectangle: textPane bounds
		colorOrInfiniteForm: (Theme current paneBackgroundFrom: color)! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 5/23/2012 19:31'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: bounds
		colorOrInfiniteForm: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol.

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 5/23/2012 19:33'!
drawRoundGradientLookOn: aCanvas
	| r colorForButton rect bottomFactor topFactor |

	rect _ bounds insetBy: 1@3.
	self isPressed
		ifFalse: [
			topFactor _ Theme current buttonGradientTopFactor.
			bottomFactor _ Theme current buttonGradientBottomFactor.
			self mouseIsOver
				ifTrue: [	
					colorForButton _ Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [
					colorForButton _ color ]]
		ifTrue: [
			topFactor _ Theme current buttonGradientBottomFactor.
			bottomFactor _ Theme current buttonGradientTopFactor.
			colorForButton _ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		Theme current useButtonGradient
			ifTrue: [
				aCanvas
					roundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				aCanvas roundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ]! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 5/23/2012 18:43'!
openInWorld
	"Ensure all widgets have proper colors before opening"
	self widgetsColor: self widgetsColor.
	super openInWorld! !

!SystemWindow methodsFor: 'panes' stamp: 'jmv 5/23/2012 18:46'!
widgetsColor

	widgetsColor ifNotNil: [ ^ widgetsColor ].
	^Display depth > 2
		ifTrue: [ self windowColor ]
		ifFalse: [ Color white ]! !

!SystemWindow methodsFor: 'panes' stamp: 'jmv 5/23/2012 19:00'!
widgetsColor: aColor
	"aColor will be used for titles, borders, etc.
	A variation of it, #paneColorFrom:, will be used for panes background"

	widgetsColor _ aColor.
	self color: self textBackgroundColor.
	self adoptWidgetsColor: widgetsColor! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 19:11'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames upperRow description summary buttonRow browseChangesButton saveButton browseButton createButton deleteButton backColor labelBackground |
	backColor _ self textBackgroundColor.	
	labelBackground _ Theme current background.
	dirtyFlags _ PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: backColor.
	dirtyFlags _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Unsaved?') fixedHeight: 16;
		addMorphUseAll: dirtyFlags.

	names _ PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: backColor.
	names _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Package Name') fixedHeight: 16;
		addMorphUseAll: names.

	fileNames _ PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: backColor.
	fileNames _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' File Name') fixedHeight: 16;
		addMorphUseAll: fileNames.

	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.1;
		addAdjusterAndMorph: names proportionalWidth: 0.2;
		addAdjusterAndMorph: fileNames proportionalWidth: 0.7.

	description _ TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:.

	summary _ TextModelMorph
		textProvider: model
		textGetter: #summary.

	saveButton _ PluggableButtonMorph model: model action: #save label: 'Save (overwrite)'.
	createButton _ PluggableButtonMorph model: self action: #createPackage label: 'Create Package'.
	deleteButton _ PluggableButtonMorph model: self action: #deletePackage label: 'Delete (merge in Cuis)'.
	browseChangesButton _ PluggableButtonMorph model: self action: #browseChanges label: 'Browse unsaved changes'.
	browseButton _ PluggableButtonMorph model: self action: #browse label: 'Browse package code'.
	buttonRow _ LayoutMorph newRow.
	buttonRow
		color: self widgetsColor quiteWhiter;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: createButton proportionalWidth: 0.6;
		addMorph: deleteButton proportionalWidth: 0.6;
		addMorph: browseChangesButton proportionalWidth: 0.6;
		addMorph: browseButton proportionalWidth: 0.6.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: summary proportionalHeight: 0.13;
		addAdjusterAndMorph: description proportionalHeight: 0.3;
		addAdjusterAndMorph: buttonRow proportionalHeight: 0.07.
	self setLabel: 'Installed Packages'! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 18:03'!
optionalButtonRow
	"Answer a row of control buttons"

	| row buttons widths buttonColor |
	buttons _ OrderedCollection new.
	widths _ OrderedCollection new.
	buttonColor _ self buttonColor.
	self optionalButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button _ PluggableButtonMorph 
					model: self
					stateGetter: nil
					action: tuple third.
		button color: buttonColor.
		button label: tuple second asString.
		tuple size > 3 ifTrue: [button setBalloonText: tuple fourth].
		buttons add: button ].
	row _ LayoutMorph newRow.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !

!CodeWindow methodsFor: 'updating' stamp: 'jmv 5/23/2012 18:02'!
decorateForInheritance
	"Check to see if the currently-viewed method has a super send or an override, and if so, change screen feedback, unless the #decorateBrowserButtons says not to."

	| cm aColor aButton flags buttonColor |
	(aButton _ self inheritanceButton) ifNil: [^ self].
	buttonColor _ self buttonColor.

	Preferences decorateBrowserButtons
		ifFalse: [ ^aButton color: buttonColor ].
	cm _ model currentCompiledMethod.
	(cm isKindOf: CompiledMethod)
		ifFalse: [ ^aButton color: buttonColor ].

	flags _ 0.
	model isThisAnOverride ifTrue: [ flags _ flags bitOr: 4 ].
	cm sendsToSuper ifTrue: [ flags _ flags bitOr: 2 ].
	model isThereAnOverride ifTrue: [ flags _ flags bitOr: 1 ].
	aColor _ {

		"This is NOTan override. There is no super implementation."
		buttonColor.							"no sends to super. there is not override in any subclass"
		Color tan.							"no sends to super. there is an override in some subclass"
		Color red.							"sends to super. there is no override in any subclass. Error: no super to call (or calls super with a different message)"
		Color red.							"sends to super. there is  an override in some subclass. Error: no super to call (or calls super with a different message)"

		"This is an override. There is some super implementation"
		Color red muchLighter.			"doesn't have sub; has super but doesn't call it"
		Color r: 0.94 g: 0.823 b: 0.673.		"has sub; has super but doesn't call it"
		Color green muchLighter.			"doesn't have sub; has super and callsl it"
		Color blue muchLighter.			"has sub; has super and callsl it"

	} at: flags + 1.
	aButton color: aColor! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 18:02'!
buildMorphicSwitches
	| instanceSwitch commentSwitch classSwitch row buttonColor |
	instanceSwitch := PluggableButtonMorph 
				model: model
				stateGetter: #instanceMessagesIndicated
				action: #indicateInstanceMessages.
	instanceSwitch
		label: 'instance'.
	commentSwitch := PluggableButtonMorph 
				model: model
				stateGetter: #classCommentIndicated
				action: #plusButtonHit.
	commentSwitch
		label: '?';
		setBalloonText: 'class comment'.
	classSwitch := PluggableButtonMorph 
				model: model
				stateGetter: #classMessagesIndicated
				action: #indicateClassMessages.
	classSwitch
		label: 'class'.
	row _ LayoutMorph newRow.
	row
		addMorph: instanceSwitch proportionalWidth: 0.45;
		addMorph: commentSwitch proportionalWidth: 0.22;
		addMorph: classSwitch proportionalWidth: 0.33.
	buttonColor _ self buttonColor.
	row color: buttonColor.
	{ 
		instanceSwitch.
		commentSwitch.
		classSwitch} do: [:m | 
				m color: buttonColor ].
	^row! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 18:02'!
optionalButtonRow
	"Answer a row of buttons to occur in a tool pane"

	| row buttons widths buttonColor |
	buttons _ OrderedCollection new.
	widths _ OrderedCollection new.
	buttonColor _ self buttonColor.
	self optionalModelButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button _ PluggableButtonMorph 
					model: model
					stateGetter: nil
					action: tuple third.
		button color: buttonColor.
		button label: tuple second asString.
		buttons add: button.
		button setBalloonText: tuple fourth].
	buttons add: self lineDiffButton.
	widths add: 14.
	buttons add: self wordDiffButton.
	widths add: 16.
	model wantsPrettyDiffOption ifTrue: [
		buttons add:  self prettyLineDiffButton.
		widths add: 21.
		buttons add:  self prettyWordDiffButton.
		widths add: 23 ].
	row _ LayoutMorph newRow.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 19:12'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| dirtyFlags changeSetList classList messageList upperPanes backColor labelBackground |
	backColor _ self textBackgroundColor.
	labelBackground _ Theme current background.
	model myChangeSet ifNil: [
		self flag: #ojo. "Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	dirtyFlags _ PluggableListMorph
		model: model
		listGetter: #changeSetDirtyFlags
		indexGetter: nil
		indexSetter: nil.
	dirtyFlags color: backColor.
	dirtyFlags _ LayoutMorph newColumn
		color: Theme current background;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Unsaved?') fixedHeight: 16;
		addMorphUseAll: dirtyFlags.

	changeSetList _ (PluggableListMorphByItem
				model: model
				listGetter: #changeSetList
				indexGetter: #currentCngSet
				indexSetter: #showChangeSetNamed:
				mainView: self
				menuGetter: #changeSetMenu
				keystrokeAction: #changeSetListKey:from:)
			autoDeselect: false.
	changeSetList color: backColor.
	changeSetList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: 'Change Set name') fixedHeight: 16;
		addMorphUseAll: changeSetList.

	classList _ PluggableListMorphByItem
				model: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:
				mainView: self
				menuGetter: #classListMenu
				keystrokeAction: #classListKey:from:.
	classList color: backColor.
	classList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: 'Classes') fixedHeight: 16;
		addMorphUseAll: classList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: dirtyFlags proportionalWidth: 0.1;
		addAdjusterAndMorph: changeSetList proportionalWidth: 0.5;
		addAdjusterAndMorph: classList proportionalWidth: 0.4.

	messageList _ PluggableListMorphByItem
				model: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:
				mainView: self
				menuGetter: #messageMenu
				keystrokeAction: #messageListKey:from:.
	messageList color: backColor.
	messageList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: 'Methods') fixedHeight: 16;
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.5.

	self setLabel: model labelString! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 18:03'!
customButtonRow
	"Answer a button pane affording the user one-touch access to certain functions; the pane is given the formal name 'customButtonPane' by which it can be retrieved by code wishing to send messages to widgets residing on the pane"

	| button buttons row buttonColor |
	
	buttons _ OrderedCollection new.
	buttonColor _ self buttonColor.
	"button with target = self"
	button _ PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #proceed.
	button color: buttonColor.
	button label: 'Proceed'.
	button setBalloonText: 'close the debugger and proceed.'.
	buttons add: button.
	"buttons with model target"
	self customButtonSpecs do: [ :tuple |
		button _ PluggableButtonMorph 
					model: model
					stateGetter: nil
					action: tuple second.
		button color: buttonColor.
		button label: tuple first asString.
		tuple size > 2 ifTrue: [button setBalloonText: tuple third].
		buttons add: button].

	row _ LayoutMorph newRow.
	row color: buttonColor.
	row addMorphs: buttons.
	^row! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 18:03'!
buttonToTriggerIn: aFileList for: service
	"Answer a button that will trigger the receiver service in a file list"

	| aButton |
	service argumentProvider: aFileList.
	aButton := PluggableButtonMorph 
				model: service
				stateGetter: nil
				action: #performService.
	aButton label: service buttonLabel.
	aButton color: self buttonColor.
	aButton setBalloonText: service description.
	^aButton! !

!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 18:03'!
optionalButtonRow
	"Answer the button row associated with a file list"

	| row buttonColor |
	row _ LayoutMorph newRow.
	buttonColor _ self buttonColor.
	row setProperty: #buttonRow toValue: true.  "Used for dynamic retrieval later on"
	row color: buttonColor.
	self updateButtonRow: row.
	^row! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 19:25'!
buildUpperControls
	| refreshButton filterButton stopButton runOneButton runButton runProfiledButton row column1 column2 column3 theTestsList |

	refreshButton _ self buildRefreshButton.
	filterButton _ self buildFilterButton.
	stopButton _ self buildStopButton.
	column1 _ LayoutMorph newColumn.
	column1 addMorphs: { refreshButton . filterButton . stopButton }.

	theTestsList _ PluggableListMorphOfMany
				model: model
				listGetter: #tests
				primarySelectionGetter: #selectedSuite
				primarySelectionSetter: #selectedSuite:
				listSelectionGetter: #listSelectionAt:
				listSelectionSetter: #listSelectionAt:put:
				mainView: self
				menuGetter: #listMenu
				keystrokeAction: nil.
	theTestsList autoDeselect: false.
	theTestsList color: self textBackgroundColor.
	column2 _ LayoutMorph newColumn.
	column2
		addMorph: theTestsList proportionalHeight: 1;
		addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight.

	runOneButton _ self buildRunOneButton.
	runButton _ self buildRunButton.
	runProfiledButton := self buildRunProfiledButton.	
	column3 _ LayoutMorph newColumn.
	column3 addMorphs: { runOneButton . runButton . runProfiledButton }.
	
	row _ LayoutMorph newRow.
	row
		addMorph: column1 fixedWidth: 80;
		addMorph: column2 proportionalWidth: 1;
		addMorph: column3 fixedWidth: 120.

	^row

! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 5/23/2012 18:03'!
optionalButtonRow
	| row button buttons widths buttonColor |

	buttons _ OrderedCollection new.
	widths _ OrderedCollection new.
	buttonColor _ self buttonColor.
	self optionalModelButtonTuples do: [ :tuple | 
		widths add: tuple first.
		button _ PluggableButtonMorph 
			model: model
			stateGetter: nil
			action: tuple third.
		button color: buttonColor.
		button
			label: tuple second.
		buttons add: button].
	row _ LayoutMorph newRow.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !

!TestRunnerWindow methodsFor: 'updating' stamp: 'jmv 5/23/2012 18:19'!
refreshWindow
	| pc |
	pc _ self widgetsColor.
	passFailText color: pc.
	detailsText color: pc.
	model refreshTR! !

!TestRunnerWindow methodsFor: 'updating' stamp: 'jmv 5/23/2012 18:19'!
updateColors
	| aTestResult theColor |
	theColor _ self widgetsColor.
	model ifNotNil: [
		model runButtonState ifFalse: [
			aTestResult _ model result.
			theColor _ aTestResult errors size + aTestResult failures size = 0
				ifTrue: [ Color green lighter ]
				ifFalse: [
					aTestResult errors size > 0
						ifTrue: [ Color red lighter ]
						ifFalse: [ Color yellow lighter ]]].
		self updatePartColors: theColor ]! !


!Theme methodsFor: 'tool colors' stamp: 'jmv 5/23/2012 19:23'!
messageNames

	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ Color r: 0.53 g: 0.77 b: 0.382 ]! !

!Theme methodsFor: 'widget colors' stamp: 'jmv 5/23/2012 18:49'!
acceptButton

	^ self buttonColorFrom: 
		(self useUniformColors
			ifTrue: [ self defaultWindowColor ]
			ifFalse: [ Color r: 0.2 g: 0.6 b: 0.1 ])! !

!Theme methodsFor: 'widget colors' stamp: 'jmv 5/23/2012 18:51'!
cancelButton

	^ self buttonColorFrom: 
		(self useUniformColors
			ifTrue: [ self defaultWindowColor ]
			ifFalse: [ Color r: 0.8 g: 0.2 b: 0.2 ])! !

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 5/23/2012 19:50'!
generateShoutConfig

	| styles colors |
	
	styles := OrderedCollection new.
	colors := self shout as: Dictionary.

	{
		{self undefined. colors at: #undefined}.
		{self literals . colors at: #pseudoVariables}.
		{self defaults . colors at: #defaults}.
		{self pseudoVariables . colors at: #pseudoVariables}.
		{self blockLevelFour . colors at: #blockLevelFour}.
		{self instVar . colors at: #instVar}.
		{self messages . colors at: #messages}.
		{self blockLevelFive . colors at: #blockLevelFive}.
		{self blockLevelSix . colors at: #blockLevelSix}.
		{self blockLevelSeven . colors at: #blockLevelSeven}.
		{self tempBar . colors at: #tempBar}.
		{self methodTags . colors at: #methodTags . #bold}.
		{self globals . colors at: #defaults . #bold}.
		{self incompleteMessages . colors at: #incompleteMessages . #underlined}.
		{self argumentTypes . colors at: #arguments . #italicArguments}.
		{self symbols . colors at: #messages . #bold}.
		{self nilly . nil . #bold}. "This one is odd.  --cbr"
		{self tempVars . colors at: #tempVars . #italic }.
		{self blockTemps . colors at: #tempBar . #italic}
	} do: [:style|
		styles addAll:
			(style first
				collect: [ :category | | elements |
					elements _ style asOrderedCollection.
					elements at: 1 put: category.
					elements last = #italicArguments ifTrue: [
						self italizeArguments
							ifTrue: [ elements at: 3 put: #italic ]
							ifFalse: [ elements removeLast ]].
					Array withAll: elements ])].

	"Miscellaneous remainder after factoring out commonality:"
	self flag: #todo. "investigate meaning of nil in this context"
	styles addAll: {
		{#unfinishedString . colors at: #undefined . #normal}.
		{#undefinedIdentifier . colors at: #undefined . #bold}.
		{#unfinishedComment . colors at: #pseudoVariables . #italic}.
		{#comment . colors at: #methodTags . #italic}.
		{#string . colors at: #instVar . #normal}.
		{#literal . nil . #italic}.
		{#incompleteIdentifier . colors at: #tempVars . {#italic. #underlined}}.
		{#classVar . colors at: #tempVars . #bold}.
	}.

	^ styles! !

!Theme methodsFor: 'shout' stamp: 'jmv 5/23/2012 20:06'!
shout
	"Color symbols as an association list."
	
	^ {
		#defaults 				-> #black.
		#undefined 				-> #red.
		#methodTags 			-> #(green muchDarker).
		#pseudoVariables 		-> #(red muchDarker).
		#messages 				-> #(blue darker).
		#arguments 				-> #(cyan muchDarker).
		#instVar 					-> #(magenta muchDarker).
		#incompleteMessages -> #(gray veryMuchDarker).
		#blockLevelFour 		-> #(green darker).
		#blockLevelFive 		-> #(orange darker).
		#blockLevelSix 			-> #(magenta darker).
		#blockLevelSeven 		-> #blue.
		#tempBar 				-> #gray.
		#tempVars 				-> #(gray muchDarker).
	}! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 5/23/2012 18:55'!
changeTheme

	| themes set menu result |
		themes _ Theme allSubclasses copyWith: Theme.
		set _ themes collect: [ :i | { i asString . i } ].
		menu _ SelectionMenu fromArray: set asArray.
		result _ menu startUpWithCaption: 'Choose a theme'.

	result ifNotNil: [ result beCurrent ]! !

!methodRemoval: PluggableButtonMorph #drawLabelOn:!
PluggableButtonMorph removeSelector: #drawLabelOn:!
