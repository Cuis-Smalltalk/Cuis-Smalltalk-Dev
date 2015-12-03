'From Cuis 4.2 of 25 July 2013 [latest update: #2603] on 29 November 2015 at 6:15:36.532324 pm'!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
RectangleLikeMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation padding cachedMinExtent doAdoptWidgetsColor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 11/29/2015 13:16'!
doAdoptWidgetsColor
	doAdoptWidgetsColor _ true! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 11/29/2015 13:25'!
buildButtonPane

	| saveButton createButton deleteButton browseChangesButton browseButton addReqButton buttonRow |
	saveButton := PluggableButtonMorph 
							model: model 
							action: #save 
							label: 'Save (overwrite)'.
	createButton := PluggableButtonMorph 
							model: self 
							action: #createPackage 
							label: 'Create Package'.
	deleteButton := PluggableButtonMorph 
							model: self 
							action: #deletePackage 
							label: 'Delete (merge in Cuis)'.
	browseChangesButton := PluggableButtonMorph 
							model: self 
							action: #browseChanges 
							label: 'Browse unsaved changes'.
	browseButton := PluggableButtonMorph 
							model: self 
							action: #browse 
							label: 'Browse package code'.
	addReqButton := PluggableButtonMorph 
							model: self 
							action: #addRequirement 
							label: 'Add requirement'.
	buttonRow := LayoutMorph newRow.
	buttonRow
		doAdoptWidgetsColor;
		color: self widgetsColor quiteWhiter;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: createButton proportionalWidth: 0.6;
		addMorph: deleteButton proportionalWidth: 0.6;
		addMorph: browseChangesButton proportionalWidth: 0.6;
		addMorph: browseButton proportionalWidth: 0.6;
		addMorph: addReqButton proportionalWidth: 0.6.
	buttonRow submorphsDo: [ :button | button  color: self widgetsColor ].
	^ buttonRow ! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 11/29/2015 13:26'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout |
	requirements := PluggableListMorph
		model: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	requirements color: Theme current textPane.
		
	deleteReqButton := PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'Delete'.
	deleteReqButton color: self widgetsColor.
							
	buttonLayout := LayoutMorph newColumn.
	buttonLayout addMorph: deleteReqButton 
					layoutSpec: (LayoutSpec 
										proportionalWidth: 1.0 
										proportionalHeight: 1.0
										minorDirectionPadding: #top);
					color: self widgetsColor quiteWhiter.
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LayoutMorph newRow.
	^ reqLayout 
		doAdoptWidgetsColor;
		addMorph: requirements 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.9 
							proportionalHeight: 1.0 
							minorDirectionPadding: #left);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.1 
							proportionalHeight: 1.0 
							minorDirectionPadding: #right);
		color: Color transparent;
		yourself
		! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 11/29/2015 13:18'!
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
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 11/29/2015 13:17'!
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
		doAdoptWidgetsColor;
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


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'jmv 11/29/2015 13:29'!
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


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 11/29/2015 13:18'!
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
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 11/29/2015 13:23'!
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
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons.
	^row! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 11/29/2015 13:29'!
buttonRowForPreDebugWindow: aDebugger
	| row aButton triads buttons |
	buttons _ OrderedCollection new.
	triads _ OrderedCollection withAll: self preDebugButtonSpec.
	(aDebugger interruptedContext selector == #doesNotUnderstand:) ifTrue: [
		triads add: { 'Create'. #createMethod. 'create the missing method' }
	].
	triads do: [ :triad |
		aButton _ PluggableButtonMorph new model: self.
		aButton label: triad first.
		aButton action: triad second.
		aButton setBalloonText: triad third.
		buttons add: aButton].
	
	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: Color transparent.
	row separation: 1.
	row addMorphs: buttons.
	^row! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 11/29/2015 13:19'!
optionalButtonRow
	"Answer the button row associated with a file list"

	| row buttonColor |
	row _ LayoutMorph newRow.
	buttonColor _ self buttonColor.
	row setProperty: #buttonRow toValue: true.  "Used for dynamic retrieval later on"
	row doAdoptWidgetsColor.
	row color: buttonColor.
	self updateButtonRow: row.
	^row! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 11/29/2015 13:22'!
buildUpperControls
	| refreshButton filterButton stopButton runOneButton runButton runProfiledButton row column1 column2 column3 theTestsList |

	refreshButton _ self buildRefreshButton.
	filterButton _ self buildFilterButton.
	stopButton _ self buildStopButton.
	column1 _ LayoutMorph newColumn.
	column1 doAdoptWidgetsColor.
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
	column3 doAdoptWidgetsColor.
	column3 addMorphs: { runOneButton . runButton . runProfiledButton }.
	
	row _ LayoutMorph newRow.
	row
		addMorph: column1 fixedWidth: 80;
		addMorph: column2 proportionalWidth: 1;
		addMorph: column3 fixedWidth: 120.

	^row

! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 11/29/2015 13:19'!
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
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 11/29/2015 13:15'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	doAdoptWidgetsColor == true ifTrue: [
		self color: (Theme current buttonColorFrom: paneColor) 
		]
	ifFalse: [
	self color: Color transparent
	]! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 11/29/2015 13:16'!
initialize
	super initialize.
	separation _ 0.
	doAdoptWidgetsColor _ false! !


!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 11/29/2015 18:05'!
line: pt1 to: pt2 width: wp color: c
	|  p1 p2 w |
	(wp > 0 and: [ c isTransparent not ]) ifTrue: [
		p1 _ (currentTransformation transform: pt1) rounded.
		p2 _ (currentTransformation transform: pt2) rounded.
		w _ currentTransformation externalizeScalar: wp.
		self setPaintColor: c.
		port
			width: w;
			height: w;
			drawFrom: p1 to: p2 ]! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 11/29/2015 18:09'!
image: aForm multipliedBy: aColor at: aPoint
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	Display getCanvas image: (Form makeStar asFormOfDepth: 32) multipliedBy: Color red at: 20@20. Display forceToScreen
	"
	aColor isTransparent ifFalse: [
		self class accessProtect critical: [
			self buildAuxWith: aForm multipliedWith: aColor.
			self image: AuxForm at: aPoint sourceRect: aForm boundingBox ]]! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 11/29/2015 18:10'!
stencil: stencilForm at: aPoint color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels
	
	Display getCanvas stencil: (Form makeStar asFormOfDepth: 1) at: 20@20 color: Color red. Display forceToScreen
	"
	^self stencil: stencilForm
		at: aPoint
		sourceRect: stencilForm boundingBox
		color: aColor! !

!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 11/29/2015 18:12'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor

	| displayRectangle doBorder doFill |
	doBorder _ mbw > 0 and: [ mbc isTransparent not].
	doFill _ morphFillColor isTransparent not.
	doBorder | doFill ifTrue: [
		displayRectangle _ (currentTransformation displayBoundsOfTransformOf: (Rectangle center: mcx@mcy extent: mrx@mry * 2)) truncated.
		"draw the border of the oval"
		doBorder ifTrue: [
			self setPaintColor: mbc.
			port frameOval: displayRectangle borderWidth: mbw].
		"fill the inside"
		doFill ifTrue: [
			self setPaintColor: morphFillColor.
			port fillOval: (displayRectangle insetBy: mbw) ]]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/29/2015 18:01'!
fillRectangle: aRectangle color: aColor
	"Fill the given rectangle."

	aColor isTransparent ifFalse: [
		self 
			frameAndFillRectangle: aRectangle
			fillColor: aColor
			borderWidth: 0
			borderColor: Color transparent ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/29/2015 18:13'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	Display getCanvas fillRectangle: (10@10 extent: 100@100) tilingWith: (BitBltCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red. Display forceToScreen.
	"

	self class accessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		"Warning: aForm boundingBox is most likely different from AuxForm boundingBox!!"
		self fillRectangle: aRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/29/2015 18:04'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect bw |

	r area = 0 ifTrue: [ ^self ].
	rect _ currentTransformation displayBoundsOfTransformOf: r.
	bw _ currentTransformation externalizeScalar: borderWidth.

	"draw the border of the rectangle"
	borderColor isTransparent ifFalse: [
		self setPaintColor: borderColor.
		port frameRect: rect borderWidth: bw ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillRect: (rect insetBy: bw) ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/29/2015 18:14'!
frameRectangle: r borderWidth: borderWidth color: borderColor
	"
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| rect bw |
	(borderWidth > 0 and: [ borderColor isTransparent not ]) ifTrue: [
		rect _ currentTransformation displayBoundsOfTransformOf: r.
		bw _ currentTransformation externalizeScalar: borderWidth.
		self setPaintColor: borderColor.
		port
			frameRect: rect
			borderWidth: bw ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/29/2015 18:14'!
frameRectangle: r color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol
	"
	Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white. Display forceToScreen.
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| displayRectangle bw |
		bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	aSymbol == #raised ifTrue: [
		displayRectangle _ currentTransformation displayBoundsOfTransformOf: r.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteWhiter
			bottomRightColor: aColor quiteBlacker ].

	aSymbol == #inset ifTrue: [
		displayRectangle _ currentTransformation displayBoundsOfTransformOf: r.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteBlacker
			bottomRightColor: aColor quiteWhiter ].
	
	"Unrecognized border style. Draw some border..."
	self frameRectangle: r borderWidth: bw color: aColor! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/29/2015 18:14'!
reverseRectangleBorder: r borderWidth: borderWidth
	"
	Display getCanvas
		reverseRectangleBorder: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	| rect |
	rect _ currentTransformation displayBoundsOfTransformOf: r.
	port
		sourceForm: nil;
		fillColor: Color gray;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth! !

!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
RectangleLikeMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation padding cachedMinExtent doAdoptWidgetsColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
