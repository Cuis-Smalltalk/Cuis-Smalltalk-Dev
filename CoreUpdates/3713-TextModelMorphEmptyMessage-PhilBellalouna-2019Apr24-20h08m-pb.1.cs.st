'From Cuis 5.0 of 7 November 2016 [latest update: #3707] on 24 April 2019 at 9:22:51 pm'!

!TextModelMorph methodsFor: 'accessing' stamp: 'pb 4/24/2019 20:32:26'!
emptyTextDisplayMessage: aString
	self
		setProperty: #emptyTextDisplayMessage
		toValue: aString.! !


!FillInTheBlankMorph methodsFor: 'accessing' stamp: 'pb 4/24/2019 21:14:06'!
emptyTextDisplayMessage: aString
	self
		setProperty: #emptyTextDisplayMessage
		toValue: aString.! !


!Theme methodsFor: 'colors' stamp: 'pb 4/24/2019 20:46:25'!
textEmptyDisplayMessage
	^ `Color veryLightGray`! !


!Browser methodsFor: 'class functions' stamp: 'pb 4/24/2019 20:20:32'!
classCommentText
	"return the text to display for the comment of the currently selected class"
	| theClass |
	theClass _ self selectedClassOrMetaClass.
	^ Text
		initialFont: Preferences standardCodeFont
		stringOrText:
			((theClass notNil and: [ theClass hasComment ])
				ifTrue: [ theClass comment ]
				ifFalse: [ '' ]).! !


!MessageNames methodsFor: 'initialization' stamp: 'pb 4/24/2019 20:40:35'!
initialize
	super initialize.
	searchString _ ''! !


!TextModelMorph methodsFor: 'drawing' stamp: 'pb 4/24/2019 20:57:54'!
drawOn: aCanvas
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"
	| bw bc |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	super drawOn: aCanvas.
	bw _ Preferences focusIndicatorWidth.
	bc _ nil.
	self wantsFrameAdornments ifTrue: [
		model refusesToAccept
			ifTrue: [ "Put up feedback showing that code cannot be submitted in this state"
				bc _ Color tan ]
			ifFalse: [
				self textMorph hasEditingConflicts
					ifTrue: [
						bw _ 3.
						bc _ Color red ]
					ifFalse: [
						self textMorph hasUnacceptedEdits ifTrue: [ bc _ Color red ]]]].
	(drawKeyboardFocusIndicator and: [ self textMorph hasKeyboardFocus ])
		ifTrue: [ bc ifNil: [ bc _ Theme current focusIndicator ]]
		ifFalse: [
			bc ifNotNil: [
				bc _ bc
					alphaMixed: 0.4
					with: Color white ]].
	bc ifNotNil: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: bw
			color: bc ].
	model actualContents isEmpty ifTrue: [
		self
			valueOfProperty: #emptyTextDisplayMessage
			ifPresentDo: [ :msg |
				aCanvas
					drawString: msg
					at: `0 @ 0`
					font: nil
					color: Theme current textEmptyDisplayMessage ]].! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'pb 4/24/2019 20:22:29'!
buildMorphicCommentPane
	"Construct the pane that shows the class comment."
	 ^ (BrowserCommentTextMorph
		textProvider: model
		textGetter: #classCommentText
		textSetter: #newClassComment:) emptyTextDisplayMessage: 'THIS CLASS HAS NO COMMENT!!'.! !


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'pb 4/24/2019 20:40:18'!
buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"

"MessageNames openMessageNames"

	| selectorListView firstRow searchButton secondRow |
	textMorph _ (TextModelMorph
		textProvider: model
		textGetter: #searchString 
		textSetter: #searchString:
		selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'type here, then hit Search'.
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


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'pb 4/24/2019 21:15:30'!
createTextPaneAcceptOnCR: acceptBoolean
	"create the textPane"
	| result |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	result _ TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval.
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


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'pb 4/24/2019 21:19:42'!
initializedInstance
	| aFillInTheBlankMorph |
	aFillInTheBlankMorph _ self new
		emptyTextDisplayMessage: 'Enter answer here';
		
			setQuery: 'queryString'
			initialAnswer: ''
			acceptOnCR: true.
	aFillInTheBlankMorph responseUponCancel: 'returnOnCancel'.
	^ aFillInTheBlankMorph.! !

