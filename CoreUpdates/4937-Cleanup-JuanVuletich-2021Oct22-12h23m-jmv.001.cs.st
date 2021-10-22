'From Cuis 5.0 [latest update: #4935] on 22 October 2021 at 12:27:13 pm'!

!TextModelMorph methodsFor: 'accessing' stamp: 'jmv 10/22/2021 12:14:38'!
innerTextMorph
	^ scroller! !


!TextModelMorph methodsFor: 'accessing' stamp: 'jmv 10/22/2021 11:53:17'!
disableEditing
	scroller disableEditing! !

!TextModelMorph methodsFor: 'accessing' stamp: 'jmv 10/22/2021 11:53:38'!
editor
	^scroller editor! !

!TextModelMorph methodsFor: 'accessing' stamp: 'jmv 10/22/2021 11:53:41'!
enableEditing

	scroller enableEditing! !

!TextModelMorph methodsFor: 'accessing' stamp: 'jmv 10/22/2021 11:54:30'!
wrapFlag: aBoolean
	scroller wrapFlag: aBoolean! !

!TextModelMorph methodsFor: 'dependents access' stamp: 'jmv 10/22/2021 11:53:12'!
canDiscardEdits
	"Return true if this view either has no text changes or does not care."

	^ scroller canDiscardEdits! !

!TextModelMorph methodsFor: 'drawing' stamp: 'jmv 10/22/2021 11:53:33'!
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
				scroller hasEditingConflicts
					ifTrue: [
						bw _ 3.
						bc _ Color red ]
					ifFalse: [
						scroller hasUnacceptedEdits ifTrue: [ bc _ Color red ]]]].
	(drawKeyboardFocusIndicator and: [ scroller hasKeyboardFocus ])
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
			color: bc ].! !

!TextModelMorph methodsFor: 'editor access' stamp: 'jmv 10/22/2021 11:54:07'!
scrollSelectionIntoView
	"Scroll my text into view if necessary and return true, else return false"

	| delta |
	delta _ scroller morphPosition.
	self editor pointIndex > 1
		ifTrue: [
			self scrollToShow: (self editor pointBlock translatedBy: delta) ]
		ifFalse: [
			self scrollToShow: (self editor selectionRectangle translatedBy: delta) ]! !

!TextModelMorph methodsFor: 'editor access' stamp: 'jmv 10/22/2021 11:54:10'!
selectAll
	"Tell my textMorph's editor to select all"

	scroller selectAll! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 10/22/2021 11:53:15'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	scroller clickAndHalf: aMouseButtonEvent localPosition: localEventPosition! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 10/22/2021 11:53:20'!
doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	scroller doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 10/22/2021 11:53:52'!
keyStroke: aKeyboardEvent
	"A keystroke was hit while the receiver had keyboard focus.  Pass the keywtroke on to my textMorph, and and also, if I have an event handler, pass it on to that handler"

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	scroller keyStroke: aKeyboardEvent! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 10/22/2021 11:54:04'!
mouseEnter: event
	super mouseEnter: event.
	Preferences focusFollowsMouse
		ifTrue: [ event hand newKeyboardFocus: scroller ]! !

!TextModelMorph methodsFor: 'event handling' stamp: 'jmv 10/22/2021 11:54:02'!
mouseButton2Activity
	scroller mouseButton2Activity! !

!TextModelMorph methodsFor: 'focus handling' stamp: 'jmv 10/22/2021 11:53:47'!
focusText

	self world activeHand newKeyboardFocus: scroller! !

!TextModelMorph methodsFor: 'initialization' stamp: 'jmv 10/22/2021 11:53:05'!
acceptOnCR: aBoolean
	scroller acceptOnCR: aBoolean! !

!TextModelMorph methodsFor: 'initialization' stamp: 'jmv 10/22/2021 11:53:43'!
escAction: aBlock
	
	scroller escAction: aBlock! !

!TextModelMorph methodsFor: 'initialization' stamp: 'jmv 10/22/2021 11:53:59'!
model: aTextModel

	super model: aTextModel.
	scroller model: model wrappedTo: self viewableWidth.
	model refetch.
	self setScrollDeltas! !

!TextModelMorph methodsFor: 'model access' stamp: 'jmv 10/22/2021 11:54:13'!
setTextColor: aColor
	"Set the color of my text to the given color"

	scroller color: aColor! !

!TextModelMorph methodsFor: 'scrolling' stamp: 'jmv 10/22/2021 11:53:55'!
mightNeedHorizontalScrollBar

	scroller isWrapped ifTrue: [ ^false ].

	^super mightNeedHorizontalScrollBar
! !

!TextModelMorph methodsFor: 'unaccepted edits' stamp: 'jmv 10/22/2021 11:53:09'!
askBeforeDiscardingEdits: aBoolean
	"Set the flag that determines whether the user should be asked before discarding unaccepted edits."

	scroller askBeforeDiscardingEdits: aBoolean! !

!TextModelMorph methodsFor: 'unaccepted edits' stamp: 'jmv 10/22/2021 11:53:50'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag in my morph."
	scroller hasUnacceptedEdits: aBoolean! !

!TextModelMorph methodsFor: 'updating' stamp: 'jmv 10/22/2021 11:54:19'!
updateAcceptedContents

	scroller hasUnacceptedEdits ifTrue: [
		scroller hasEditingConflicts: true.
		^self redrawNeeded ].
	model refetch.
	"#actualContents also signalled in #refetch. No need to repeat what's done there."
	^self ! !

!TextModelMorph methodsFor: 'updating' stamp: 'jmv 10/22/2021 11:54:22'!
updateActualContents 

	"Some day, it would be nice to keep objects and update them
	instead of throwing them away all the time for no good reason..."
	scroller
		releaseEditorAndTextComposition;
		installEditorAndTextComposition;
		formatAndStyleIfNeeded.
	self setScrollDeltas.
	self redrawNeeded.
	^self ! !

!TextModelMorph methodsFor: 'updating' stamp: 'jmv 10/22/2021 11:54:24'!
updateAutoSelect 

	TextEditor abandonChangeText.	"no replacement!!"
	self selectMessage
		ifFalse: [ self selectString ].

	scroller updateFromTextComposition.
	^self scrollSelectionIntoView! !

!TextModelMorph methodsFor: 'updating' stamp: 'jmv 10/22/2021 11:54:26'!
updateShoutStyled

	scroller stylerStyled.
	^self redrawNeeded ! !


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'jmv 10/22/2021 12:14:51'!
buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"
	"MessageNames openMessageNames"
	| selectorListView firstRow searchButton secondRow |
	textMorph _ (TextModelMorph
		textProvider: model
		textGetter: #searchString
		textSetter: #searchString:
		selectionGetter: #contentsSelection)
		setBalloonText: 'See MessageNames class comment for search string options';
		emptyTextDisplayMessage: 'Type here, then hit Search'.
	textMorph askBeforeDiscardingEdits: false.
	textMorph acceptOnCR: true.
	textMorph hideScrollBarsIndefinitely.
	searchButton _ PluggableButtonMorph new
		model: textMorph innerTextMorph;
		label: 'Search';
		action: #acceptContents.
	searchButton setBalloonText: 'Type some letters into the pane at right, and then press this Search button (or hit RETURN) and all method selectors that match what you typed will appear in the list pane below.  Click on any one of them, and all the implementors of that selector will be shown in the right-hand pane, and you can view and edit their code without leaving this tool.'.
	firstRow _ LayoutMorph newRow.
	firstRow color: self windowColor.
	firstRow
		doAdoptWidgetsColor;
		
			addMorph: searchButton
			proportionalWidth: 0.25;
		
			addMorph: textMorph
			proportionalWidth: 0.75.
	selectorListView _ PluggableListMorph
		model: model
		listGetter: #selectorList
		indexGetter: #selectorListIndex
		indexSetter: #selectorListIndex:
		mainView: self
		menuGetter: #selectorListMenu
		keystrokeAction: #selectorListKey:from:.
	secondRow _ LayoutMorph newRow.
	secondRow
		
			addMorph: selectorListView
			proportionalWidth: 0.5;
		
			addAdjusterAndMorph: self buildMorphicMessageList
			proportionalWidth: 0.5.
	self layoutMorph
		
			addMorph: firstRow
			fixedHeight: self defaultButtonPaneHeight + 4;
		
			addAdjusterAndMorph: secondRow
			proportionalHeight: 0.5;
		
			addAdjusterAndMorph: self buildLowerPanes
			proportionalHeight: 0.5.
	model changed: #editSelection.! !

!MessageNamesWindow methodsFor: 'GUI building' stamp: 'jmv 10/22/2021 12:14:54'!
submorphToFocusKeyboard
	^textMorph innerTextMorph! !


!FillInTheBlankMorph methodsFor: 'menu' stamp: 'jmv 10/22/2021 12:15:00'!
acceptClicked
	"Sent by the accept button."

	textPane innerTextMorph acceptContents! !

!methodRemoval: TextModelMorph #textMorph stamp: 'jmv 10/22/2021 12:26:56'!
TextModelMorph removeSelector: #textMorph!
