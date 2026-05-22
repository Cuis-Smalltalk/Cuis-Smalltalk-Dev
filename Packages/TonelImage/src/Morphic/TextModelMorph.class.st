"
This is a morph that enables users to view and edit text.

The main features of this class include:
- using either a TextModel or TextProvider to hold the text being edited
- styling and text wrapping (provided by InnerTextMorph)
- scrolling with scrollbars (provided by superclass PluggableScrollPane)

There are two approaches for creating instances. One approach is to hold the text value in a TextModel object. The class methods withModel: and withText: support this. The other approach is to hold the text value in a TextProvider. The class methods that begin with textProvider: support this. For examples, see the method comments in those class methods.

The default background color is white. A border is only displayed when it has focus. One way to make its bounds apparent when it doesn't have focus is to set the background color of the parent component. Another way is to set the background color of this morph.

By default, words that would extend past the right side wrap to the next line. To prevent wrapping, send an instance #wrapFlag: with an argument of false.

The default size is 200 by 100, set in the initialize method of the superclass PluggableMorph. Depending on the font, the default size displays around four lines of wrapping text with around 17 characters per line. To change the size, send an instance #morphExtent: with a Point argument that specifies a width and height. The size should include space for scroll bars if they may be needed. Setting the height to zero causes it to actually be set to the height required for a single line in the current font.

Related classes include TextModel, TextProvider, PluggableScrollPane, and InnerTextMorph.
"
Class {
	#name : 'TextModelMorph',
	#superclass : 'PluggableScrollPane',
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'instance creation' }
TextModelMorph class >> textProvider: aTextProvider [
	^ self
		textProvider: aTextProvider
		textGetter: nil
]

{ #category : 'instance creation' }
TextModelMorph class >> textProvider: aTextProvider textGetter: getTextSel [
	^ self
		textProvider: aTextProvider
		textGetter: getTextSel
		textSetter: nil
]

{ #category : 'instance creation' }
TextModelMorph class >> textProvider: aTextProvider textGetter: getTextSel textSetter: setTextSel [
	"aTextProvider must have corresponding getter and setter methods
	that manage the text to be edited.
	For example, we can use an instance of the Exception class
	which has the methods messageText and messageText:.

	Select and evaluate the code below.
	In the Inspector that appears, select the messageText instance variable.
	In the TextModelMorph that appears, change the text
	and note how it updates in the Inspector.

	| exception morph |
	exception := Exception new.
	exception messageText: 'initial'.
	exception inspect.
	morph := TextModelMorph textProvider: exception textGetter: #messageText textSetter: #messageText:.
	morph acceptOnAny: true.
	morph openInWorld.
	"
	^ self
		textProvider: aTextProvider
		textGetter: getTextSel
		textSetter: setTextSel
		selectionGetter: nil
]

{ #category : 'instance creation' }
TextModelMorph class >> textProvider: aTextProvider textGetter: textGetter textSetter: textSetter selectionGetter: selectionGetter [
	| newModel answer |
	answer := self new.
	newModel := PluggableTextModel on: aTextProvider.
	newModel textGetter: textGetter textSetter: textSetter selectionGetter: selectionGetter.
	aTextProvider addDependent: newModel.
	answer model: newModel.
	^answer
]

{ #category : 'instance creation' }
TextModelMorph class >> withModel: aTextModel [

	|  answer |
	answer := self new.
	answer model: aTextModel.
	^answer
]

{ #category : 'instance creation' }
TextModelMorph class >> withText: aTextOrString [
	^self withModel: (TextModel withText: aTextOrString)
]

{ #category : 'accept/cancel' }
TextModelMorph >> acceptOnAny: aBoolean  [
	"Accept on any keystroke."
	
	self scroller acceptOnAny: aBoolean 
]

{ #category : 'initialization' }
TextModelMorph >> acceptOnCR: aBoolean [
	scroller acceptOnCR: aBoolean
]

{ #category : 'unaccepted edits' }
TextModelMorph >> askBeforeDiscardingEdits: aBoolean [
	"Set the flag that determines whether the user should be asked before discarding unaccepted edits."

	scroller askBeforeDiscardingEdits: aBoolean
]

{ #category : 'dependents access' }
TextModelMorph >> canDiscardEdits [
	"Return true if this view either has no text changes or does not care."

	^ scroller canDiscardEdits
]

{ #category : 'events' }
TextModelMorph >> clickAndHalf: aMouseButtonEvent localPosition: localEventPosition [

	scroller clickAndHalf: aMouseButtonEvent localPosition: localEventPosition
]

{ #category : 'interactive error protocol' }
TextModelMorph >> correctFrom: start to: stop with: aString [

	^ self editor correctFrom: start to: stop with: aString
]

{ #category : 'initialization' }
TextModelMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ Theme current textPane
]

{ #category : 'accessing' }
TextModelMorph >> disableEditing [
	scroller disableEditing
]

{ #category : 'events' }
TextModelMorph >> doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition [

	scroller doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition
]

{ #category : 'drawing' }
TextModelMorph >> drawOn: aCanvas [
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"
	| bw bc |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	super drawOn: aCanvas.
	bw := Preferences at: #focusIndicatorWidth.
	bc := nil.
	self wantsFrameAdornments ifTrue: [
		model refusesToAccept
			ifTrue: [ "Put up feedback showing that code cannot be saved in this state"
				bc := Color tan ]
			ifFalse: [
				scroller hasEditingConflicts
					ifTrue: [
						bw := 3.
						bc := Color red ]
					ifFalse: [
						scroller hasUnacceptedEdits ifTrue: [ bc := Color red ]]]].
	(drawKeyboardFocusIndicator and: [ scroller hasKeyboardFocus ])
		ifTrue: [ bc ifNil: [ bc := Theme current focusIndicator ]]
		ifFalse: [
			bc ifNotNil: [
				bc := bc
					alphaMixed: 0.4
					with: Color white ]].
	bc ifNotNil: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: bw
			color: bc ].
]

{ #category : 'accessing' }
TextModelMorph >> editor [
	^scroller editor
]

{ #category : 'accessing' }
TextModelMorph >> emptyTextDisplayMessage: aString [
	self
		setProperty: #emptyTextDisplayMessage
		toValue: aString.
]

{ #category : 'accessing' }
TextModelMorph >> enableEditing [

	scroller enableEditing
]

{ #category : 'initialization' }
TextModelMorph >> escAction: aBlock [
	
	scroller escAction: aBlock
]

{ #category : 'focus handling' }
TextModelMorph >> focusText [

	self world activeHand newKeyboardFocus: scroller
]

{ #category : 'focus handling' }
TextModelMorph >> hasKeyboardFocus [
	"For all practical purposes, our inner text having focus means us having focus. No two separated concepts."
	^super hasKeyboardFocus or: [ scroller hasKeyboardFocus ]
]

{ #category : 'unaccepted edits' }
TextModelMorph >> hasUnacceptedEdits: aBoolean [
	"Set the hasUnacceptedEdits flag in my morph."
	scroller hasUnacceptedEdits: aBoolean
]

{ #category : 'geometry' }
TextModelMorph >> innerHeight: aNumber [
	"Adjust height and scrollbar to the new contents height.
	Nothing to do here: TextModelMorph height does not depend on contents height."
]

{ #category : 'initialization' }
TextModelMorph >> innerMorphClass [
	^InnerTextMorph
]

{ #category : 'accessing' }
TextModelMorph >> innerTextMorph [
	^ scroller
]

{ #category : 'event handling' }
TextModelMorph >> keyboardFocusChange: aBoolean [
	"If user clicks or mouseEnters into the blank area not not covered by our inner text morph,
	(meaning no scrollbars, and text being shorter than us), then still give keyboard focus to it."
	
	aBoolean ifTrue: [
		self focusText ].
]

{ #category : 'initialization' }
TextModelMorph >> keystrokeAction: aBlock [
	"Sets the action to perform when user presses any key, after regular processing."
	
	scroller keystrokeAction: aBlock
]

{ #category : 'scrolling' }
TextModelMorph >> mightNeedHorizontalScrollBar [

	scroller isWrapped ifTrue: [ ^false ].

	^super mightNeedHorizontalScrollBar

]

{ #category : 'initialization' }
TextModelMorph >> model: aTextModel [

	super model: aTextModel.
	scroller model: model wrappedTo: self viewableWidth.
	model refetch.
	self setScrollDeltas
]

{ #category : 'events' }
TextModelMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [

	| eventPositionLocalToScroller |
	eventPositionLocalToScroller := localEventPosition - scroller morphPosition.
	scroller mouseButton1Down: aMouseButtonEvent localPosition: eventPositionLocalToScroller.

	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil
]

{ #category : 'event handling' }
TextModelMorph >> mouseButton2Activity [
	"Button 2 presses inside us, but not inside our InnerTextMorph (i.e. scroller) are caught here.
	Forward them to the InnerTextMorph, so they behave the same as if the button press actually occured there."

	scroller mouseButton2Activity
]

{ #category : 'interactive error protocol' }
TextModelMorph >> nextTokenFrom: start direction: dir [
	^ self editor nextTokenFrom: start direction: dir
]

{ #category : 'interactive error protocol' }
TextModelMorph >> notify: aString at: anInteger in: aStream [
	^ self editor notify: aString at: anInteger in: aStream
]

{ #category : 'notifications' }
TextModelMorph >> possiblyChanged [
	"A hook for notificating possible interested parties
	Not used in base system"
]

{ #category : 'geometry' }
TextModelMorph >> scrollDeltaHeight [
	"Return the increment in pixels which this pane should be scrolled."

	^ FontFamily defaultLineSpacing
]

{ #category : 'editor access' }
TextModelMorph >> scrollSelectionIntoView [
	"Scroll my text into view if necessary and return true, else return false"

	| delta |
	delta := scroller morphPosition.
	self editor pointIndex > 1
		ifTrue: [
			self scrollToShow: (self editor pointBlock translatedBy: delta) ]
		ifFalse: [
			self scrollToShow: (self editor selectionRectangle translatedBy: delta) ]
]

{ #category : 'editor access' }
TextModelMorph >> selectAll [
	"Tell my textMorph's editor to select all"

	scroller selectAll
]

{ #category : 'interactive error protocol' }
TextModelMorph >> selectFrom: start to: stop [
	^ self editor selectFrom: start to: stop
]

{ #category : 'interactive error protocol' }
TextModelMorph >> selectInvisiblyFrom: start to: stop [
	^ self editor selectInvisiblyFrom: start to: stop
]

{ #category : 'interactive error protocol' }
TextModelMorph >> selectionInterval [
	^ self editor selectionInterval
]

{ #category : 'model access' }
TextModelMorph >> setSelection: sel [

	sel == #all
		ifTrue: [ self editor selectAll ]
		ifFalse: [
			sel
				ifNil: [ self editor selectFrom:1 to: 0 ]
				ifNotNil: [ self editor selectFrom: sel first to: sel last ]].
	self scrollSelectionIntoView
]

{ #category : 'model access' }
TextModelMorph >> setTextColor: aColor [
	"Set the color of my text to the given color"

	scroller color: aColor
]

{ #category : 'editor access' }
TextModelMorph >> startIndex [
	
	^self editor startIndex 
]

{ #category : 'focus handling' }
TextModelMorph >> tabToFocus: aBoolean [
	"Enable or disable #tabToFocus.
	Disabled means that tab is valid character input."
	scroller tabToFocus: aBoolean
]

{ #category : 'model access' }
TextModelMorph >> text [
	"The returned object should be treated as read-only, and never modified"
	^ model actualContents
]

{ #category : 'updating' }
TextModelMorph >> update: aSymbol [

	super update: aSymbol.
	aSymbol ifNil: [^self].

	aSymbol == #flash ifTrue: [^self flash].
	aSymbol == #actualContents ifTrue: [ ^self updateActualContents ].
	aSymbol == #acceptedContents ifTrue: [ ^self updateAcceptedContents ].
	aSymbol == #refetched ifTrue: [ ^self updateRefetched ].
	aSymbol == #initialSelection ifTrue: [ ^self updateInitialSelection ].
	aSymbol == #clearUserEdits ifTrue: [ ^self updateClearUserEdits ].
	aSymbol == #shoutStyled ifTrue: [ ^self updateShoutStyled ].

]

{ #category : 'updating' }
TextModelMorph >> updateAcceptedContents [

	scroller hasUnacceptedEdits ifTrue: [
		scroller hasEditingConflicts: true.
		^self redrawNeeded ].
	model refetch.
	"#actualContents also signalled in #refetch. No need to repeat what's done there."
	^self 
]

{ #category : 'updating' }
TextModelMorph >> updateActualContents  [

	"Some day, it would be nice to keep objects and update them
	instead of throwing them away all the time for no good reason..."
	scroller
		releaseEditorAndTextComposition;
		installEditorAndTextComposition;
		convertAndStyleIfNeeded.
	self setScrollDeltas.
	self redrawNeeded.
	^self 
]

{ #category : 'updating' }
TextModelMorph >> updateClearUserEdits [
	
	"Quite ugly"
	^self hasUnacceptedEdits: false

]

{ #category : 'updating' }
TextModelMorph >> updateInitialSelection [

	^self 
		setSelection: model getSelection; 
		redrawNeeded
]

{ #category : 'updating' }
TextModelMorph >> updateRefetched  [

	"#actualContents also signalled when #refetched is signalled.
	No need to repeat what's done there."
	self setSelection: model getSelection.
	self hasUnacceptedEdits: false.
	
	^self 
]

{ #category : 'updating' }
TextModelMorph >> updateShoutStyled [

	scroller stylerStyled.
	^self redrawNeeded 
]

{ #category : 'drawing' }
TextModelMorph >> wantsFrameAdornments [
	^model wantsFrameAdornments
]

{ #category : 'accessing' }
TextModelMorph >> wrapFlag: aBoolean [
	scroller wrapFlag: aBoolean
]
