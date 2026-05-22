"
InnerTextMorphs support display of text with emphasis.  They also support reasonable text-editing capabilities, as well as embedded hot links, and the ability to embed submorphs in the text. They are 'bare' in the sense that they can not clip contents to some window, or scroll it by themselves.

Text display is composed and clipped to the innerBounds implied by borderWidth.

Comment about Shout specifics:
-----------------------------------------

In order to colour the text, I use an instance of SHTextStylerST80, which I store in my 'styler' instance variable.

When my setText: method is called, I use my styler to ...
	a) optionally set all assignments to ansi or leftArrow. 
	b) Colour my text (immediately, if the text is less than 4096 chars in length, or in a backgroundProcess otherwise)

	When my text is changed, my hasUnacceptedEdits: method is called with true, and I ask my styler to re-colour my text. This is performed in a background process so that typing remains responsive regardless of the length of the text.

	Just before my styler is about to format/style the text, I send #stylerAboutToStyle:  to my model. This gives my model a chance to veto the styling (by answering false), or to initialize the styler with information it needs in order to parse the text correctly (e.g. the class to which a method belongs, or the workspace in which I am contained).

	My styler informs me that it has finished styling by triggering the #shoutStyled event which I handle. I then update the textAttributes of my text and refresh the display.
 
	My 'unstyledAcceptText' instance variable is used in conjunction with my #acceptTextInModel and #correctFrom:to:with: methods to ensure that when my text is modified during a method compilation (removing unused vars etc), I do not lose those changes.
"
Class {
	#name : 'InnerTextMorph',
	#superclass : 'InnerPluggableMorph',
	#instVars : [
		'model',
		'wrapFlag',
		'textComposition',
		'editor',
		'pauseBlinking',
		'acceptOn',
		'hasUnacceptedEdits',
		'hasEditingConflicts',
		'askBeforeDiscardingEdits',
		'styler',
		'autoCompleter',
		'mutex',
		'needsFit'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'editing' }
InnerTextMorph >> aboutToEdit [

	styler ifNotNil: [ styler terminateBackgroundStylingProcess ]
]

{ #category : 'editing' }
InnerTextMorph >> acceptContents [
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| accepted prevSelection prevScrollValue |
	
	prevSelection := self editor selectionInterval copy.
	prevScrollValue := owner verticalScrollBar scrollValue.
	
	hasUnacceptedEdits ifFalse: [ self flash. ^true ].
	hasEditingConflicts ifTrue: [
		self confirmAcceptAnyway ifFalse: [self flash. ^false]].
	
	accepted := model acceptContentsFrom: owner.
	"During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the code-holding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
	
	"some implementors of acceptContentsFrom: answer self :("
	^accepted == true 
		ifTrue: [
			model refetch.
			self editor selectFrom: prevSelection first to: prevSelection last.
			UISupervisor whenUIinSafeState: [
				"The next line would make PopUpMenu to auto close because of losing focus..."
				"self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ]."
				(owner is: #ScrollPane) ifTrue: [
					owner verticalScrollBar internalScrollValue: prevScrollValue]].
			true]
		ifFalse: [ false ]
]

{ #category : 'accept/cancel' }
InnerTextMorph >> acceptOnAny: aBoolean [
	aBoolean
		ifTrue: [ acceptOn := #any ]
		ifFalse: [
			acceptOn = #any
				ifTrue: [ acceptOn := #none ]].
]

{ #category : 'editing' }
InnerTextMorph >> acceptOnCR [
	"Answer whether the receiver wants to accept when the Return key is hit"

	^ acceptOn == #enter
]

{ #category : 'accept/cancel' }
InnerTextMorph >> acceptOnCR: aBoolean [
	aBoolean
		ifTrue: [ acceptOn := #enter ]
		ifFalse: [
			acceptOn = #enter
				ifTrue: [ acceptOn := #none ]].
]

{ #category : 'menu' }
InnerTextMorph >> addCustomMenuItems: aCustomMenu hand: aHandMorph  [
	"Add text-related menu items to the menu"

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu 
		addUpdating: #wrapString
		target: self
		action: #wrapOnOff
]

{ #category : 'submorphs-add/remove' }
InnerTextMorph >> addMorphFrontFromWorldPosition: aMorph [
	"Overridden for more specific re-layout and positioning"
	| positionInWorld |
	positionInWorld := aMorph positionInWorld.
	^self anchorMorph: aMorph at: positionInWorld
]

{ #category : 'geometry' }
InnerTextMorph >> adjustExtent [
	"This is just a suggestion. If we do wordwrap, the width will be honored.
	But the height is whatever is appropriate for the contents! See #fit"
	self morphExtent: owner viewableExtent
]

{ #category : 'anchors' }
InnerTextMorph >> anchorMorph: aMorph at: aPoint [
	| relPt |
	aMorph owner == self ifTrue: [ self removeMorph: aMorph ].
	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Texts' ].
	self addMorphFront: aMorph.
	relPt := aPoint - self positionInWorld.
	editor insertMorph: aMorph at: relPt.
	self fit.
]

{ #category : 'accessing' }
InnerTextMorph >> askBeforeDiscardingEdits: aBoolean [
	"Set the flag that determines whether the user should be asked before discarding unaccepted edits."

	askBeforeDiscardingEdits := aBoolean
]

{ #category : 'private' }
InnerTextMorph >> autoCompleterClass: aTextCompleterClass [
	autoCompleter class == aTextCompleterClass ifFalse: [
		autoCompleter := aTextCompleterClass ifNotNil: [
			aTextCompleterClass withModel: model ].
		autoCompleter ifNotNil: [
			autoCompleter textMorph: self ]]
]

{ #category : 'testing' }
InnerTextMorph >> canDiscardEdits [
	"Return true if this view either has no text changes or does not care."
	^ (hasUnacceptedEdits & askBeforeDiscardingEdits) not
]

{ #category : 'events' }
InnerTextMorph >> clickAndHalf: aMouseButtonEvent localPosition: localEventPosition [

	self handleInteraction: [
		editor clickAndHalfAt: localEventPosition ].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].
]

{ #category : 'event handling testing' }
InnerTextMorph >> clickToFocusConsumesEvent [
	"Only relevant if #focusFollowsMouse == false.
	According to the preference #clickToFocusClickToSelect:
	- #always: Require one click to focus, then another to select.
	- #never: A single click sets focus and the sets or clears selection.
	- #ifSelection: We don't react to a focus setting click if we have a selection,
					so we don't lose selection just for focusing."

	| prefSymbol | 
	prefSymbol := Preferences at: #clickToFocusClickToSelect.
	prefSymbol == #never ifTrue: [ ^false ].
	prefSymbol == #always ifTrue: [ ^true ].
	"Assume #ifSelection"
	^editor hasSelection
]

{ #category : 'accessing' }
InnerTextMorph >> codeProvider [
	"Return my Text Provider."
	editor ifNil: [ self installEditorAndTextComposition ].
	^editor codeProvider
]

{ #category : 'editing' }
InnerTextMorph >> confirmAcceptAnyway [

	^ self confirm: 
'Caution! Contents were saved
elsewhere since you started
editing them here.  Accept anyway?'
]

{ #category : 'accessing' }
InnerTextMorph >> contents: stringOrText [
	^ self contentsAsIs: stringOrText
]

{ #category : 'accessing' }
InnerTextMorph >> contentsAsIs: stringOrText [
	"Accept new text contents with line breaks only as in the text.
	Fit my width and height to the result."
	wrapFlag := false.
	model basicActualContents: stringOrText
]

{ #category : 'shout' }
InnerTextMorph >> convertAndStyleIfNeeded [
	"Apply both converting (changes to the characters in the text, such as 	preferred assignment operators),
	and styling (TextAttributes to make Smalltalk code easier to understand)"

	| previousSize delta isAtEnd |
	previousSize := model actualContents size.
	isAtEnd := editor stopIndex > previousSize.
	model stylesText ifTrue: [
		model convertAndStyleIfNeededWith: styler ].
	delta := model actualContents size - previousSize.
	(isAtEnd and: [ delta < 0 ]) ifTrue: [
		delta := 0].
	delta = 0 ifFalse: [
		editor moveCursor: [ :p | p+delta] forward: delta>0 event: nil ].
]

{ #category : 'accessing' }
InnerTextMorph >> crAction [
	"Return the action to perform when user presses <Return> key"
	^self valueOfProperty: #crAction
]

{ #category : 'accessing' }
InnerTextMorph >> crAction: aBlock [
	"Sets the action to perform when user presses <Return> key"
	self setProperty: #crAction toValue: aBlock
]

{ #category : 'drawing' }
InnerTextMorph >> debugDrawLineRectsOn: aCanvas [
	"Shows where text line rectangles are"

	self textComposition lines do: [ :line |
		aCanvas
			frameRectangle: line rectangle
			borderWidth: 1
			color: `Color brown` ]

]

{ #category : 'initialization' }
InnerTextMorph >> defaultColor [
	"Return the default fill style for the receiver"
	^ Theme current text
]

{ #category : 'accessing' }
InnerTextMorph >> disableEditing [
	self setProperty: #disablesEditing toValue: true.
	self stopBlinking
]

{ #category : 'event handling testing' }
InnerTextMorph >> disablesEditing [

	^self hasProperty: #disablesEditing
]

{ #category : 'miscellaneous' }
InnerTextMorph >> disregardUnacceptedEdits [

	needsFit :=​ false.
	hasUnacceptedEdits :=​ false.
	hasEditingConflicts :=​ false.
]

{ #category : 'events' }
InnerTextMorph >> doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition [

	self handleInteraction: [
		editor doubleClickAndHalf ].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].
]

{ #category : 'drawing' }
InnerTextMorph >> drawOn: aCanvas [
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		drawTextComposition: self textComposition
		at: `0@0`
		extent: extent
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus).

	model actualContents isEmpty ifTrue: [
		owner
			valueOfProperty: #emptyTextDisplayMessage
			ifPresentDo: [ :msg |
				aCanvas
					drawString: msg
					at: `0@0`
					font: nil
					color: Theme current textEmptyDisplayMessage ]].
]

{ #category : 'accessing' }
InnerTextMorph >> editor [
	"Return my current editor, or install a new one."
	editor ifNil: [ self installEditorAndTextComposition ].
	^editor
]

{ #category : 'accessing' }
InnerTextMorph >> enableEditing [
	
	self removeProperty: #disablesEditing
]

{ #category : 'editing' }
InnerTextMorph >> enterClickableRegion: aMorphicEvent localPosition: localEventPosition [

]

{ #category : 'accessing' }
InnerTextMorph >> escAction [
	"Return the action to perform when user presses <Esc> key"

	^self valueOfProperty: #escAction
]

{ #category : 'accessing' }
InnerTextMorph >> escAction: aBlock [
	"Sets the action to perform when user presses <Esc> key"

	self setProperty: #escAction toValue: aBlock.
]

{ #category : 'geometry' }
InnerTextMorph >> extentChanged: oldExtent [
	"Our extent changed. Reflow text."
	super extentChanged: oldExtent.
	model ifNotNil: [
		wrapFlag ifTrue: [
			extent x = oldExtent x ifFalse: [
				self resetTextComposition.
				self editor recomputeSelection.	
				self updateFromTextComposition.
				(owner is: #ScrollPane) ifTrue: [
					owner
						updateScrollBarsBounds;
						setScrollDeltas ]]]].
]

{ #category : 'private' }
InnerTextMorph >> extentForComposing [

	^wrapFlag
		ifTrue: [ extent x @ 9999999 ]
		ifFalse: [ 9999999@9999999 ]
]

{ #category : 'private' }
InnerTextMorph >> fit [
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth oldExtent |
	(owner is: #ScrollPane) ifFalse: [
		^self ].

	newWidth := extent x.
	"Adjust width only if we don't wrap text to own width!"
	wrapFlag ifFalse: [
		newWidth := self textComposition usedWidth + 3 max: 9 ].
	newHeight := self textComposition usedHeight max: FontFamily defaultLineSpacing + 2.
	newExtent := newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
			self redrawNeeded ]
		ifFalse: [
			oldExtent := extent.
			extent := newExtent.
			self extentChanged: oldExtent.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ].

	owner innerHeight: newExtent y.
]

{ #category : 'miscellaneous' }
InnerTextMorph >> flash [
	^ owner flash
]

{ #category : 'notifications' }
InnerTextMorph >> fontPreferenceChanged [

	super fontPreferenceChanged.
	hasUnacceptedEdits
		ifTrue: [ model basicActualContents: model actualContents ]
		ifFalse: [ model refetch ].
	self updateFromTextComposition.
]

{ #category : 'editing' }
InnerTextMorph >> handleInteraction: interactionBlock [
	"Perform the changes in interactionBlock, noting any change in selection
	and possibly a change in the composition"

	self selectionChanged.  "Note old selection"

	interactionBlock value.

	self selectionChanged.  "Note new selection"
	self updateFromTextComposition
]

{ #category : 'event handling testing' }
InnerTextMorph >> handlesKeyboardFocus [
	"Not if we are read-only"

	^self disablesEditing not
]

{ #category : 'event handling testing' }
InnerTextMorph >> handlesMouseDown: aMouseButtonEvent [
	^ true
]

{ #category : 'accessing' }
InnerTextMorph >> hasEditingConflicts [
	"Return true if a conflicting edit to the same code (typically) is known to have occurred after the current contents started getting edited"

	^ hasEditingConflicts
]

{ #category : 'accessing' }
InnerTextMorph >> hasEditingConflicts: aBoolean [

	hasEditingConflicts := aBoolean
]

{ #category : 'testing' }
InnerTextMorph >> hasUnacceptedEdits [
	^hasUnacceptedEdits
]

{ #category : 'editing' }
InnerTextMorph >> hasUnacceptedEdits: aBoolean [
	"Set the hasUnacceptedEdits flag to the given value. "
	
	(aBoolean or: hasUnacceptedEdits) ifTrue: [
		self convertAndStyleIfNeeded ].
	needsFit := aBoolean.
	
	aBoolean == hasUnacceptedEdits ifFalse: [
		hasUnacceptedEdits := aBoolean].
	aBoolean ifFalse: [ hasEditingConflicts := false].

]

{ #category : 'initialization' }
InnerTextMorph >> initialize [
	super initialize.
	wrapFlag := true.
	acceptOn := #none.
	hasUnacceptedEdits := false.
	hasEditingConflicts := false.
	askBeforeDiscardingEdits := true.
	needsFit := false.
]

{ #category : 'private' }
InnerTextMorph >> installEditorAndTextComposition [
	"Install an editor for my textComposition. Install also the textComposition."
	| e tc |
	
	"Editor and TextComposition are assigned here atomically."
	e := model editorClass new morph: self.
	e model: model.
	tc := TextComposition new.
	"Keep critical section short"
	self mutex critical: [
		editor := e.
		textComposition := tc.
		tc
			setModel: model;
			extentForComposing: self extentForComposing.
		e textComposition: tc.
		tc editor: e ].
	e setEmphasisHereFromText.
	tc composeAll.
	e resetState.
	self fit.
	self selectionChanged.
	self possiblyChanged.

	"Add extras. Text Styler and Autocompleter"
	self stylerClass: model textStylerClass.
	self autoCompleterClass: model autoCompleterClass.
]

{ #category : 'classification' }
InnerTextMorph >> is: aSymbol [
	^ aSymbol == #InnerTextMorph or: [ super is: aSymbol ]
]

{ #category : 'accessing' }
InnerTextMorph >> isWrapped [
	
	^wrapFlag
]

{ #category : 'events' }
InnerTextMorph >> keyStroke: aKeyboardEvent [

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ 
		self selectionChanged.  "Note new selection"
		self updateFromTextComposition.
		^self ].

	autoCompleter 
		ifNil: [ self processKeystrokeEvent: aKeyboardEvent ]
		ifNotNil: [
			autoCompleter
				autoCompletionAround: [ self processKeystrokeEvent: aKeyboardEvent ]
				keyStroke: aKeyboardEvent ].

	aKeyboardEvent wasHandled: true.

]

{ #category : 'event handling' }
InnerTextMorph >> keyboardFocusChange: aBoolean [
	
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw border feedback"
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor storeSelectionInComposition ].	"Forces install"
			(self disablesEditing not and: [ editor hasSelection not ]) ifTrue: [
				self startBlinking ]]
		ifFalse: [ self stopBlinking ].
	
	(owner notNil and: [ owner drawsKeyboardFocusIndicator ])
		ifTrue: [ owner redrawNeeded ]
		ifFalse: [
			"Selection might be shown differently when focused"
			self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ]]	.
]

{ #category : 'events' }
InnerTextMorph >> keyboardShortcut: aKeyboardEvent [
	"Handle a keyboard shortcut.
	See coment at Morph >> #getMenu
	"
	"ctrl and option (if on Mac) are used to enter #normalCharacter:
	Only cmd is considered a shortcut generating modifier."
	^(aKeyboardEvent commandAltKeyPressed and: [aKeyboardEvent keyValue between: 32 and: 126])
		ifTrue: [super keyboardShortcut: aKeyboardEvent]
		ifFalse: [false].
]

{ #category : 'accessing' }
InnerTextMorph >> keystrokeAction: aBlock [
	"Sets the action to perform when user presses any key, after regular processing."

	self setProperty: #keyStroke: toValue: aBlock.
]

{ #category : 'menu' }
InnerTextMorph >> menuNshortcutsSpecsDo: aBlock [
	"Iterate over our own menu and shortcut specs.
	Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for themselves.
	See implementors."

	"First take default editor menu and shortcut specs"
	self editor defaultMenuSpec do: aBlock.

	self window ifNotNil: [ :myWindow |

		"Then include additions, overrides and deletions registered for #Editor, #TextEditor, #SmalltalkEditor, etc.
		Hierarchy order allows more specific classes to override choices made by superclasses."
		(self editor class withAllSuperclassesDownFrom: Editor) do: [ :editorClass |
			myWindow allRegisteredMenuAndShortcutsSpecProviders: editorClass name do: aBlock widget: self ].

		"Also allow registering menu+shortcut entries for specific TextModels or TextProviders"
		myWindow
			allRegisteredMenuAndShortcutsSpecProviders: self
			textProviderOrModel class name
			do: aBlock
			widget: self ].
]

{ #category : 'geometry' }
InnerTextMorph >> minimumExtent [

	^(9@(FontFamily defaultLineSpacing+2))
]

{ #category : 'editing' }
InnerTextMorph >> model [
	^model
]

{ #category : 'accessing' }
InnerTextMorph >> model: aTextModel [
	model := aTextModel.
	styler ifNotNil: [ styler textModel: model ].
	self releaseEditorAndTextComposition.	"So the model is properly set on the editor and the text composition"
]

{ #category : 'accessing' }
InnerTextMorph >> model: aTextModel wrappedTo: width [
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	| newExtent oldExtent |
	wrapFlag := true.
	model := aTextModel.
	newExtent := width truncated@extent y.
	extent = newExtent ifFalse: [
		oldExtent := extent.
		extent := newExtent.
		self extentChanged: oldExtent.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].
	self model: aTextModel
]

{ #category : 'events' }
InnerTextMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	self handleInteraction: [ editor mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil
]

{ #category : 'events' }
InnerTextMorph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [
	| mouseButton1DownHandled |

	mouseButton1DownHandled := self 
		valueOfProperty: #mouseButton1Up:localPosition:
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]
		ifAbsent: [ false ].

	mouseButton1DownHandled ifFalse: [
		self handleInteraction: [ editor mouseButton1Up: aMouseButtonEvent  localPosition: localEventPosition ]].

	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ]
]

{ #category : 'events' }
InnerTextMorph >> mouseMove: aMouseMoveEvent localPosition: localEventPosition [

	aMouseMoveEvent mouseButton1Pressed ifFalse: [
		^ self enterClickableRegion: aMouseMoveEvent localPosition: localEventPosition ].
	self handleInteraction: [
		editor mouseMove: aMouseMoveEvent localPosition: localEventPosition].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].
]

{ #category : 'private' }
InnerTextMorph >> mutex [
	mutex
		ifNil: [ mutex := Mutex new ].
	^mutex
]

{ #category : 'copying' }
InnerTextMorph >> okayToDuplicate [
	"Answered false by morphs that can't simply be duplicated"

	^ false
]

{ #category : 'blinking cursor' }
InnerTextMorph >> onBlinkCursor [
	"Blink the cursor"
	textComposition ifNil: [ ^nil ].
	textComposition showTextCursor: textComposition showTextCursor not | pauseBlinking.
	pauseBlinking := false.
	textComposition lastTextCursorRect ifNotNil: [ :r | self invalidateLocalRect: r].
]

{ #category : 'blinking cursor' }
InnerTextMorph >> pauseBlinking [
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking := true.
	textComposition ifNotNil: [
		self disablesEditing ifFalse: [
			"Show cursor right now if needed"
			textComposition showTextCursor ifFalse: [
				textComposition showTextCursor: true ]]]
]

{ #category : 'accessing' }
InnerTextMorph >> positionInTextOf: aMousePosition  [
	
	^(textComposition characterBlockAtPoint: aMousePosition) stringIndex. 
	
]

{ #category : 'notifications' }
InnerTextMorph >> possiblyChanged [
	| embeddedMorphs |
	embeddedMorphs := model actualContents embeddedMorphs.
	self submorphsDo: [:each| 
		(embeddedMorphs includes: each) ifFalse: [
			self privateRemove: each.
			each privateOwner: nil ]].
	embeddedMorphs do: [ :each|
		each owner == self ifFalse: [
			self addMorphFront: each.
			each hide "Show it only when properly located"]].
	(owner is: #ScrollPane) ifTrue: [
		owner possiblyChanged ].
	self redrawNeeded.
]

{ #category : 'geometry' }
InnerTextMorph >> privateExtent: aPoint [
	| newExtent |

	newExtent := aPoint.
	(owner is: #ScrollPane) ifTrue: [
		"Resist changing the extent if no wordwrap. this should be checked."
		wrapFlag ifFalse: [ ^ false ].
		
		"Just update width. Height is set by ourselves. See #fit"
		newExtent := aPoint x truncated @ extent y ].

	^ super privateExtent: newExtent
]

{ #category : 'event handling' }
InnerTextMorph >> processKeystrokeEvent: evt [

	(acceptOn = #enter and: [evt isReturnKey]) ifTrue: [^ self acceptContents].

	self pauseBlinking.

	"Return - check for special action"
	evt isReturnKey ifTrue: [
		self crAction ifNotNil: [ :action |
			^action value ]].

	"Esc - check for special action"
	evt isEsc ifTrue: [			
		self escAction ifNotNil: [ :action |
			^action value ]].

	self handleInteraction: [ editor processKeystrokeEvent: evt ].	
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].

	"Only accept (on any) if we have unaccepted edits, otherwise the morph will flash."
	(hasUnacceptedEdits and: [ acceptOn == #any ]) ifTrue: [ self acceptContents ].
]

{ #category : 'caching' }
InnerTextMorph >> releaseCachedState [

	super releaseCachedState.
	self releaseEditorAndTextComposition.

]

{ #category : 'private' }
InnerTextMorph >> releaseEditorAndTextComposition [
	"Editor and TextComposition instantiation is lazy -- they will be created only when needed"

	editor := nil.
	textComposition := nil.
	self possiblyChanged.
]

{ #category : 'private' }
InnerTextMorph >> removedMorph: aMorph [

	editor removeMorph: aMorph.
	self fit.
	super removedMorph: aMorph
]

{ #category : 'private' }
InnerTextMorph >> resetTextComposition [
	textComposition ifNotNil: [
		textComposition
			initialize;
			extentForComposing: self extentForComposing;
			composeAll.
		editor storeSelectionInComposition ].
	self fit.
	self selectionChanged.
]

{ #category : 'selection' }
InnerTextMorph >> scrollSelectionIntoView [

	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ]
]

{ #category : 'miscellaneous' }
InnerTextMorph >> selectAll [
	"Tell my editor to select all the text"

	self editor selectAll.
	self redrawNeeded
]

{ #category : 'private' }
InnerTextMorph >> selectionChanged [
	self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ].
	(self disablesEditing not and: [ editor hasSelection not ])
		ifTrue: [ self hasKeyboardFocus ifTrue: [self startBlinking ]]
		ifFalse: [ self stopBlinking ]
]

{ #category : 'selection' }
InnerTextMorph >> setSelection: sel [

	sel == #all
		ifTrue: [ self editor selectAll ]
		ifFalse: [
			sel
				ifNil: [ self editor selectFrom:1 to: 0 ]
				ifNotNil: [ self editor selectFrom: sel first to: sel last ]].
	self scrollSelectionIntoView
]

{ #category : 'notifications' }
InnerTextMorph >> someSubmorphPositionOrExtentChanged [
	"Some submorph changed position or extent.
	Must layout submorphs again."

	textComposition ifNotNil: [ 
		textComposition composeAll.
		self fit.
		self selectionChanged ]
]

{ #category : 'blinking cursor' }
InnerTextMorph >> startBlinking [
	"And show the cursor"
	pauseBlinking := true.
	"Start blinking in a short while"
	textComposition ifNotNil: [ textComposition showTextCursor: true ].
	self startStepping: #onBlinkCursor stepTime: 500
]

{ #category : 'blinking cursor' }
InnerTextMorph >> stopBlinking [
	"And do not show cursor anymore."
	self stopStepping: #onBlinkCursor.
	textComposition ifNotNil: [
		textComposition showTextCursor: false ]
]

{ #category : 'private' }
InnerTextMorph >> stylerClass: aTextStylerClass [
	styler class == aTextStylerClass ifFalse: [
		styler := aTextStylerClass ifNotNil: [
			aTextStylerClass new ].
		styler ifNotNil: [
			model ifNotNil: [
				styler textModel: model ]]]
]

{ #category : 'shout' }
InnerTextMorph >> stylerStyled [

	self textComposition composeAll.
	needsFit := true.
	self editor recomputeSelection.	
	self updateFromTextComposition.
	self editor blinkParen.
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].
]

{ #category : 'event handling testing' }
InnerTextMorph >> tabToFocus [
	"Answer true if tab and shift+tab are used to transfer keyboard focus between windgets.
	See senders."
	^(self valueOfProperty: #tabIsValidInput) == false
]

{ #category : 'event handling testing' }
InnerTextMorph >> tabToFocus: aBoolean [
	"Enable or disable #tabToFocus.
	Disabled means that tab is valid character input."
	^self setProperty: #tabIsValidInput toValue: aBoolean not
]

{ #category : 'accessing' }
InnerTextMorph >> textColor [

	^ color
]

{ #category : 'accessing' }
InnerTextMorph >> textColor: aColor [

	color = aColor ifTrue: [^ self].
	color := aColor.
	self redrawNeeded
]

{ #category : 'private' }
InnerTextMorph >> textComposition [
	"textComposition instantiation is lazy -- create it only when needed"
	textComposition ifNil: [ self installEditorAndTextComposition ].
	^textComposition
]

{ #category : 'accessing' }
InnerTextMorph >> textProviderOrModel [

	^ (model is: #hasTextProvider) ifTrue: [ model textProvider ] ifFalse: [ model ].
]

{ #category : 'private' }
InnerTextMorph >> updateFromTextComposition [
	"A change has taken place in my textComposition, as a result of editing and I must be updated. "

	textComposition ifNotNil: [
		wrapFlag ifFalse: [
			textComposition
				initialize;
				extentForComposing: self extentForComposing;
				composeAll ].
		editor storeSelectionInComposition.
		needsFit ifTrue: [
			self fit.
			needsFit := false ]].

	(owner is: #ScrollPane) ifTrue: [
		owner
			updateScrollBarsBounds;
			setScrollDeltas ].
]

{ #category : 'accessing' }
InnerTextMorph >> wrapFlag: aBoolean [
	"Change whether contents are wrapped to the container."

	aBoolean == wrapFlag ifTrue: [^ self].
	wrapFlag := aBoolean.

	"Compose my text to fit my bounds."
	self resetTextComposition.
	self editor recomputeSelection.	
	self updateFromTextComposition 
]

{ #category : 'menu' }
InnerTextMorph >> wrapOnOff [
	self wrapFlag: wrapFlag not
]

{ #category : 'menu' }
InnerTextMorph >> wrapString [
	"Answer the string to put in a menu that will invite the user to 
	switch word wrap mode"
	^ wrapFlag asMenuItemTextPrefix,
		'text wrap to bounds'
]
