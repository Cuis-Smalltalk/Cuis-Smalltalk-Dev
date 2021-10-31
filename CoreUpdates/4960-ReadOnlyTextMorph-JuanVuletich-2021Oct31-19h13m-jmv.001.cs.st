'From Cuis 5.0 [latest update: #4958] on 31 October 2021 at 7:16:14 pm'!
!classDefinition: #ReadOnlyTextMorph category: 'Morphic-Widgets'!
WidgetMorph subclass: #ReadOnlyTextMorph
	instanceVariableNames: 'contents wrapFlag textComposition editor mutex needsFit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!ReadOnlyTextMorph commentStamp: 'jmv 10/31/2021 18:56:35' prior: 0!
ReadOnlyTextMorph support display of text with emphasis. Very much like InnerTextMorph, but doesn't allow editing. Doesn't have a model. Contents can be set directly. It can be used on its own, no need to include them in some kind of TextModelMorph / ScrollPane.

Clipping to extent is done, and word wrap is optional. Support all features of Text, including fonts, sizes, emphasis and embedded morphs.

Text display is clipped to the innerBounds of the rectangle, and text composition is normally performed within a rectangle which is innerBounds inset by the margins parameter.


(ReadOnlyTextMorph contents: 'Hello
World!!') openInWorld.


(ReadOnlyTextMorph contents: Utilities defaultTextEditorContents) openInWorld.


t := ReadOnlyTextMorph contents: Utilities defaultTextEditorContents.
s := PluggableScrollPane new.
s scroller: t.
s openInWorld.!


!Text methodsFor: 'TextModel compatibility' stamp: 'jmv 10/31/2021 18:33:45'!
actualContents
	^self! !

!Text methodsFor: 'TextModel compatibility' stamp: 'jmv 10/31/2021 18:33:52'!
textSize
	^self size! !


!ReadOnlyTextMorph methodsFor: 'accessing' stamp: 'jmv 10/31/2021 18:45:21'!
contents: aTextOrString
	contents _ aTextOrString asText.
	self releaseEditorAndTextComposition.	"So the model is properly set on the editor and the text composition"! !

!ReadOnlyTextMorph methodsFor: 'accessing' stamp: 'jmv 10/31/2021 18:45:36'!
contents: aTextOrString wrappedTo: width
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	| newExtent oldExtent |
	wrapFlag _ true.
	contents _ aTextOrString asText.
	newExtent _ width truncated@extent y.
	extent = newExtent ifFalse: [
		oldExtent _ extent.
		extent _ newExtent.
		self extentChanged: oldExtent.
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].
	self contents: aTextOrString! !

!ReadOnlyTextMorph methodsFor: 'accessing' stamp: 'jmv 10/31/2021 18:45:10'!
contentsAsIs: aTextOrString
	"Accept new text contents with line breaks only as in the text.
	Fit my width and height to the result."
	wrapFlag _ false.
	contents _ aTextOrString asText.! !

!ReadOnlyTextMorph methodsFor: 'accessing' stamp: 'jmv 10/31/2021 17:33:03'!
editor
	"Return my current editor, or install a new one."
	editor ifNil: [ self installEditorAndTextComposition ].
	^editor! !

!ReadOnlyTextMorph methodsFor: 'accessing' stamp: 'jmv 10/31/2021 17:33:03'!
isWrapped
	
	^wrapFlag! !

!ReadOnlyTextMorph methodsFor: 'accessing' stamp: 'jmv 10/31/2021 17:33:03'!
textColor

	^ color! !

!ReadOnlyTextMorph methodsFor: 'accessing' stamp: 'jmv 10/31/2021 17:33:03'!
textColor: aColor

	color = aColor ifTrue: [^ self].
	color _ aColor.
	self redrawNeeded! !

!ReadOnlyTextMorph methodsFor: 'accessing' stamp: 'jmv 10/31/2021 17:33:03'!
wrapFlag: aBoolean
	"Change whether contents are wrapped to the container."

	aBoolean == wrapFlag ifTrue: [^ self].
	wrapFlag _ aBoolean.

	"Compose my text to fit my bounds."
	self resetTextComposition.
	self editor recomputeSelection.	
	self updateFromTextComposition ! !

!ReadOnlyTextMorph methodsFor: 'caching' stamp: 'jmv 10/31/2021 17:33:03'!
releaseCachedState

	super releaseCachedState.
	self releaseEditorAndTextComposition.
! !

!ReadOnlyTextMorph methodsFor: 'drawing' stamp: 'jmv 10/31/2021 17:33:03'!
debugDrawLineRectsOn: aCanvas
	"Shows where text line rectangles are"

	self textComposition lines do: [ :line |
		aCanvas
			frameRectangle: line rectangle
			borderWidth: 1
			color: `Color brown` ]
! !

!ReadOnlyTextMorph methodsFor: 'drawing' stamp: 'jmv 10/31/2021 18:11:09'!
drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		textComposition: self textComposition
		bounds: self morphLocalBounds
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus).! !

!ReadOnlyTextMorph methodsFor: 'editing' stamp: 'jmv 10/31/2021 18:00:39'!
acceptOnCR
	"Answer whether the receiver wants to accept when the Return key is hit"

	^ false! !

!ReadOnlyTextMorph methodsFor: 'editing' stamp: 'jmv 10/31/2021 17:33:03'!
enterClickableRegion: aMorphicEvent localPosition: localEventPosition
! !

!ReadOnlyTextMorph methodsFor: 'editing' stamp: 'jmv 10/31/2021 17:33:03'!
handleInteraction: interactionBlock
	"Perform the changes in interactionBlock, noting any change in selection
	and possibly a change in the composition"

	self selectionChanged.  "Note old selection"

	interactionBlock value.

	self selectionChanged.  "Note new selection"
	self updateFromTextComposition! !

!ReadOnlyTextMorph methodsFor: 'events' stamp: 'jmv 10/31/2021 18:11:31'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self handleInteraction: [
		editor clickAndHalf ].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

!ReadOnlyTextMorph methodsFor: 'events' stamp: 'jmv 10/31/2021 18:11:22'!
doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self handleInteraction: [
		editor doubleClickAndHalf ].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

!ReadOnlyTextMorph methodsFor: 'events' stamp: 'jmv 10/31/2021 17:38:05'!
keyStroke: aKeyboardEvent

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	self processKeystrokeEvent: aKeyboardEvent.
			
	super keyStroke: aKeyboardEvent! !

!ReadOnlyTextMorph methodsFor: 'events' stamp: 'jmv 10/31/2021 17:33:03'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	"If we don't focus, Get focus, and do nothing else (the user will need to click again to do further interaction)"
	self hasKeyboardFocus ifFalse: [
		^aMouseButtonEvent hand newKeyboardFocus: self].

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	self handleInteraction: [ editor mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

!ReadOnlyTextMorph methodsFor: 'events' stamp: 'jmv 10/31/2021 18:09:10'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self handleInteraction: [ editor mouseButton1Up: aMouseButtonEvent  localPosition: localEventPosition ].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ]! !

!ReadOnlyTextMorph methodsFor: 'events' stamp: 'jmv 10/31/2021 18:17:17'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent mouseButton1Pressed ifFalse: [
		^ self enterClickableRegion: aMouseMoveEvent localPosition: localEventPosition ].
	self handleInteraction: [
		editor mouseMove: aMouseMoveEvent localPosition: localEventPosition].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

!ReadOnlyTextMorph methodsFor: 'event handling testing' stamp: 'jmv 10/31/2021 18:58:59'!
disablesEditing
	^ true! !

!ReadOnlyTextMorph methodsFor: 'event handling testing' stamp: 'jmv 10/31/2021 17:33:03'!
handlesKeyboard

	^self visible! !

!ReadOnlyTextMorph methodsFor: 'event handling testing' stamp: 'jmv 10/31/2021 17:33:03'!
handlesMouseDown: aMouseButtonEvent
	^ true! !

!ReadOnlyTextMorph methodsFor: 'event handling' stamp: 'jmv 10/31/2021 18:09:56'!
keyboardFocusChange: aBoolean
	
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw border feedback"
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor storeSelectionInComposition ].	"Forces install"
			].
	
	"Selection might be shown differently when focused"
	self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ].! !

!ReadOnlyTextMorph methodsFor: 'event handling' stamp: 'jmv 10/31/2021 18:18:48'!
processKeystrokeEvent: evt

	self handleInteraction: [ editor processKeystrokeEvent: evt ].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

!ReadOnlyTextMorph methodsFor: 'events-processing' stamp: 'jmv 10/31/2021 17:33:03'!
processKeystroke: aKeyboardEvent
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue:[^self].
	self handlesKeyboard ifFalse: [^ self].
	aKeyboardEvent wasHandled: true.
	self keyStroke: aKeyboardEvent! !

!ReadOnlyTextMorph methodsFor: 'events-processing' stamp: 'jmv 10/31/2021 17:33:03'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Re-implemented to allow for mouse-up move events"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1]! !

!ReadOnlyTextMorph methodsFor: 'geometry' stamp: 'jmv 10/31/2021 18:55:58'!
adjustExtent
"So far, copied verbatim from InnerTextMorph."
	"This is just a suggestion. If we do wordwrap, the width will be honored.
	But the height is whatever is appropriate for the contents!! See #fit"
	self morphExtent: owner viewableExtent! !

!ReadOnlyTextMorph methodsFor: 'geometry' stamp: 'jmv 10/31/2021 18:38:13'!
extentChanged: oldExtent
	"Our extent changed. Reflow text."
	super extentChanged: oldExtent.
	contents ifNotNil: [
		wrapFlag ifTrue: [
			extent x = oldExtent x ifFalse: [
				self resetTextComposition.
				self editor recomputeSelection.	
				(owner is: #ScrollPane) ifTrue: [
					owner
						updateScrollBarsBounds;
						setScrollDeltas ]]]].! !

!ReadOnlyTextMorph methodsFor: 'geometry' stamp: 'jmv 10/31/2021 17:33:03'!
minimumExtent

	^(9@(FontFamily defaultLineSpacing+2))! !

!ReadOnlyTextMorph methodsFor: 'geometry' stamp: 'jmv 10/31/2021 18:55:43'!
privateExtent: aPoint
	| newExtent |
"So far, copied verbatim from InnerTextMorph."
	"Resist changing the extent if no wordwrap. this should be checked."
	wrapFlag ifFalse: [ ^ false ].
	
	"Just update width. Height is set by ourselves. See #fit"
	newExtent _ aPoint x truncated @ extent y.

	^ super privateExtent: newExtent! !

!ReadOnlyTextMorph methodsFor: 'initialization' stamp: 'jmv 10/31/2021 17:33:03'!
defaultColor
	"Return the default fill style for the receiver"
	^ Theme current text! !

!ReadOnlyTextMorph methodsFor: 'initialization' stamp: 'jmv 10/31/2021 18:22:11'!
initialize
	super initialize.
	wrapFlag _ true.
	needsFit _ false.! !

!ReadOnlyTextMorph methodsFor: 'menu' stamp: 'jmv 10/31/2021 17:33:03'!
addCustomMenuItems: aCustomMenu hand: aHandMorph 
	"Add text-related menu items to the menu"

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu 
		addUpdating: #wrapString
		target: self
		action: #wrapOnOff! !

!ReadOnlyTextMorph methodsFor: 'menu' stamp: 'jmv 10/31/2021 17:33:03'!
wrapOnOff
	self wrapFlag: wrapFlag not! !

!ReadOnlyTextMorph methodsFor: 'menu' stamp: 'jmv 10/31/2021 17:33:03'!
wrapString
	"Answer the string to put in a menu that will invite the user to 
	switch word wrap mode"
	^ wrapFlag asMenuItemTextPrefix,
		'text wrap to bounds'! !

!ReadOnlyTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 17:33:03'!
extentForComposing
	self flag: #jmvVer2.	"like #extent ..."
	^wrapFlag
		ifTrue: [ extent x @ 9999999 ]
		ifFalse: [ 9999999@9999999 ]! !

!ReadOnlyTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 18:10:34'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth oldExtent |
	newWidth _ extent x.
	"Adjust width only if we don't wrap text to own width!!"
	wrapFlag ifFalse: [
		newWidth _ self textComposition usedWidth max: 9 ].
	newHeight _ self textComposition usedHeight max: FontFamily defaultLineSpacing + 2.
	newExtent _ newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				oldExtent _ extent.
				extent _ newExtent.
				self extentChanged: oldExtent.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].! !

!ReadOnlyTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 18:43:02'!
installEditorAndTextComposition
	"Install an editor for my textComposition. Install also the textComposition."
	| e tc |
	
	"Editor and TextComposition are assigned here atomically."
	e _ TextEditor new morph: self.
	e model: contents.
	tc _ TextComposition new.
	tc showTextCursor: false.
	"Keep critical section short"
	self mutex critical: [
		editor _ e.
		textComposition _ tc.
		tc
			setModel: contents;
			extentForComposing: self extentForComposing.
		e textComposition: tc.
		tc editor: e ].
	e setEmphasisHereFromText.
	tc composeAll.
	e resetState.
	self fit.
	self selectionChanged.! !

!ReadOnlyTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 17:33:03'!
mutex
	mutex
		ifNil: [ mutex := Mutex new ].
	^mutex! !

!ReadOnlyTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 17:33:03'!
releaseEditorAndTextComposition
	"Editor and TextComposition instantiation is lazy -- they will be created only when needed"

	editor _ nil.
	textComposition _ nil! !

!ReadOnlyTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 17:33:03'!
resetTextComposition
	textComposition ifNotNil: [
		textComposition
			initialize;
			extentForComposing: self extentForComposing;
			composeAll.
		editor storeSelectionInComposition ].
	self fit.
	self selectionChanged.! !

!ReadOnlyTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 17:58:57'!
selectionChanged

	self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ].! !

!ReadOnlyTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 17:33:03'!
textComposition
	"textComposition instantiation is lazy -- create it only when needed"
	textComposition ifNil: [ self installEditorAndTextComposition ].
	^textComposition! !

!ReadOnlyTextMorph methodsFor: 'private' stamp: 'jmv 10/31/2021 18:07:27'!
updateFromTextComposition
	"A change has taken place in my textComposition, as a result of editing and I must be updated. "

	textComposition ifNotNil: [
		editor storeSelectionInComposition.
		needsFit ifTrue: [
			self fit.
			needsFit _ false ]].

	(owner is: #ScrollPane) ifTrue: [
		owner
			updateScrollBarsBounds;
			setScrollDeltas ].! !

!ReadOnlyTextMorph methodsFor: 'notifications' stamp: 'jmv 10/31/2021 18:36:09'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self updateFromTextComposition.! !

!ReadOnlyTextMorph methodsFor: 'notifications' stamp: 'jmv 10/31/2021 18:49:53'!
possiblyChanged! !

!ReadOnlyTextMorph methodsFor: 'notifications' stamp: 'jmv 10/31/2021 17:33:03'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	super someSubmorphPositionOrExtentChanged.
	textComposition ifNotNil: [ 
		textComposition composeAll.
		self fit.
		self selectionChanged ]! !

!ReadOnlyTextMorph methodsFor: 'testing' stamp: 'jmv 10/31/2021 18:04:06'!
hasUnacceptedEdits
	"No editing supported."
	^false! !

!ReadOnlyTextMorph methodsFor: 'miscellaneous' stamp: 'jmv 10/31/2021 17:33:03'!
selectAll
	"Tell my editor to select all the text"

	self editor selectAll.
	self redrawNeeded! !


!ReadOnlyTextMorph class methodsFor: 'instance creation' stamp: 'jmv 10/31/2021 19:16:08'!
contents: aTextOrString
	"See a few more examples in class comment"
"
(ReadOnlyTextMorph contents: 'Hello
World!!') openInWorld
"

	^ self new contents: aTextOrString.! !

