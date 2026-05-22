"
Allows viewing just part of a larger Morph. The scroll values vary from 0.0 to 1.0.

| p |
p := PluggableScrollPane new.
p scroller: BoxMorph new.
p openInWorld.

| p |
p := PluggableScrollPane new.
p scroller: Sample01Star new.
p openInWorld.
"
Class {
	#name : 'PluggableScrollPane',
	#superclass : 'PluggableMorph',
	#instVars : [
		'scrollBar',
		'scroller',
		'hScrollBar',
		'hideScrollBars',
		'drawKeyboardFocusIndicator',
		'scrollerScale',
		'scrollerRadians',
		'theScrollerExtent'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'access' }
PluggableScrollPane >> addToScroller: aMorph [

	scroller
		addMorph: aMorph position: `0@0`;
		morphExtent: aMorph morphExtent
]

{ #category : 'access' }
PluggableScrollPane >> adoptWidgetsColor: paneColor [
	color = self defaultColor ifTrue: [
		color := Theme current paneBackgroundFrom: paneColor.
	].
	super adoptWidgetsColor: paneColor.
	scrollBar adoptWidgetsColor: paneColor.
	hScrollBar adoptWidgetsColor: paneColor
]

{ #category : 'scrollbar options' }
PluggableScrollPane >> alwaysHideHorizontalScrollbar [

	hideScrollBars := #alwaysHideHorizontal.
	self hHideScrollBar.
]

{ #category : 'scrollbar options' }
PluggableScrollPane >> alwaysHideScrollbarButtons [
	scrollBar showButtons: false.
	hScrollBar showButtons: false.
]

{ #category : 'scrollbar options' }
PluggableScrollPane >> alwaysHideVerticalScrollbar [

	hideScrollBars := #alwaysHideVertical.
	self vHideScrollBar.
]

{ #category : 'scrollbar options' }
PluggableScrollPane >> alwaysShowVerticalScrollbar [

	hideScrollBars := #alwaysShowVertical.
	self vShowScrollBar.
]

{ #category : 'geometry' }
PluggableScrollPane >> borderWidth: anInteger [
	super borderWidth: anInteger.
	self updateScrollBarsBounds
]

{ #category : 'geometry testing' }
PluggableScrollPane >> clipsSubmorphs [
	"Answer true if we clip the shape of our submorphs to our own.
	Answer true only when clipping by the canvas is needed.
	Morphs that can guarantee that submorphs don't protrude and answer false to 
	#submorphsMightProtrude should answer false for better performance"

	^ true
]

{ #category : 'initialization' }
PluggableScrollPane >> defaultPadding [
	"Answer the default inner padding for the receiver.
	Redefine this method for a different initial padding in your morphs."

	^ `3@3`
]

{ #category : 'events' }
PluggableScrollPane >> doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition [
	"Some subclasses might do something"
]

{ #category : 'access' }
PluggableScrollPane >> drawKeyboardFocusIndicator: aBoolean [

	drawKeyboardFocusIndicator := aBoolean
]

{ #category : 'testing' }
PluggableScrollPane >> drawsKeyboardFocusIndicator [

	^drawKeyboardFocusIndicator
]

{ #category : 'geometry' }
PluggableScrollPane >> extentChanged: oldExtent [
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	"Now reset widget sizes"
	scroller ifNotNil: [
		scroller adjustExtent ].
	self updateScrollBarsBounds.
	self setScrollDeltas.
	self scrollSelectionIntoView 
]

{ #category : 'geometry' }
PluggableScrollPane >> focusIndicatorBottom [
	^ self hIsScrollbarShowing
		ifTrue: [ extent y - borderWidth - hScrollBar thickness ]
		ifFalse: [ extent y - borderWidth ]
]

{ #category : 'geometry' }
PluggableScrollPane >> focusIndicatorExtent [
	^ self focusIndicatorRectangle extent
]

{ #category : 'geometry' }
PluggableScrollPane >> focusIndicatorLeft [
	^ borderWidth
]

{ #category : 'geometry' }
PluggableScrollPane >> focusIndicatorRectangle [

	^ self focusIndicatorLeft @ self focusIndicatorTop corner: self focusIndicatorRight @ self focusIndicatorBottom
]

{ #category : 'geometry' }
PluggableScrollPane >> focusIndicatorRight [
	^ self vIsScrollbarShowing
		ifTrue: [ extent x - borderWidth - scrollBar thickness ]
		ifFalse: [ extent x - borderWidth ]
]

{ #category : 'geometry' }
PluggableScrollPane >> focusIndicatorTop [
	^ borderWidth
]

{ #category : 'geometry' }
PluggableScrollPane >> fontPreferenceChanged [

	super fontPreferenceChanged.
	scrollBar recreateSubmorphs.
	hScrollBar recreateSubmorphs.
	self setScrollDeltas.
]

{ #category : 'scrolling' }
PluggableScrollPane >> hHideScrollBar [
	hScrollBar hide.
	scroller ifNotNil: [
		scroller adjustExtent ].
]

{ #category : 'scrolling' }
PluggableScrollPane >> hIsScrollbarNeeded [
	"Return whether the horz scrollbar is needed"

	self mightNeedHorizontalScrollBar ifFalse: [ ^false ].
	
	"Don't show it if we were told not to."
	hideScrollBars = #hide ifTrue: [ ^false ].
	
	hideScrollBars = #alwaysHideHorizontal  ifTrue: [ ^false ].

	^self hLeftoverScrollRange > 0
]

{ #category : 'geometry testing' }
PluggableScrollPane >> hIsScrollbarShowing [
	"Return true if a horizontal scroll bar is currently showing"

	^hScrollBar visible
]

{ #category : 'geometry' }
PluggableScrollPane >> hLeftoverScrollRange [
	"Return the entire scrolling range minus the currently viewed area."

	^ (self hTotalScrollRange - self viewableWidth) max: 0
]

{ #category : 'scrolling' }
PluggableScrollPane >> hScrollBarValue: scrollValue [

	| x |
	scroller ifNotNil: [
		self hIsScrollbarShowing ifFalse: [
			^self scrollerOffset: 0@self scrollerOffset y ].
		(x := self hLeftoverScrollRange * scrollValue) <= 0
			ifTrue: [ x := 0 ].
		self scrollerOffset: x@self scrollerOffset y ].
]

{ #category : 'geometry' }
PluggableScrollPane >> hScrollBarWidth [
	"Return the width of the horizontal scrollbar"

	^ self focusIndicatorRight - self focusIndicatorLeft
]

{ #category : 'geometry' }
PluggableScrollPane >> hSetScrollDelta [
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."
	| range delta w |
	
	delta := self scrollDeltaWidth * 1.0.		"avoid Fraction arithmetic"
	range := self hLeftoverScrollRange.
	range = 0 ifTrue: [
		^hScrollBar scrollDelta: 0.02 pageDelta: 0.2; interval: 1.0; internalScrollValue: 0 ].

	"Set up for one line (for arrow scrolling), or a full pane less one line (for paging)."
	w := self viewableWidth * 1.0.		"avoid Fraction arithmetic"
	hScrollBar scrollDelta: delta / range pageDelta: w - delta / range.
	hScrollBar interval: w / self hTotalScrollRange.
	hScrollBar internalScrollValue: hScrollBar scrollValue
]

{ #category : 'scrolling' }
PluggableScrollPane >> hShowScrollBar [

	hScrollBar show.
	scroller ifNotNil: [
		scroller adjustExtent ].
]

{ #category : 'geometry' }
PluggableScrollPane >> hTotalScrollRange [
	"Return the width extent of the receiver's scrollable area"
	scroller ifNil: [ ^0 ].
	^scroller extentInOwner x
]

{ #category : 'event handling testing' }
PluggableScrollPane >> handlesMouseDown: aMouseButtonEvent [
	^ true
]

{ #category : 'event handling testing' }
PluggableScrollPane >> handlesMouseOver: evt [
	"subclasses implement #mouseEnter: and/or #mouseLeave:"
	^true
]

{ #category : 'event handling testing' }
PluggableScrollPane >> handlesMouseScroll: aMouseScrollEvent [
	"Only accept if we can actually do something useful with the event (i.e. not scrolling up when already at the top or down when already at the bottom) or if my owner chain doesn't want it"

	(aMouseScrollEvent direction = #up and: [ scrollBar scrollValue > 0 ])
		ifTrue: [ ^ true ].
	(aMouseScrollEvent direction = #down and: [ scrollBar scrollValue < 1 ])
		ifTrue: [ ^ true ].
	(aMouseScrollEvent direction = #left and: [ hScrollBar scrollValue > 0 ])
		ifTrue: [ ^ true ].
	(aMouseScrollEvent direction = #right and: [ hScrollBar scrollValue < 1 ])
		ifTrue: [ ^ true ].
	"Even if I don't want it, one of my owners might. (i.e. nested scroll panes)  If my owners don't want it, accept the event to make sure that morphs behind me doesn't get the event."
	(self privateAnyOwnerHandlesMouseScroll: aMouseScrollEvent)
		ifFalse: [ ^ true ].
	^ false
]

{ #category : 'scrolling' }
PluggableScrollPane >> hideOrShowScrollBars [

	"Assume for a moment we don't need an horizontal scrollbar"
	self hHideScrollBar.

	"Add or remove vertical scrollbar, asuming for a monent there's no horizontal scrollbar,
	to determine need of horizontal scrollbar..."
	self vIsScrollbarNeeded
		ifTrue: [ self vShowScrollBar ]
		ifFalse: [ self vHideScrollBar ].

	"If we need an horizontal scrollbar, add it."
	self hIsScrollbarNeeded ifTrue: [
		self hShowScrollBar.

		"If horizontal scrollbar is needed, maybe vertical scrollbar will be needed too (even if we previously thoutht it wouldn't be needed)."	
		"Note that there is no chance of modifying the need of horizontal scrollbar: it was already needed. Therefore, there is no circularity here."
		self vIsScrollbarNeeded  ifTrue: [
			self vShowScrollBar ]].

	"Ensure that if no scrollbars are needed, whole contents are visible"
	self vIsScrollbarShowing ifFalse: [
		scrollBar internalScrollValue: 0 ].
	self hIsScrollbarShowing ifFalse: [
		hScrollBar internalScrollValue: 0 ].

	self updateScrollBarsBounds
]

{ #category : 'scrollbar options' }
PluggableScrollPane >> hideScrollBarsIndefinitely [

	hideScrollBars := #hide.
	self vHideScrollBar.
	self hHideScrollBar.
]

{ #category : 'access' }
PluggableScrollPane >> horizontalScrollBar [
	^hScrollBar 
]

{ #category : 'initialization' }
PluggableScrollPane >> initialize [
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars := #showIfNeeded.

	"initialize the receiver's scrollBars"
	scrollBar := ScrollBar new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar := ScrollBar new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator := Preferences at: #drawKeyboardFocusIndicator.
	self addMorph: scrollBar.
	self addMorph: hScrollBar.
	self updateScrollBarsBounds.
	self innerMorphClass ifNotNil: [ :contentsClass |
		self scroller: contentsClass new ].
]

{ #category : 'initialization' }
PluggableScrollPane >> innerMorphClass [
	^nil
]

{ #category : 'testing' }
PluggableScrollPane >> is: aSymbol [
	^ aSymbol == #ScrollPane or: [ super is: aSymbol ]
]

{ #category : 'events' }
PluggableScrollPane >> keyStroke: aKeyboardEvent [

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	scroller ifNotNil: [
		scroller keyStroke: aKeyboardEvent ].
]

{ #category : 'geometry' }
PluggableScrollPane >> mightNeedHorizontalScrollBar [
	"If not sure, answer true. Only answer false when the horizontal scrollbar will not be needed, regardless of contents, for example, when showing wrapped text.
	This method is called for deciding if a vertical scroll bar is needed. Therefore if the need of an horizontal scrollbar might depend on the vertical scrollbar being there (and taking space) or not, just answer true."
	^true
]

{ #category : 'geometry' }
PluggableScrollPane >> minimumExtent [
	| minW minH h w |
	w := theScrollerExtent ifNotNil: [ theScrollerExtent x ] ifNil: [ 0 ].
	h := theScrollerExtent ifNotNil: [ theScrollerExtent y ] ifNil: [ 0 ].
	"Figure out the minimum extent for this pane so that either content, or at least required scrollbars, will fit"
	minW := padding x * 2 + w min: Theme current scrollbarThickness * 2.
	self vIsScrollbarShowing
		ifTrue: [
			minW := minW + Theme current scrollbarThickness ].
	minH := padding y * 2 + h.
	self hIsScrollbarShowing
		ifTrue: [
			minH := minH + Theme current scrollbarThickness ].
	minH := minH min: Theme current scrollbarThickness * 2.
	^ (minW + (borderWidth * 2)) @ (minH + (borderWidth * 2))
]

{ #category : 'events' }
PluggableScrollPane >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [

	| eventPositionLocalToScroller |
	scroller ifNotNil: [
		eventPositionLocalToScroller := localEventPosition - scroller morphPosition.
		scroller mouseButton1Down: aMouseButtonEvent localPosition: eventPositionLocalToScroller ].
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil
]

{ #category : 'events' }
PluggableScrollPane >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [

	| eventPositionLocalToScroller |
	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	scroller ifNotNil: [
		eventPositionLocalToScroller := localEventPosition - scroller morphPosition.
		scroller mouseButton1Up: aMouseButtonEvent localPosition: eventPositionLocalToScroller ].
]

{ #category : 'events' }
PluggableScrollPane >> mouseMove: aMouseMoveEvent localPosition: localEventPosition [

	| eventPositionLocalToScroller |
	scroller ifNotNil: [
		eventPositionLocalToScroller := localEventPosition - scroller morphPosition.
		scroller  mouseMove: aMouseMoveEvent localPosition: eventPositionLocalToScroller ].
]

{ #category : 'events' }
PluggableScrollPane >> mouseScroll: aMouseScrollEvent localPosition: localEventPosition [
	aMouseScrollEvent direction
		caseOf: {
			[ #up ] 		-> 		[ scrollBar scrollUp: 1 ].
			[ #down ] 	-> 		[ scrollBar scrollDown: 1 ].
			[ #left ] 	-> 		[ hScrollBar scrollUp: 1 ].
			[ #right ] 	-> 		[ hScrollBar scrollDown: 1 ] }
]

{ #category : 'private' }
PluggableScrollPane >> privateMoveFrontMorph: aMorph [
	"Position 1 and 2 in our submorphs are taken by our scrollbars"
	
	self privateMove: aMorph toIndex: 3
]

{ #category : 'scrolling' }
PluggableScrollPane >> scrollBy: delta [
	"Move the contents in the direction delta."

	| newYoffset yRange xRange newXoffset |
	
	"Set the offset on the scroller"
	yRange := self vLeftoverScrollRange.
	xRange := self hLeftoverScrollRange.
	
	scroller ifNotNil: [
		newYoffset := self scrollerOffset y - delta y min: yRange max: 0.
		newXoffset := self scrollerOffset x - delta x min: xRange max: 0.	
		self scrollerOffset: newXoffset@newYoffset ].

	"Update the scrollBars"
	scrollBar scrollValue: (yRange ifNotZero: [newYoffset asFloat / yRange]).
	hScrollBar scrollValue: (xRange ifNotZero: [newXoffset asFloat / xRange]).
]

{ #category : 'geometry' }
PluggableScrollPane >> scrollDeltaHeight [
	"Return the increment in pixels which this pane should be scrolled."
	^ self valueOfProperty: #scrollDeltaHeight ifAbsent: [10]
]

{ #category : 'geometry' }
PluggableScrollPane >> scrollDeltaHeight: anInteger [
	"Set the increment in pixels which this pane should be scrolled."
	self setProperty: #scrollDeltaHeight toValue: anInteger.
	self vSetScrollDelta.
]

{ #category : 'geometry' }
PluggableScrollPane >> scrollDeltaWidth [
	"Return the increment in pixels which this pane should be scrolled."
	^ self valueOfProperty: #scrollDeltaWidth ifAbsent: [10]
]

{ #category : 'geometry' }
PluggableScrollPane >> scrollDeltaWidth: anInteger [
	"Set the increment in pixels which this pane should be scrolled."
	self setProperty: #scrollDeltaWidth toValue: anInteger.
	self hSetScrollDelta.
]

{ #category : 'scrolling' }
PluggableScrollPane >> scrollSelectionIntoView [
	"Scroll my text into view if necessary and return true, else return false"

]

{ #category : 'scrolling' }
PluggableScrollPane >> scrollToShow: aRectangle [
	"scroll to include as much of aRectangle as possible, where aRectangle is in the scroller's local space.
	This means that 0@0 is scrolling all the way top and all the way left"
	| deltaX deltaY |
	deltaX := (aRectangle amountToTranslateWithin: (`0@0` extent: self viewableExtent)) x.
	deltaY := (aRectangle amountToTranslateWithin: (`0@0` extent: self viewableExtent)) y.
	(deltaX ~= 0 or: [deltaY ~= 0]) ifTrue: [
		self scrollBy: deltaX@deltaY ]
]

{ #category : 'events' }
PluggableScrollPane >> scroller [
^scroller
]

{ #category : 'initialization' }
PluggableScrollPane >> scroller: contents [

	scroller ifNotNil: [ scroller delete ].
	scroller := contents.
	self addMorphBack: scroller.
	self scrollerOffset: `0@ 0`.
]

{ #category : 'geometry' }
PluggableScrollPane >> scrollerOffset [

	^ scroller topLeftInOwner negated + self viewableAreaTopLeft
]

{ #category : 'geometry' }
PluggableScrollPane >> scrollerOffset: newOffset [

	| delta |
	delta := scroller topLeftInOwner - scroller morphPosition.
	scroller morphPosition: self viewableAreaTopLeft - newOffset - delta
]

{ #category : 'geometry' }
PluggableScrollPane >> setScrollDeltas [
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."

	self hideOrShowScrollBars.
	self vIsScrollbarShowing ifTrue: [ self vSetScrollDelta ].
	self hIsScrollbarShowing ifTrue: [ self hSetScrollDelta ].
]

{ #category : 'scrollbar options' }
PluggableScrollPane >> showScrollbarButtonsIfPossible [
	scrollBar showButtons: nil.
	hScrollBar showButtons: nil.
]

{ #category : 'geometry' }
PluggableScrollPane >> someSubmorphPositionOrExtentChanged [
	"Some submorph changed position or extent.
	Must layout submorphs again."

	self updateScrollBarsBounds
	scroller ifNotNil: [ | scrollerLocation doIt |
		doIt := false.
		scroller hasVariableExtent ifTrue: [
			theScrollerExtent = scroller extentInOwner ifFalse: [
				theScrollerExtent := scroller extentInOwner.
				doIt := true ]].
		scrollerLocation := scroller location.
		scrollerScale = scrollerLocation scale ifFalse: [
			scrollerScale := scrollerLocation scale.
			doIt := true ].
		scrollerRadians = scrollerLocation radians ifFalse: [
			scrollerRadians := scrollerLocation radians.
			doIt := true ].
		doIt ifTrue: [ self setScrollDeltas ]].
]

{ #category : 'geometry' }
PluggableScrollPane >> updateScrollBarsBounds [
	
	| t |
	hideScrollBars = #hide ifTrue: [^self].
	t := scrollBar thickness.
	(hideScrollBars = #hideVertical)
		ifFalse: [
			scrollBar
				position: extent x - t - borderWidth @ borderWidth
				extent: t @ self vScrollBarHeight ].
	t := hScrollBar thickness.
	hScrollBar
		position: borderWidth @ (extent y - t - borderWidth)
		extent: self hScrollBarWidth@t
]

{ #category : 'scrolling' }
PluggableScrollPane >> vHideScrollBar [
	scrollBar hide.
	scroller ifNotNil: [
		scroller adjustExtent ].
]

{ #category : 'scrolling' }
PluggableScrollPane >> vIsScrollbarNeeded [
	"Return whether the vertical scrollbar is needed"
	
	"Don't show it if we were told not to."
	hideScrollBars = #hide ifTrue: [ ^false ].
	
	hideScrollBars = #alwaysHideVertical  ifTrue: [ ^false ].

	hideScrollBars = #alwaysShowVertical ifTrue: [ ^true ].

	^self vLeftoverScrollRange > 0
]

{ #category : 'geometry testing' }
PluggableScrollPane >> vIsScrollbarShowing [
	"Return true if a vertical scroll bar is currently showing"

	^scrollBar visible
]

{ #category : 'geometry' }
PluggableScrollPane >> vLeftoverScrollRange [
	"Return the entire scrolling range minus the currently viewed area."

	^ (self vTotalScrollRange - self viewableHeight) max: 0
]

{ #category : 'geometry' }
PluggableScrollPane >> vScrollBarHeight [
	^extent y - (2 * borderWidth)
]

{ #category : 'scrolling' }
PluggableScrollPane >> vScrollBarValue: scrollValue [

	scroller ifNotNil: [
		self scrollerOffset: 
			(self scrollerOffset x @ 
			(self vLeftoverScrollRange * scrollValue) rounded) ].
]

{ #category : 'geometry' }
PluggableScrollPane >> vSetScrollDelta [
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."
	| range delta h |
	
	delta := self scrollDeltaHeight * 1.0.	"avoid Fraction arithmetic"
	range := self vLeftoverScrollRange.
	range = 0 ifTrue: [
		^ scrollBar scrollDelta: 0.02 pageDelta: 0.2; interval: 1.0; internalScrollValue: 0 ].

	"Set up for one line (for arrow scrolling), or a full pane less one line (for paging)."
	h := self viewableHeight * 1.0. 		"avoid Fraction arithmetic"
	scrollBar scrollDelta: delta / range pageDelta: h - delta / range.
	scrollBar interval: h / self vTotalScrollRange.
	scrollBar internalScrollValue: scrollBar scrollValue
]

{ #category : 'scrolling' }
PluggableScrollPane >> vShowScrollBar [

	scrollBar show.
	scroller ifNotNil: [
		scroller adjustExtent ].
]

{ #category : 'geometry' }
PluggableScrollPane >> vTotalScrollRange [
	"Return the height extent of the receiver's scrollable area"
	scroller ifNil: [ ^0 ].
	^scroller extentInOwner y
]

{ #category : 'access' }
PluggableScrollPane >> verticalScrollBar [
	^scrollBar
]

{ #category : 'geometry' }
PluggableScrollPane >> viewableArea [
	^ self viewableAreaTopLeft corner: self viewableAreaRight @ self viewableAreaBottom
]

{ #category : 'geometry' }
PluggableScrollPane >> viewableAreaBottom [
	^ self focusIndicatorBottom - padding y
]

{ #category : 'geometry' }
PluggableScrollPane >> viewableAreaLeft [
	^ self focusIndicatorLeft + padding x
]

{ #category : 'geometry' }
PluggableScrollPane >> viewableAreaRight [
	^ self focusIndicatorRight - padding x
]

{ #category : 'geometry' }
PluggableScrollPane >> viewableAreaTop [
	^ self focusIndicatorTop + padding y
]

{ #category : 'geometry' }
PluggableScrollPane >> viewableAreaTopLeft [
	^ self viewableAreaLeft @ self viewableAreaTop
]

{ #category : 'geometry' }
PluggableScrollPane >> viewableExtent [

	^ self viewableWidth @ self viewableHeight
]

{ #category : 'geometry' }
PluggableScrollPane >> viewableHeight [
	"Viewable height.
	Leave room for horizontal scrollbar if present"

	^ self viewableAreaBottom - self viewableAreaTop 
]

{ #category : 'geometry' }
PluggableScrollPane >> viewableWidth [
	"Viewable width.
	Leave room for vertical scrollbar if present"

	^ self viewableAreaRight - self viewableAreaLeft
]
