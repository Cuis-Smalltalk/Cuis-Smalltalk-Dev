'From Cuis 4.0 of 3 April 2012 [latest update: #1255] on 12 April 2012 at 10:40:01 pm'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2012 22:37'!
iconColor

	^ self isPressed
		ifTrue: [ Color gray: 0.75 ]
		ifFalse: [
			self mouseIsOver
				ifTrue: [ Color gray: 0.75 ]
				ifFalse: [ Color white ]].! !


!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 4/12/2012 22:04'!
scrollBarClass
	^ScrollBar! !


!ScrollBar methodsFor: 'initialize' stamp: 'jmv 4/12/2012 22:11'!
buttonClass
	^PluggableButtonMorph! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 4/12/2012 22:12'!
sliderClass
	^DraggeableButtonMorph! !


!ScrollBar class methodsFor: 'constants' stamp: 'jmv 4/12/2012 22:33'!
scrollbarThickness

	^Preferences scrollbarThickness! !


!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 4/12/2012 22:30'!
resetMenu
	| w f |
	firstVisible _ 1.
	self selected: 1.
	w _ 120.
	f _ self class listFont.
	1
		to: completer entryCount
		do: [ :index |
			w _ w max: (f widthOfString: (completer entries at: index) asString)].
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ w + ScrollBar scrollbarThickness ].
	self extent: w + 4 @ (self visibleItemsCount * self itemHeight+2)! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2012 22:30'!
drawOn: aCanvas
	| rectangle w x0 y0 h y1 y2 scrollbarThickness |
	aCanvas frameAndFillRectangle: self bounds fillColor: self color borderWidth: borderWidth borderColor: borderColor.
	x0 _ bounds left+1.
	y0 _ bounds top+1.
	w _ bounds width-2.
	scrollbarThickness _ ScrollBar scrollbarThickness.
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ bounds width-scrollbarThickness -2.
		aCanvas
			frameRectangle: (bounds topRight - (scrollbarThickness@0)
				extent: scrollbarThickness @ bounds height)
			borderWidth: 1
			color: borderColor.
		aCanvas
			image: (FormCanvas arrowOfDirection: #up size: scrollbarThickness)
			at: bounds topRight - (scrollbarThickness@0).
		aCanvas
			image: (FormCanvas arrowOfDirection: #down size: scrollbarThickness)
			at: bounds bottomRight - scrollbarThickness.
		h _ bounds height - (2 * scrollbarThickness).
		y1 _ (1.0 * self firstVisible-1 / completer entryCount * h) ceiling + y0 + scrollbarThickness-1.
		y2 _ (1.0 * self lastVisible / completer entryCount * h) floor + y0 + scrollbarThickness -1.
		aCanvas
			fillRectangle: (bounds right - scrollbarThickness+2@y1 corner: bounds right-2 @ y2)
			colorOrInfiniteForm: Color veryLightGray ].
	self firstVisible
		to: self lastVisible
		do: [ :index |
			rectangle _ x0@y0 extent: w@self class itemHeight.
			index = self selected
				ifTrue: [
					aCanvas fillRectangle: rectangle colorOrInfiniteForm: (Theme current listHighlightFocused: true) ].
			aCanvas
				drawString: (completer entries at: index) asString
				in: rectangle
				font: self class listFont
				color: Theme current text.
			y0 _ y0 + self itemHeight ]! !


!LayoutMorph methodsFor: 'private' stamp: 'jmv 4/12/2012 22:34'!
minPaneWidthForReframe

	^ScrollBar scrollbarThickness * 3! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2012 22:36'!
drawInconOn: aCanvas

	| theIcon |
	theIcon _ self magnifiedIcon.
	aCanvas
		image: theIcon
		multipliedBy: self iconColor
		at: bounds center - (theIcon extent //2)! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2012 22:15'!
drawOn: aCanvas

	self isRoundButton
		ifTrue: [ icon ifNil: [ self drawRoundGradientLookOn: aCanvas ]]
		ifFalse: [
			 self draw3DLookOn: aCanvas ].

	icon ifNotNil: [
		self drawInconOn: aCanvas ].! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 4/12/2012 22:31'!
updateDownButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowOfDirection: #down size: ScrollBar scrollbarThickness.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		actWhen: #buttonStillDown;		"to enable multiple action if held down"
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 4/12/2012 22:31'!
updateLeftButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowOfDirection: #left size: ScrollBar scrollbarThickness.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		actWhen: #buttonStillDown;		"to enable multiple action if held down"
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 4/12/2012 22:31'!
updateRightButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowOfDirection: #right size: ScrollBar scrollbarThickness.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		actWhen: #buttonStillDown;		"to enable multiple action if held down"
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 4/12/2012 22:31'!
updateUpButtonImage
	"update the receiver's as a upButton. put a new image inside"

	icon _ FormCanvas arrowOfDirection: #up size: ScrollBar scrollbarThickness.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		actWhen: #buttonStillDown;		"to enable multiple action if held down"
		redrawNeeded! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 4/12/2012 22:28'!
extent: newExtent
	
	| minH minW |
	"Figure out the minimum width and height for this pane so that scrollbars will appear"
	minH _ self vIsScrollbarShowing
		ifTrue: [self scrollBarClass scrollbarThickness * 2]
		ifFalse: [0].
	minW _ self hIsScrollbarShowing
		ifTrue: [self scrollBarClass scrollbarThickness * 2]
		ifFalse: [0].
	super extent: (newExtent max: (minW@minH)).

	"Now reset widget sizes"
	scroller adjustExtent.
	self updateScrollBarsBounds.
	self setScrollDeltas! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 4/12/2012 22:28'!
focusIndicatorRectangle

	| b topLeft bottomRight |
	b _ self innerBounds.
	topLeft _ b topLeft.
	bottomRight _ b bottomRight.
	self vIsScrollbarShowing ifTrue: [
		bottomRight _ scrollBar bounds left -1@ bottomRight y].
	self hIsScrollbarShowing ifTrue: [
		bottomRight _ bottomRight x @ 
			(bottomRight y - self scrollBarClass scrollbarThickness)].
	^topLeft corner: bottomRight! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 4/12/2012 22:28'!
hScrollBarWidth
	"Return the width of the horizontal scrollbar"

	| w |	
	w _ bounds width - (2 * borderWidth).
	self vIsScrollbarShowing
		ifTrue: [ w _ w - self scrollBarClass scrollbarThickness ].
	^w! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 4/12/2012 22:28'!
updateScrollBarsBounds
	
	| t topLeft |
	hideScrollBars ifTrue: [^self].
	t _ self scrollBarClass scrollbarThickness.

	topLeft _ bounds topRight + (0-t-borderWidth @ borderWidth).
	scrollBar bounds: (topLeft extent: t @ self vScrollBarHeight).

	topLeft _ bounds bottomLeft + (borderWidth @ (t + borderWidth) negated).
	hScrollBar bounds: (topLeft extent: self hScrollBarWidth@ t)! !

!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 4/12/2012 22:04'!
initialize
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars _ false.

	"initialize the receiver's scrollBars"
	scrollBar _ self scrollBarClass new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar _ self scrollBarClass new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator _ true.

	scroller _ self innerMorphClass new.
	self addMorph: scroller.
	self scrollerOffset: 0@ 0.
	self addMorph: scrollBar.
	self addMorph: hScrollBar.! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 4/12/2012 22:34'!
buttonExtent

	^self class scrollbarThickness! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 4/12/2012 22:11'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	self addMorph: downButton.
	downButton bounds: (self innerBounds bottomRight - e extent: e).
	bounds isWide
		ifTrue: [ downButton updateRightButtonImage ]
		ifFalse: [ downButton updateDownButtonImage ]! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 4/12/2012 22:12'!
initializeSlider
	"initialize the receiver's slider"

	sliderShadow _ RectangleMorph new.
	sliderShadow borderWidth: 0.
	self addMorph: sliderShadow.
	sliderShadow bounds: self totalSliderArea.
	sliderShadow hide.
		
	slider _ self sliderClass new.
	slider model: self.
	slider grabSelector: #sliderGrabbed.
	slider dragSelector: #scrollAbsolute:.
	slider action: #sliderReleased.
	self addMorph: slider.
	slider bounds: self totalSliderArea.

	self computeSlider! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 4/12/2012 22:11'!
initializeUpButton
	"initialize the receiver's upButton"

	upButton _ self buttonClass new.
	upButton model: self.
	self addMorph: upButton.
	upButton
		bounds: (self innerBounds topLeft extent: self buttonExtent).
	bounds isWide
		ifTrue: [ upButton updateLeftButtonImage ]
		ifFalse: [ upButton updateUpButtonImage ].! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 4/12/2012 22:21'!
sliderGrabbed

	sliderShadow
		bounds: slider bounds;
		show! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 4/12/2012 22:34'!
defaultBounds
	"answer the default bounds for the receiver"

	^ 0 @ 0 extent: self class scrollbarThickness @ 100! !

!ScrollBar methodsFor: 'drawing' stamp: 'jmv 4/12/2012 22:20'!
drawOn: aCanvas

	aCanvas
		fillRectangle: bounds
		colorOrInfiniteForm: (color alphaMixed: 0.3 with: Color white)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 4/12/2012 22:32'!
minPaneWidthForReframe
	^ScrollBar scrollbarThickness * 3! !

!methodRemoval: Theme #buttonPressedIconColor!
Theme removeSelector: #buttonPressedIconColor!
!methodRemoval: Theme #scrollbarThickness!
Theme removeSelector: #scrollbarThickness!

!ScrollBar class reorganize!
('constants' scrollbarThickness)
!

!methodRemoval: DraggeableButtonMorph #drawSTELookOn:!
DraggeableButtonMorph removeSelector: #drawSTELookOn:!
!methodRemoval: PluggableButtonMorph #drawSTELookOn:!
PluggableButtonMorph removeSelector: #drawSTELookOn:!
