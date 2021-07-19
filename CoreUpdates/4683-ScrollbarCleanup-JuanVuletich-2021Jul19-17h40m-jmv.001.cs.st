'From Cuis 5.0 [latest update: #4681] on 19 July 2021 at 5:44:30 pm'!

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 7/19/2021 16:09:05'!
focusIndicatorBottom
	^ self hIsScrollbarShowing
		ifTrue: [ extent y - borderWidth - ScrollBar scrollbarThickness ]
		ifFalse: [ extent y - borderWidth ]! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 7/19/2021 16:09:11'!
focusIndicatorRight
	^ self vIsScrollbarShowing
		ifTrue: [ extent x - borderWidth - ScrollBar scrollbarThickness ]
		ifFalse: [ extent x - borderWidth ]! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 7/19/2021 16:09:41'!
minimumExtent
	| minW minH |
	"Figure out the minimum extent for this pane so that either content, or at least required scrollbars, will fit"
	minW _ self xtraBorder * 2 + scroller morphWidth min: ScrollBar scrollbarThickness * 2.
	self vIsScrollbarShowing
		ifTrue: [
			minW _ minW + ScrollBar scrollbarThickness].
	minH _ self xtraBorder * 2 + scroller morphHeight.
	self hIsScrollbarShowing
		ifTrue: [
			minH _ minH + ScrollBar scrollbarThickness].
	minH _ minH min: ScrollBar scrollbarThickness * 2.
	^ (minW + (borderWidth * 2)) @ (minH + (borderWidth * 2))! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 7/19/2021 16:09:54'!
updateScrollBarsBounds
	
	| t |
	hideScrollBars = #hide ifTrue: [^self].
	t _ ScrollBar scrollbarThickness.
	(hideScrollBars = #hideVertical)
	ifFalse: [
		scrollBar
			morphPosition: extent x - t - borderWidth @ borderWidth
			extent: t @ self vScrollBarHeight.
	].
	hScrollBar
		morphPosition: borderWidth @ (extent y - t - borderWidth)
		extent: self hScrollBarWidth@t! !

!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 7/19/2021 16:09:22'!
initialize
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars _ #showIfNeeded.

	"initialize the receiver's scrollBars"
	scrollBar _ ScrollBar new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar _ ScrollBar new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator _ true.

	scroller _ self innerMorphClass new.
	self addMorph: scroller.
	self scrollerOffset: `0@ 0`.
	self addMorph: scrollBar.
	self addMorph: hScrollBar! !


!ScrollBar methodsFor: 'initialization' stamp: 'jmv 7/19/2021 16:06:58'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ PluggableButtonMorph new.
	downButton model: self.
	downButton morphExtent: e@e.
	Theme current scrollbarShowButtons ifFalse: [^ self].
	self addMorph: downButton position: extent - borderWidth - e.
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ self updateRightButton: downButton ]
		ifFalse: [ self updateDownButton: downButton ]! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 7/19/2021 16:08:11'!
initializeSlider
	"initialize the receiver's slider"

	sliderShadow _ WidgetMorph new noBorder.
	self addMorph: sliderShadow.
	sliderShadow hide.
		
	slider _ DraggeableButtonMorph new.
	slider model: self.
	slider grabSelector: #sliderGrabbedAt:.
	slider dragSelector: #scrollTo:.
	slider action: #sliderReleased.
	self addMorph: slider.

	self computeSlider! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 7/19/2021 16:07:05'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ PluggableButtonMorph new.
	upButton model: self.
	upButton morphExtent: e@e.
	Theme current scrollbarShowButtons ifFalse: [^ self].
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ self updateLeftButton: upButton ]
		ifFalse: [ self updateUpButton: upButton ].
! !

!methodRemoval: ScrollBar #sliderClass stamp: 'jmv 7/19/2021 17:40:43'!
ScrollBar removeSelector: #sliderClass!
!methodRemoval: ScrollBar #buttonClass stamp: 'jmv 7/19/2021 17:41:04'!
ScrollBar removeSelector: #buttonClass!
!methodRemoval: PluggableScrollPane #scrollBarClass stamp: 'jmv 7/19/2021 17:41:10'!
PluggableScrollPane removeSelector: #scrollBarClass!
