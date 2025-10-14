'From Cuis7.5 [latest update: #7658] on 14 October 2025 at 1:28:01 pm'!

!PluggableScrollPane methodsFor: 'scrollbar options' stamp: 'jmv 10/14/2025 12:21:14'!
alwaysHideScrollbarButtons
	scrollBar showButtons: false.
	hScrollBar showButtons: false.! !

!PluggableScrollPane methodsFor: 'scrollbar options' stamp: 'jmv 10/14/2025 13:10:11'!
showScrollbarButtonsIfPossible
	scrollBar showButtons: nil.
	hScrollBar showButtons: nil.! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 10/14/2025 12:53:59'!
buttonExtentToUse
	"Answer zero if buttons are not visible.
	Answer zero if no button is to be drawn due to non-standard thickness."

	| desired possible |
	upButton visible ifFalse: [ ^0 ].
	desired := self thickness. "To suit us"
	possible := Theme current scrollbarThickness. "To be drawn by any canvas"
	^desired = possible ifTrue: [desired] ifFalse: [0]! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 10/14/2025 13:27:18'!
hideOrShowButtons
	| b |
	b := self showsButtons and: [
		self thickness = Theme current scrollbarThickness and: [
			extent rho > (self thickness * 4)]].
	upButton visible: b.
	downButton visible: b.
	self expandSlider.
	self computeSlider.! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 10/14/2025 11:59:52'!
thickness
	"width for vertical scrollbars, height for horizontal scrollbars"
	^extent x min: extent y! !

!ScrollBar methodsFor: 'options' stamp: 'jmv 10/14/2025 12:46:41'!
showButtons: aBoolean
	"Default is `Theme current scrollbarShowButtons`. Can be overriden for true or false.
	Buttons will only be shown if there's enough room for a reasonable slider size + buttons."

	self setProperty: #showButtons toValue: aBoolean.
	self hideOrShowButtons.! !

!ScrollBar methodsFor: 'options' stamp: 'jmv 10/14/2025 12:09:33'!
showsButtons
	"Default is `Theme current scrollbarShowButtons`. Can be overriden for true or false.
	Buttons will only be shown if there's enough room for a reasonable slider size + buttons."

	^ self valueOfProperty: #showButtons ifAbsent: [ Theme current scrollbarShowButtons ]! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/14/2025 11:59:56'!
focusIndicatorBottom
	^ self hIsScrollbarShowing
		ifTrue: [ extent y - borderWidth - hScrollBar thickness ]
		ifFalse: [ extent y - borderWidth ]! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/14/2025 12:00:00'!
focusIndicatorRight
	^ self vIsScrollbarShowing
		ifTrue: [ extent x - borderWidth - scrollBar thickness ]
		ifFalse: [ extent x - borderWidth ]! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/14/2025 11:58:13'!
minimumExtent
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
	^ (minW + (borderWidth * 2)) @ (minH + (borderWidth * 2))! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/14/2025 12:57:15'!
updateScrollBarsBounds
	
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
		extent: self hScrollBarWidth@t! !


!PluggableListMorph methodsFor: 'layout' stamp: 'jmv 10/14/2025 12:00:07'!
maximumLayoutHeight
	| answer |
	limitLayoutHeight ifTrue: [
		answer := self desiredHeight.
		self hIsScrollbarShowing ifTrue: [
			answer := answer + hScrollBar thickness].
		^answer ].
	^nil! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 10/14/2025 11:38:25'!
computeSlider

	| delta |
	delta := self buttonExtentToUse + (self freeSliderRoom * value) asInteger.
	self isHorizontal
		ifTrue: [
			slider morphPosition: borderWidth + delta @ (borderWidth + self sliderSlack) ]
		ifFalse: [
			slider morphPosition: (borderWidth + self sliderSlack) @ (borderWidth + delta) ] .! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 10/14/2025 12:00:22'!
expandSlider
	"Compute the new size of the slider."

	| e |
	e := (self totalSliderRoom * interval) asInteger max: 7.
	slider morphExtent: (self isHorizontal
		ifTrue: [ e @ (self thickness-(self sliderSlack*2)) ]
		ifFalse: [ self thickness-(self sliderSlack*2) @ e ])! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 10/14/2025 12:19:57'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."
	| isH wasH p |

	super extentChanged: oldExtent.
	"Doesn't move!!"
	"upButton morphPosition: borderWidth@borderWidth."
	p := extent - borderWidth - downButton morphExtent.
	downButton morphPosition: p.
	self hideOrShowButtons.
	wasH := oldExtent notNil and: [oldExtent x > oldExtent y].
	isH := self isHorizontal.
	isH = wasH ifFalse: [
		isH
			ifTrue: [
				self updateLeftButton: upButton.
				self updateRightButton: downButton ]
			ifFalse: [
				self updateUpButton: upButton.
				self updateDownButton: downButton ]].! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 10/14/2025 11:42:55'!
freeSliderRoom
	"Answer the length or height of the free slider area, i.e. subtract the slider itself.
	If we are really too short of room, lie a little bit. Answering at least 4, even when the
	free space might be actually negative, makes the scrollbar somewhat usable."

	^ ((self isHorizontal
		ifTrue: [extent x - slider morphWidth]
		ifFalse: [extent y - slider morphHeight])
			- (borderWidth * 2) - (self buttonExtentToUse*2)) max: 4! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 10/14/2025 11:05:01'!
minimumExtent
	^6@6! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 10/14/2025 12:00:34'!
sliderSlack
	^self thickness // 6! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 10/14/2025 11:48:13'!
totalSliderRoom
	"Answer the length or height of the slider area"

	^ (self isHorizontal
		ifTrue: [ extent x ]
		ifFalse: [ extent y ])
			- (borderWidth * 2) - (self buttonExtentToUse * 2).! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 10/14/2025 13:06:52'!
initialize
	super initialize.
	extent := Theme current scrollbarThickness @ 100.
	value := 0.0.
	self recreateSubmorphs.
	scrollDelta := 0.02.
	pageDelta := 0.2.
	interval := 1.0.! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 10/14/2025 12:10:13'!
initializeDownButton
	"Initialize the receiver's downButton"

	| e |
	e := Theme current scrollbarThickness. "To be drawn by any canvas"
	downButton := PluggableButtonMorph new.
	downButton actionTarget: self.
	downButton morphExtent: e@e.
	self addMorph: downButton position: extent - borderWidth - e.
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ self updateRightButton: downButton ]
		ifFalse: [ self updateDownButton: downButton ].
	downButton visible: self showsButtons.! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 10/14/2025 12:10:19'!
initializeUpButton
	"Initialize the receiver's upButton"

	| e |
	e := Theme current scrollbarThickness. "To be drawn by any canvas"
	upButton := PluggableButtonMorph new.
	upButton actionTarget: self.
	upButton morphExtent: e@e.
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ self updateLeftButton: upButton ]
		ifFalse: [ self updateUpButton: upButton ].
	upButton visible: self showsButtons.! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 10/14/2025 11:47:00'!
scrollTo: handPositionRelativeToSlider
	| v handPositionRelativeToUs |
	grabPosition ifNotNil: [
		handPositionRelativeToUs := slider externalize: handPositionRelativeToSlider.
		v := (self isHorizontal
			ifTrue: [ handPositionRelativeToUs x - grabPosition x ]
			ifFalse: [ handPositionRelativeToUs y - grabPosition y ])
				- borderWidth - self buttonExtentToUse * 1.0
					/ self freeSliderRoom.
		self internalScrollValue: v ]! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 10/14/2025 12:00:31'!
sliderGrabbedAt: handPositionRelativeToSlider

	| e p |
	grabPosition := handPositionRelativeToSlider.
	self isHorizontal
		ifTrue: [
			e := slider morphWidth @ self thickness.
			p := slider morphPosition x @ borderWidth ]
		ifFalse: [
			e := self thickness @ slider morphHeight.
			p := borderWidth @ slider morphPosition y ].
	sliderShadow
		position: p extent: e;
		show! !

!methodRemoval: ScrollBar class #scrollbarThickness stamp: 'jmv 10/14/2025 13:24:46'!
ScrollBar class removeSelector: #scrollbarThickness!
!methodRemoval: ScrollBar #buttonExtent stamp: 'jmv 10/14/2025 13:23:51'!
ScrollBar removeSelector: #buttonExtent!

!PluggableScrollPane reorganize!
('access' addToScroller: adoptWidgetsColor: drawKeyboardFocusIndicator: horizontalScrollBar verticalScrollBar)
('events' doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition: mouseScroll:localPosition: scroller)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseScroll:)
('geometry' borderWidth: extentChanged: focusIndicatorBottom focusIndicatorExtent focusIndicatorLeft focusIndicatorRectangle focusIndicatorRight focusIndicatorTop fontPreferenceChanged hLeftoverScrollRange hScrollBarWidth hSetScrollDelta hTotalScrollRange mightNeedHorizontalScrollBar minimumExtent scrollDeltaHeight scrollDeltaHeight: scrollDeltaWidth scrollDeltaWidth: scrollerOffset scrollerOffset: setScrollDeltas someSubmorphPositionOrExtentChanged updateScrollBarsBounds vLeftoverScrollRange vScrollBarHeight vSetScrollDelta vTotalScrollRange viewableArea viewableAreaBottom viewableAreaLeft viewableAreaRight viewableAreaTop viewableAreaTopLeft viewableExtent viewableHeight viewableWidth)
('geometry testing' clipsSubmorphs hIsScrollbarShowing vIsScrollbarShowing)
('initialization' defaultPadding initialize innerMorphClass scroller:)
('scrolling' hHideScrollBar hIsScrollbarNeeded hScrollBarValue: hShowScrollBar hideOrShowScrollBars scrollBy: scrollSelectionIntoView scrollToShow: vHideScrollBar vIsScrollbarNeeded vScrollBarValue: vShowScrollBar)
('scrollbar options' alwaysHideHorizontalScrollbar alwaysHideScrollbarButtons alwaysHideVerticalScrollbar alwaysShowVerticalScrollbar hideScrollBarsIndefinitely showScrollbarButtonsIfPossible)
('testing' drawsKeyboardFocusIndicator is:)
('private' privateMoveFrontMorph:)
!


!ScrollBar reorganize!
('access' color: interval: scrollDelta:pageDelta: scrollValue)
('accessing' adoptWidgetsColor: model:setValueSelector:)
('drawing' drawOn:)
('events' mouseButton1Down:localPosition: mouseStillDown)
('event handling testing' handlesMouseDown: handlesMouseStillDown:)
('geometry' buttonExtentToUse computeSlider expandSlider extentChanged: freeSliderRoom hideOrShowButtons minimumExtent sliderSlack thickness totalSliderRoom)
('initialization' initialize initializeDownButton initializeSlider initializeUpButton recreateSubmorphs updateDownButton: updateLeftButton: updateRightButton: updateUpButton:)
('model access' internalScrollValue: scrollValue:)
('scrolling' scrollByPage scrollDown scrollDown: scrollTo: scrollUp scrollUp: setNextDirectionFromEvent: sliderGrabbedAt: sliderReleased)
('options' showButtons: showsButtons)
('testing' isHorizontal)
!


!ScrollBar class reorganize!
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
ScrollBar allSubInstancesDo: [ :sb | (sb instVarNamed: 'interval') isNil ifTrue: [sb instVarNamed: 'interval' put: 1.0 ]]!