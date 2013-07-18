'From Cuis 4.1 of 12 December 2012 [latest update: #1657] on 2 April 2013 at 7:13:04 pm'!

!PluggableScrollPane methodsFor: 'drawing' stamp: 'jmv 4/2/2013 19:00'!
clipsSubmorphs
	"Drawing specific. If this property is set, clip the receiver's  
	submorphs to the receiver's clipping bounds.
	Maybe move up to RectangleLikeMorph"
	^ true! !


!Morph methodsFor: 'accessing - extension' stamp: 'jmv 4/2/2013 18:45'!
assureExtension
	"creates an extension for the receiver if needed"
	self hasExtension
		ifFalse: [self privateExtension: MorphExtension new].
	^ self extension! !

!Morph methodsFor: 'drawing' stamp: 'jmv 4/2/2013 18:40'!
clipsSubmorphs
	"Drawing specific. If this property is set, clip the receiver's  
	submorphs to the receiver's clipping bounds."
	^ false! !

!Morph methodsFor: 'menus' stamp: 'jmv 4/2/2013 18:26'!
addToggleItemsToHaloMenu: aMenu
	"Add standard true/false-checkbox items to the memu"

	#(
		(stickinessString toggleStickiness 'whether I should be resistant to a drag done by mousing down on me')
		(lockedString lockUnlockMorph 'when "locked", I am inert to all user interactions')
	) do: [ :trip | 
			aMenu addUpdating: trip first action: trip second.
			aMenu balloonTextForLastItem: trip third ]! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 4/2/2013 18:37'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| row |
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		self listMorph noSelection].
	row _ self rowAtLocation: localEventPosition.
	row = 0  ifTrue: [
		^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil! !


!PluggableScrollPane reorganize!
('access' addToScroller: adoptWidgetsColor: drawKeyboardFocusIndicator: verticalScrollBar)
('access options' hideScrollBarsIndefinitely)
('drawing' clipsSubmorphs)
('events' doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition: scrollByKeyboard: scroller)
('event handling testing' handlesMouseDown: handlesMouseOver:)
('geometry' borderWidth: focusIndicatorExtent focusIndicatorRectangle hLeftoverScrollRange hScrollBarWidth hSetScrollDelta hTotalScrollRange mightNeedHorizontalScrollBar morphExtent: scrollDeltaHeight scrollDeltaWidth scrollerOffset scrollerOffset: setScrollDeltas someSubmorphPositionOrExtentChanged updateScrollBarsBounds vLeftoverScrollRange vScrollBarHeight vSetScrollDelta vTotalScrollRange viewableExtent viewableHeight viewableWidth xtraBorder)
('geometry testing' hIsScrollbarShowing vIsScrollbarShowing)
('initialization' defaultBorderColor initialize innerMorphClass scrollBarClass)
('scroll bar events' mouseButton2Activity)
('scrolling' hHideScrollBar hIsScrollbarNeeded hScrollBarValue: hShowScrollBar hideOrShowScrollBars scrollBy: scrollToShow: vHideScrollBar vIsScrollbarNeeded vScrollBarValue: vShowScrollBar)
('testing' is:)
!

!methodRemoval: PasteUpMorph #clipsSubmorphs!
PasteUpMorph removeSelector: #clipsSubmorphs!
!methodRemoval: PasteUpMorph #initialize!
PasteUpMorph removeSelector: #initialize!
!methodRemoval: LayoutMorph #clipsSubmorphs!
LayoutMorph removeSelector: #clipsSubmorphs!
!methodRemoval: BorderedRectMorph #clipsSubmorphs!
BorderedRectMorph removeSelector: #clipsSubmorphs!
!methodRemoval: Morph #changeClipSubmorphs!
Morph removeSelector: #changeClipSubmorphs!
!methodRemoval: Morph #clipSubmorphs:!
Morph removeSelector: #clipSubmorphs:!
!methodRemoval: Morph #hasClipSubmorphsString!
Morph removeSelector: #hasClipSubmorphsString!
!methodRemoval: Morph #initializeExtension!
Morph removeSelector: #initializeExtension!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

PasteUpMorph allInstancesDo: [ :w |
	w privateExtension: nil.
	w activeHand privateExtension: nil ].
Morph allSubInstancesDo: [ :m | m resetExtension ].
Smalltalk garbageCollect.!

