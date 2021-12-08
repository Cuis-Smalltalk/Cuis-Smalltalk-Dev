'From Cuis 5.0 [latest update: #4932] on 18 October 2021 at 7:09:32 pm'!

!PluggableScrollPane commentStamp: '<historical>' prior: 0!
Allows viewing just part of a larger Morph. The scroll values vary from 0.0 to 1.0.!


!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/18/2021 11:24:30'!
adjustExtent
	"Morphs intended to be included in PluggableScrollPanes as scrolled content might need to adjust their extent.
	NOP by default."! !


!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 10/18/2021 11:23:18'!
scroller: contents

	scroller ifNotNil: [ scroller delete ].
	scroller _ contents.
	self addMorphBack: scroller.
	self scrollerOffset: `0@ 0`.! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 10/18/2021 14:10:08'!
keyStroke: aKeyboardEvent

	( self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	super keyStroke: aKeyboardEvent.
	scroller ifNotNil: [
		scroller keyStroke: aKeyboardEvent ].! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 10/18/2021 14:10:42'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	scroller ifNotNil: [
		eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
		scroller mouseButton1Down: aMouseButtonEvent localPosition: eventPositionLocalToScroller ].
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 10/18/2021 14:10:57'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	scroller ifNotNil: [
		eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
		scroller mouseButton1Up: aMouseButtonEvent localPosition: eventPositionLocalToScroller ].! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 10/18/2021 14:11:19'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	scroller ifNotNil: [
		eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
		scroller  mouseMove: aMouseMoveEvent localPosition: eventPositionLocalToScroller ].! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/18/2021 14:08:49'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	"Now reset widget sizes"
	scroller ifNotNil: [
		scroller adjustExtent ].
	self updateScrollBarsBounds.
	self setScrollDeltas.
	self scrollSelectionIntoView ! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/18/2021 13:02:17'!
hTotalScrollRange
	"Return the width extent of the receiver's scrollable area"
	scroller ifNil: [ ^0 ].
	^scroller morphExtentInOwner x! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/18/2021 13:02:44'!
minimumExtent
	| minW minH scrollerExtent |
	scrollerExtent _ scroller ifNil: [ 0@0 ] ifNotNil: [ scroller morphExtentInOwner ].
	"Figure out the minimum extent for this pane so that either content, or at least required scrollbars, will fit"
	minW _ self xtraBorder * 2 + scrollerExtent x min: ScrollBar scrollbarThickness * 2.
	self vIsScrollbarShowing
		ifTrue: [
			minW _ minW + ScrollBar scrollbarThickness].
	minH _ self xtraBorder * 2 + scrollerExtent y.
	self hIsScrollbarShowing
		ifTrue: [
			minH _ minH + ScrollBar scrollbarThickness].
	minH _ minH min: ScrollBar scrollbarThickness * 2.
	^ (minW + (borderWidth * 2)) @ (minH + (borderWidth * 2))! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/18/2021 13:02:01'!
vTotalScrollRange
	"Return the height extent of the receiver's scrollable area"
	scroller ifNil: [ ^0 ].
	^scroller morphExtentInOwner y! !

!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 10/18/2021 19:06:12'!
initialize
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars _ #showIfNeeded.

	"initialize the receiver's scrollBars"
	scrollBar _ ScrollBar new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar _ ScrollBar new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator _ Preferences drawKeyboardFocusIndicator.

	self innerMorphClass ifNotNil: [ :contentsClass |
		self scroller: contentsClass new ].
	self addMorph: scrollBar.
	self addMorph: hScrollBar.! !

!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 10/18/2021 11:18:31'!
innerMorphClass
	^nil! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 10/18/2021 19:07:42'!
hHideScrollBar
	hScrollBar hide.
	scroller ifNotNil: [
		scroller adjustExtent ].! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 10/18/2021 19:08:19'!
hScrollBarValue: scrollValue

	| x |
	scroller ifNotNil: [
		self hIsScrollbarShowing ifFalse: [
			^self scrollerOffset: 0@self scrollerOffset y ].
		(x _ self hLeftoverScrollRange * scrollValue) <= 0
			ifTrue: [ x _ 0 ].
		self scrollerOffset: x@self scrollerOffset y ].! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 10/18/2021 19:09:01'!
hShowScrollBar

	hScrollBar show.
	scroller ifNotNil: [
		scroller adjustExtent ].! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 10/18/2021 14:12:38'!
scrollBy: delta
	"Move the contents in the direction delta."

	| newYoffset yRange xRange newXoffset |
	
	"Set the offset on the scroller"
	yRange _ self vLeftoverScrollRange.
	xRange _ self hLeftoverScrollRange.
	
	scroller ifNotNil: [
		newYoffset _ self scrollerOffset y - delta y min: yRange max: 0.
		newXoffset _ self scrollerOffset x - delta x min: xRange max: 0.	
		self scrollerOffset: newXoffset@newYoffset ].

	"Update the scrollBars"
	scrollBar scrollValue: (yRange ifNotZero: [newYoffset asFloat / yRange]).
	hScrollBar scrollValue: (xRange ifNotZero: [newXoffset asFloat / xRange]).! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 10/18/2021 19:07:48'!
vHideScrollBar
	scrollBar hide.
	scroller ifNotNil: [
		scroller adjustExtent ].! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 10/18/2021 19:08:44'!
vScrollBarValue: scrollValue

	scroller ifNotNil: [
		self scrollerOffset: 
			(self scrollerOffset x @ 
			(self vLeftoverScrollRange * scrollValue) rounded) ].! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 10/18/2021 19:09:04'!
vShowScrollBar

	scrollBar show.
	scroller ifNotNil: [
		scroller adjustExtent ].! !

!methodRemoval: InnerPluggableMorph #adjustExtent stamp: 'jmv 10/18/2021 19:04:01'!
InnerPluggableMorph removeSelector: #adjustExtent!

!InnerPluggableMorph reorganize!
('as yet unclassified')
!

