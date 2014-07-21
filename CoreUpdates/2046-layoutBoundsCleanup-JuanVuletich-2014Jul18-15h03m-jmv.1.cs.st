'From Cuis 4.2 of 25 July 2013 [latest update: #2044] on 18 July 2014 at 3:30:09.808581 pm'!

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 7/18/2014 15:28'!
layoutBounds
	"Return the bounds for laying out children of the receiver"

	^0@0 extent: extent! !


!SystemWindow methodsFor: 'layout' stamp: 'jmv 7/18/2014 15:24'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww pos |
	thickness _ 4.
	cornerExtent _ 20.
	ww _ extent x.
	wh _ extent y.
	w _ ww - cornerExtent - cornerExtent.
	h _ wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) morphPosition: cornerExtent@0 extent: w@thickness.
	(adjusters at: #bottomAdjuster) morphPosition: cornerExtent@(wh-thickness) extent: w@thickness.
	(adjusters at: #leftAdjuster) morphPosition: 0@cornerExtent extent: thickness@h.
	(adjusters at: #rightAdjuster) morphPosition: ww-thickness@cornerExtent extent: thickness@h.
	(adjusters at: #topLeftAdjuster) morphPosition: 0@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster) morphPosition: 0@(wh-cornerExtent) extent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster) morphPosition: ww-cornerExtent@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster) morphPosition: ww@wh-cornerExtent extent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		pos _ borderWidth @ (borderWidth + self labelHeight).
		layoutMorph
			morphPosition: pos
			extent: self morphExtent - pos - borderWidth ].
	
	layoutNeeded _ false! !

!methodRemoval: SystemWindow #layoutBounds!
SystemWindow removeSelector: #layoutBounds!
!methodRemoval: BorderedRectMorph #layoutBounds!
BorderedRectMorph removeSelector: #layoutBounds!
!methodRemoval: Morph #layoutBounds!
Morph removeSelector: #layoutBounds!
