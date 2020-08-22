'From Cuis 5.0 [latest update: #4355] on 21 August 2020 at 12:03:13 pm'!

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/21/2020 11:23:13'!
systemWindowBorderSize
	| w |
	w _ Preferences standardListFont pointSize / 11.
	Theme current minimalWindows ifFalse: [
		w _ w * (Theme current roundWindowCorners ifTrue: [ 4 ] ifFalse: [ 2 ])].
	^w rounded max: 1! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/21/2020 11:57:29'!
drawLabelOn: aCanvas

	| x0 y0 f w availableW l |
	f _ Preferences windowTitleFont.
	x0 _  f lineSpacing * 5 + borderWidth.
	y0 _ borderWidth * 6 // 10.
	availableW _ extent x - x0.
	l _ labelString.
	w _ f widthOfString: l.
	[ w > availableW ] whileTrue: [
		l _ l squeezedTo: (1.0 * l size * availableW / w) truncated.
		l isEmpty ifTrue: [ ^self ].
		w _ f widthOfString: l ].
	aCanvas
		drawString: l
		at: x0@y0
		font: f
		color: Theme current windowLabel
		embossed: Theme current embossedTitles! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/21/2020 10:53:52'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^Preferences systemWindowBorderSize! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/21/2020 12:02:19'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| topLeft spacing |
	topLeft _ (self labelHeight + borderWidth - self titleBarButtonsExtent / 2) ceiling asPoint.
	spacing _ self titleBarButtonsExtent x *14//10.
	self addMorph: self createCloseButton position: topLeft.
	self addMorph: self createCollapseButton position: spacing@0 + topLeft.
	self addMorph: self createExpandButton position: spacing*2@0 + topLeft.
	self addMorph: self createMenuButton position: spacing*3@0 + topLeft! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/21/2020 11:59:53'!
titleBarButtonsExtent
	"answer the extent to use for close & other title bar buttons. 
	 
	the label height is used to be proportional to the fonts preferences"
	| e |
	Theme current minimalWindows ifTrue: [^`0@0`].
	e _ Preferences windowTitleFont pointSize * 13 // 10.
	^e@e! !

!SystemWindow methodsFor: 'label' stamp: 'len 5/1/2020 06:34:51'!
labelHeight
	"Answer the height for the window label."
	Theme current minimalWindows ifTrue: [^ 0].
	^ Preferences windowTitleFont lineSpacing+1! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 8/21/2020 11:07:07'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww pos |
	thickness _ self defaultBorderWidth.
	cornerExtent _ thickness * 5.
	ww _ extent x.
	wh _ extent y.
	w _ ww - cornerExtent - cornerExtent.
	h _ wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) morphPosition: cornerExtent@0 extent: w@thickness.
	(adjusters at: #bottomAdjuster) morphPosition: cornerExtent@(wh-thickness) extent: w@thickness.
	(adjusters at: #leftAdjuster) morphPosition: 0@cornerExtent extent: thickness@h.
	(adjusters at: #rightAdjuster) morphPosition: ww-thickness@cornerExtent extent: thickness@h.
	(adjusters at: #topLeftAdjuster) morphPosition: `0@0` extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster) morphPosition: 0@(wh-cornerExtent) extent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster) morphPosition: ww-cornerExtent@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster) morphPosition: ww@wh-cornerExtent extent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		pos _ borderWidth @ (borderWidth + self labelHeight).
		layoutMorph
			morphPosition: pos
			extent: extent - pos - borderWidth ].

	self layoutNeeded: false.! !


!WindowEdgeAdjustingMorph methodsFor: 'geometry testing' stamp: 'jmv 8/21/2020 11:23:01'!
morphContainsPoint: aLocalPoint
	| sensitiveBorder |
	( self morphLocalBounds containsPoint: aLocalPoint) ifFalse: [ ^false ].
	sensitiveBorder _ owner borderWidth.
	selector caseOf: {
		[ #windowTopLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowTopRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowBottomLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ extent y- aLocalPoint y <= sensitiveBorder ]].
		[ #windowBottomRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ extent y - aLocalPoint y <= sensitiveBorder ]].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."
		^true ]! !


!WindowEdgeAdjustingMorph reorganize!
('initialization' initializeBottom initializeBottomLeft initializeBottomRight initializeLeft initializeRight initializeTop initializeTopLeft initializeTopRight)
('adjusting' adjustOwnerAt:)
('accessing' cursor)
('drawing' drawOn:)
('geometry testing' isOrthoRectangularMorph morphContainsPoint:)
!

