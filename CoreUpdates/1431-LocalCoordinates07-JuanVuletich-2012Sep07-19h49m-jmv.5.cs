'From Cuis 4.0 of 21 April 2012 [latest update: #1429] on 7 September 2012 at 11:25:51 pm'!

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/7/2012 23:25'!
morphHeight: aNumber

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	self morphExtent: extent x@aNumber! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/7/2012 23:25'!
morphWidth: aNumber

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	self morphExtent: aNumber@extent y! !


!BorderedRectMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 19:53'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		zzfillRectangle: (0@0 extent: extent)
		colorOrInfiniteForm: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 19:52'!
drawOn: aCanvas
	| rectangle w y0 h y1 y2 scrollbarThickness |
	aCanvas zzframeAndFillRectangle: (0@0 extent: extent) fillColor: self color borderWidth: borderWidth borderColor: borderColor.
	y0 _ 1.
	w _ extent x-2.
	scrollbarThickness _ ScrollBar scrollbarThickness.
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ w - scrollbarThickness.
		aCanvas
			zzframeRectangle: (extent x - scrollbarThickness@0
				extent: scrollbarThickness @ extent y)
			borderWidth: 1
			color: borderColor.
		aCanvas
			zzimage: (FormCanvas arrowOfDirection: #up size: scrollbarThickness)
			at: extent x - scrollbarThickness@0.
		aCanvas
			zzimage: (FormCanvas arrowOfDirection: #down size: scrollbarThickness)
			at: 0@0 + extent - scrollbarThickness.
		h _ extent y - (2 * scrollbarThickness).
		y1 _ (1.0 * self firstVisible-1 / completer entryCount * h) ceiling + y0 + scrollbarThickness-1.
		y2 _ (1.0 * self lastVisible / completer entryCount * h) floor + y0 + scrollbarThickness -1.
		aCanvas
			zzfillRectangle: (extent x - scrollbarThickness+2@y1 corner:  extent x-2 @ y2)
			colorOrInfiniteForm: Color veryLightGray ].
	self firstVisible
		to: self lastVisible
		do: [ :index |
			rectangle _ 1@y0 extent: w@self class itemHeight.
			index = self selected
				ifTrue: [
					aCanvas zzfillRectangle: rectangle colorOrInfiniteForm: (Theme current listHighlightFocused: true) ].
			aCanvas
				zzdrawString: (completer entries at: index) asString
				in: rectangle
				font: self class listFont
				color: Theme current text.
			y0 _ y0 + self itemHeight ]! !


!EllipseMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 19:53'!
drawOn: aCanvas 

	| r bc bw |
	r _ 0@0 extent: extent.
	bw _ borderWidth.
	bc _ borderColor.
	aCanvas isShadowDrawing
		ifTrue: [
			bw _ 0.
			bc _ nil ].
	aCanvas zzfillOval: r color: color borderWidth: bw borderColor: bc.
! !

!EllipseMorph methodsFor: 'geometry testing' stamp: 'jmv 9/7/2012 23:03'!
containsPoint: aPoint

	| radius other delta xOverY e |
	(self morphBoundsInWorld containsPoint: aPoint) ifFalse: [^ false].  "quick elimination"
	e _ self morphExtentInWorld.
	e > (1@1)
		ifFalse: [^ true].  "Degenerate case -- code below fails by a bit"

	radius _ e y asFloat / 2.
	other _ e x asFloat / 2.
	delta _ aPoint - self morphPositionInWorld - (other@radius).
	xOverY _ e x asFloat / e y asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 9/7/2012 19:54'!
createTextPaneExtent: answerExtent acceptBoolean: acceptBoolean
	"create the textPane"
	| result |
	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval
				allowStyler: true.
	result morphExtent: answerExtent.
	result borderWidth: 1; borderColor: Color lightGray.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	self addMorph: result.
	result morphPositionInOwner: 14@25.
	result morphExtent: extent-(28@62).
	^ result! !

!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 19:54'!
drawOn: aCanvas
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas
				zzroundRect: (0@0 extent: extent)
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ].
	aCanvas
		zzfillRectangle: (14@25 extent: extent-(28@62))
		colorOrInfiniteForm: (Theme current paneBackgroundFrom: color)! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 9/7/2012 23:17'!
addNameBeneath: outerRectangle string: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition w nameBackground |
	w _ self world ifNil: [ target world ].
	nameBackground _ RectangleLikeMorph new
		color: (Color lightBlue alpha: 0.9).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color magenta.
	self addMorph: nameBackground.
	self addMorph: nameMorph.
	namePosition _ outerRectangle bottomCenter - ((nameMorph morphWidth // 2) @ (self handleSize negated // 2 - 1)).
	nameMorph morphPosition: (namePosition min: w viewBox bottomRight - nameMorph morphHeight + 5).
	nameBackground morphBoundsInWorld: (nameMorph morphBoundsInWorld outsetBy: 2).
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/7/2012 23:16'!
basicBox
	| aBox minSide anExtent w |
	minSide _ 4 * self handleSize.
	anExtent _ ((extent x + self handleSize + 8) max: minSide) @
				((extent y + self handleSize + 8) max: minSide).
	aBox _ Rectangle center: self morphBoundsInWorld center extent: anExtent.
	w _ self world ifNil: [ target outermostWorldMorph ].
	^ w
		ifNil:
			[ aBox ]
		ifNotNil:
			[ aBox intersect: (w viewBox insetBy: 8@8) ]! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 19:54'!
drawOn: aCanvas

	| tRect sRect colorToUse sLeft aForm centeringOffset |
	isSelected ifTrue: [
		aCanvas
			zzfillRectangle: (0@0 extent: extent)
			colorOrInfiniteForm: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].

	complexContents hasContents ifTrue: [
		tRect _ self toggleRectangle.
		aForm _ isExpanded 
			ifTrue: [ container expandedForm ]
			ifFalse: [ container notExpandedForm ].
		centeringOffset _ ((tRect height - aForm extent y) / 2.0) rounded.
		aCanvas 
			zzimage: aForm 
			at: (tRect topLeft translatedBy: 0 @ centeringOffset) ].

	sLeft _ indentLevel * 12 + 16.
	sRect _ sLeft@0 extent: extent - (sLeft@0).
	colorToUse _ complexContents preferredColor ifNil: [ color ].
	aCanvas
		zzdrawString: contents asString
		in: sRect
		font: self fontToUse
		color: colorToUse! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 9/7/2012 23:16'!
model: aTextModel wrappedTo: width
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	wrapFlag _ true.
	self basicExtent: width truncated@extent y.
	self model: aTextModel! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 9/7/2012 23:14'!
popUpAdjacentTo: rightOrLeftPoint forHand: hand from: sourceItem
	"Present this menu at the given point under control of the given hand."

	| delta tryToPlace selectedOffset |
	hand world startSteppingSubmorphsOf: self.
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition - self morphPosition.
	sourceItem owner owner addMorphFront: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self morphFullBoundsInWorld amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPoint first value: false;
		value: rightOrLeftPoint last - (extent x @ 0) value: false;
		value: rightOrLeftPoint first value: true! !

!MenuMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 19:55'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas zzroundRect: (0@0 extent: extent) color: color radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas zzfillRectangle: (0@0 extent: extent) colorOrInfiniteForm: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/7/2012 19:57'!
focusIndicatorExtent
	| e |
	e _ extent - borderWidth - borderWidth.
	self vIsScrollbarShowing ifTrue: [
		e _ e x - self scrollBarClass scrollbarThickness @ e y ].
	self hIsScrollbarShowing ifTrue: [
		e _ e x @ (e y - self scrollBarClass scrollbarThickness) ].
	^e! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/7/2012 19:58'!
focusIndicatorRectangle

	| topLeft bottomRight |
	topLeft _ borderWidth@borderWidth.
	bottomRight _ extent - borderWidth.
	self vIsScrollbarShowing ifTrue: [
		bottomRight _ bottomRight x - self scrollBarClass scrollbarThickness@ bottomRight y].
	self hIsScrollbarShowing ifTrue: [
		bottomRight _ bottomRight x @ 
			(bottomRight y - self scrollBarClass scrollbarThickness)].
	^topLeft corner: bottomRight! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/7/2012 23:15'!
hScrollBarWidth
	"Return the width of the horizontal scrollbar"

	| w |	
	w _ extent x - (2 * borderWidth).
	self vIsScrollbarShowing
		ifTrue: [ w _ w - self scrollBarClass scrollbarThickness ].
	^w! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/7/2012 23:15'!
updateScrollBarsBounds
	
	| t |
	hideScrollBars ifTrue: [^self].
	t _ self scrollBarClass scrollbarThickness.
	scrollBar
		morphPositionInOwner: extent x - t - borderWidth @ borderWidth;
		morphExtent: t @ self vScrollBarHeight.
	hScrollBar
		morphPositionInOwner: borderWidth @ (extent y - t - borderWidth);
		morphExtent: self hScrollBarWidth@t! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/7/2012 23:16'!
vScrollBarHeight
	^extent y - (2 * borderWidth)! !


!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 9/7/2012 23:17'!
itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	| ptY last internalPoint |
	internalPoint _ scroller internalize: aPoint.
	scroller hasSubmorphs ifFalse: [ ^nil ].
	(internalPoint > (0@0) and: [ internalPoint < scroller morphExtent ]) ifFalse: [ ^nil ].
	ptY _ internalPoint y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	scroller firstSubmorph morphPositionInOwner y > ptY ifTrue: [ ^nil ].
	last _ scroller lastSubmorph.
	last morphPositionInOwner y + last morphHeight < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^scroller 
		findSubmorphBinary: [ :m |
			(m morphPositionInOwner y <= ptY and: [ m morphPositionInOwner y + m morphHeight >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPositionInOwner y + (m morphHeight // 2) > ptY ifTrue: [-1] ifFalse: [1]]]! !


!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 9/7/2012 23:14'!
initialize
	super initialize.
	scroller morphWidth: extent x.
	scroller color: self textColor.! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 9/7/2012 23:18'!
freeSliderRoom
	"Answer the length or height of the free slider area, i.e. substract the slider itself"

	^ (self isHorizontal
		ifTrue: [ extent x - slider morphWidth]
		ifFalse: [ extent y - slider morphHeight])
			- (borderWidth * 2) - (self buttonExtent * 2).! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 9/7/2012 20:00'!
totalSliderRoom
	"Answer the length or height of the slider area"

	^ (self isHorizontal
		ifTrue: [ extent x ]
		ifFalse: [ extent y ])
			- (borderWidth * 2) - (self buttonExtent * 2).! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 9/7/2012 20:00'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	self addMorph: downButton.
	downButton
		morphPositionInOwner: extent - borderWidth - e;
		morphExtent: e@e.
	self isHorizontal
		ifTrue: [ downButton updateRightButtonImage ]
		ifFalse: [ downButton updateDownButtonImage ]! !

!ScrollBar methodsFor: 'testing' stamp: 'jmv 9/7/2012 20:00'!
isHorizontal

	^extent x > extent y! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:12'!
drawClassicFrameOn: aCanvas color: titleColor
	"Window border encompasses title area. No round corners. No title gradient."

	aCanvas zzfillRectangle: (0@0 extent: extent) colorOrInfiniteForm: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self widgetsColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas zzfillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight) colorOrInfiniteForm: titleColor! !

!SystemWindow methodsFor: 'change reporting' stamp: 'jmv 9/7/2012 23:13'!
invalidateTitleArea

	"not really pretty... also invalidating the top border, regardless of it being above or below the title area
	(Different themes use various looks, this covers them all)"
	self zzinvalidRect: (0@0 extent: extent x @ (self labelHeight + borderWidth))! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 9/7/2012 19:52'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww |
	thickness _ 4.
	cornerExtent _ 20.
	ww _ extent x.
	wh _ extent y.
	w _ ww - cornerExtent - cornerExtent.
	h _ wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster)
		morphPositionInOwner: cornerExtent@0;
		morphExtent: w@thickness.
	(adjusters at: #bottomAdjuster)
		morphPositionInOwner: cornerExtent@(wh-thickness);
		morphExtent: w@thickness.
	(adjusters at: #leftAdjuster)
		morphPositionInOwner: 0@cornerExtent;
		morphExtent: thickness@h.
	(adjusters at: #rightAdjuster)
		morphPositionInOwner: ww-thickness@cornerExtent;
		morphExtent: thickness@h.
	(adjusters at: #topLeftAdjuster)
		morphPositionInOwner: 0@0;
		morphExtent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster)
		morphPositionInOwner: 0@(wh-cornerExtent);
		morphExtent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster)
		morphPositionInOwner: ww-cornerExtent@0;
		morphExtent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster)
		morphPositionInOwner: ww@wh-cornerExtent;
		morphExtent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		layoutMorph morphBoundsInWorld: self layoutBounds ].
	
	layoutNeeded _ false! !

!methodRemoval: SystemWindow #titleAreaInnerRect!
SystemWindow removeSelector: #titleAreaInnerRect!
!methodRemoval: Morph #morphHeight:!
Morph removeSelector: #morphHeight:!
!methodRemoval: Morph #morphWidth:!
Morph removeSelector: #morphWidth:!
