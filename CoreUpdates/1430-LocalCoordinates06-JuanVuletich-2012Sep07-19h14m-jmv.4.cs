'From Cuis 4.0 of 21 April 2012 [latest update: #1429] on 7 September 2012 at 7:49:21 pm'!

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/7/2012 18:33'!
zzfillRectangle: aRectangle colorOrInfiniteForm: aColorOrInfiniteForm borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

	| displayRectangle bw |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	^self fillRectangle: displayRectangle colorOrInfiniteForm: aColorOrInfiniteForm borderWidth: bw borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/7/2012 18:33'!
zzroundRect: aRectangle color: aColor radius: radious
	"
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10
	"
	"top stripe"
	| displayRectangle r |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	r _ (currentTransformation externalizeScalar: radious) rounded.
	self
		image: (FormCanvas topLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: displayRectangle topLeft.
	self
		image: (FormCanvas topRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: displayRectangle topRight - (r@0).
	self fillRectangle: ((displayRectangle withHeight: r) insetBy: r@0) colorOrInfiniteForm: aColor.

	"center stripe"
	self fillRectangle: (displayRectangle insetBy: (0 @ r corner: 0 @ r)) colorOrInfiniteForm: aColor.
	
	"bottom stripe"
	self
		image: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: displayRectangle bottomLeft - (0@r).
	self
		image: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: displayRectangle bottomRight - (r@r) .
	self fillRectangle: ((displayRectangle bottomLeft + (r@r negated)) extent: (displayRectangle width - r - r@r)) colorOrInfiniteForm: aColor! !


!Morph methodsFor: 'geometry' stamp: 'jmv 9/7/2012 19:49'!
morphExtentInWorld
	"eventually, remove. If not, at least translate!!!!!!!!!!"
	self flag: #jmvVer2.
	^self morphExtent! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 9/7/2012 19:03'!
createAcceptButton
	"create the [accept] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current acceptButton;
		 label: 'Accept';
		 action: #acceptClicked.
	self addMorph: result.
	result morphPositionInOwner: 29@90.
	result morphExtent: 93@27.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 9/7/2012 19:04'!
createCancelButton
	"create the [cancel] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current cancelButton;
		 label: 'Cancel';
		 action: #cancelClicked.
	self addMorph: result.
	result morphPositionInOwner: 149@90.
	result morphExtent: 93@27.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 9/7/2012 19:04'!
createQueryTextMorph: queryString 
	"create the queryTextMorph"
	| result |
	result _ StringMorph new contents: queryString.
	result lock.
	self addMorph: result.
	result morphPositionInOwner: 30@7.
	result morphExtent: 239@15.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 9/7/2012 19:03'!
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
	result morphExtent: self morphExtent-(28@62).
	^ result! !

!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 18:40'!
drawOn: aCanvas
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas
				zzroundRect: (0@0 extent: self morphExtent)
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ].
	aCanvas
		zzfillRectangle: (14@25 extent: self morphExtent-(28@62))
		colorOrInfiniteForm: (Theme current paneBackgroundFrom: color)! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 9/7/2012 19:22'!
adjustExtent
	"And reposition submorphs"
	| w h y |
	"make all items wide, so selection indicator is wide too"
	w _ self desiredWidth.
	y _ 0.
	self submorphsDo: [ :m |
		m
			morphPositionInOwner: 0@y;
			morphWidth: w.
		h _ m morphHeight.
		y _ y + h ].
	self morphExtent: w@y! !


!InnerListMorph methodsFor: 'list management' stamp: 'jmv 9/7/2012 19:46'!
drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!!"

	self flag: #jmvVer2.
	"revisar senders"
	^ 0 @ (row - 1 * font height) extent: extent x @ font height! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 9/7/2012 19:46'!
extentForComposing
	self flag: #jmvVer2.	"like #extent ..."
	^wrapFlag
		ifTrue: [ extent x @ 9999999 ]
		ifFalse: [ 9999999@9999999 ]! !


!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 19:47'!
displayInsertionMarkAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas
	| caretColor x1 isBold isItalic x0 h w halfW r d |
	isBold _ emphasis allMask: 1.
	isItalic _ emphasis allMask: 2.
	caretColor _ Theme current insertionPoint.
	h _ bottom - top.
	w _ isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW _ w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed!!"
			d _ isBold ifTrue: [ 3 ] ifFalse: [ h // 24].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x.
			x1 _ x].
	x0 < halfW ifTrue: [
		x1 _ x1 - x0 + halfW.
		x0 _ halfW ].
	r _ extent x-halfW-1.
	r < x1 ifTrue: [
		x0 _ x0 + r - x1.
		x1 _ r ].
	caretRect _ x0-halfW-1@ top corner: x1+halfW+1+1 @ bottom.
	aCanvas
		zzline: x0@(bottom-halfW) to: x1@(top+halfW)
		width: w color: caretColor! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/7/2012 19:18'!
focusIndicatorExtent
	| e |
	e _ self morphExtent - borderWidth - borderWidth.
	self vIsScrollbarShowing ifTrue: [
		e _ e x - self scrollBarClass scrollbarThickness @ e y ].
	self hIsScrollbarShowing ifTrue: [
		e _ e x @ (e y - self scrollBarClass scrollbarThickness) ].
	^e! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/7/2012 19:17'!
hScrollBarWidth
	"Return the width of the horizontal scrollbar"

	| w |	
	w _ self morphWidth - (2 * borderWidth).
	self vIsScrollbarShowing
		ifTrue: [ w _ w - self scrollBarClass scrollbarThickness ].
	^w! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/7/2012 19:16'!
updateScrollBarsBounds
	
	| t |
	hideScrollBars ifTrue: [^self].
	t _ self scrollBarClass scrollbarThickness.
	scrollBar
		morphPositionInOwner: self morphWidth-t-borderWidth @ borderWidth;
		morphExtent: t @ self vScrollBarHeight.
	hScrollBar
		morphPositionInOwner: borderWidth @ (self morphHeight - t - borderWidth);
		morphExtent: self hScrollBarWidth@t! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/7/2012 19:17'!
vScrollBarHeight
	^self morphHeight - (2 * borderWidth)! !


!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 9/7/2012 19:46'!
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
	last morphPositionInOwner y + last morphExtent y < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^scroller 
		findSubmorphBinary: [ :m |
			(m morphPositionInOwner y <= ptY and: [ m morphPositionInOwner y + m morphExtent y >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPositionInOwner y + (m morphExtent y // 2) > ptY ifTrue: [-1] ifFalse: [1]]]! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 9/7/2012 19:46'!
scrollSelectionIntoView

	selectedMorph ifNotNil: [
		self flag: #jmvVer2.	"traducir mejor el rectangulo..."
		self scrollToShow: ((scroller externalize: selectedMorph morphPositionInOwner) extent: selectedMorph morphExtent) ]! !


!SystemWindow methodsFor: 'layout' stamp: 'jmv 9/7/2012 19:39'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww |
	thickness _ 4.
	cornerExtent _ 20.
	ww _ extent x.
	wh _ extent y.
	w _ ww - cornerExtent - cornerExtent.
	h _ ww - cornerExtent - cornerExtent.
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

!methodRemoval: RectangleLikeMorph #morphExtentInOwner!
RectangleLikeMorph removeSelector: #morphExtentInOwner!
!methodRemoval: Morph #morphExtentInOwner!
Morph removeSelector: #morphExtentInOwner!
