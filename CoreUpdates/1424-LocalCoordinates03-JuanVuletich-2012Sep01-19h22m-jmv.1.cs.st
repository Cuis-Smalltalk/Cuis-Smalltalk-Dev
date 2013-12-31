'From Cuis 4.0 of 21 April 2012 [latest update: #1423] on 1 September 2012 at 7:39:19 pm'!

!FormCanvas methodsFor: 'drawing' stamp: 'jmv 8/31/2012 13:42'!
zzline: pt1 to: pt2 width: w color: c
	| offset p1 p2 |
	p1 _ currentTransformation transform: pt1.
	p2 _ currentTransformation transform: pt2.
	offset _ origin - (w // 2) asPoint.
	self setPaintColor: c.
	port
		width: w;
		height: w;
		drawFrom: (p1 rounded + offset) to: (p2 rounded + offset)! !

!FormCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 8/31/2012 13:49'!
zzfillOval: r color: fillColor borderWidth: borderWidth borderColor: borderColor

	| rect displayRectangle |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: r.
	"draw the border of the oval"
	rect _ (displayRectangle translatedBy: origin) truncated.
	(borderWidth = 0 or: [borderColor isTransparent]) ifFalse:[
		self setPaintColor: borderColor.
		port frameOval: rect borderWidth: borderWidth].
	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillOval: (rect insetBy: borderWidth)].
! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/31/2012 13:37'!
zzframeAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect |
	rect _ (currentTransformation displayBoundsOfTransformOf: r) translatedBy: origin.
	"draw the border of the rectangle"
	borderColor isTransparent ifFalse:[
		self setPaintColor: borderColor.
		port frameRect: rect borderWidth: borderWidth ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillRect: (rect insetBy: borderWidth) offset: origin ]! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/31/2012 17:18'!
zzframeRectangle: r borderWidth: borderWidth color: borderColor
	"
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red
	"
	| rect |
	rect _ (currentTransformation displayBoundsOfTransformOf: r) translatedBy: origin.
	self setPaintColor: borderColor.
	port
		frameRect: rect
		borderWidth: borderWidth.! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 8/31/2012 13:53'!
zzroundRect: aRectangle color: aColor radius: r
	"
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10
	"
	"top stripe"
	| displayRectangle |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
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


!FormCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/31/2012 13:12'!
onForm: aForm

	^ self new setForm: aForm
! !

!FormCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/31/2012 13:11'!
withExtent: extent depth: depth

	^ self new setForm: (Form extent: extent depth: depth)! !


!RectangleLikeMorph methodsFor: 'geometry testing' stamp: 'jmv 8/31/2012 13:24'!
isOrthoRectangularMorph
	"Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and
	specified by my bounds instance variable.
	If true, #containsPoint: can simply check #bounds."
	^true! !


!Form methodsFor: 'accessing' stamp: 'jmv 8/31/2012 13:12'!
getCanvas
	"Return a Canvas that can be used to draw onto the receiver"
	^self defaultCanvasClass onForm: self! !


!FormCanvas methodsFor: 'private' stamp: 'jmv 8/31/2012 13:04'!
setForm: aForm

	form := aForm.
	port := self portClass toForm: form.

	"this was the contents of the #reset method"
	origin := 0@0.							"origin of the top-left corner of this cavas"
	clipRect := (0@0 corner: form extent).	"default clipping rectangle"
	shadowColor _ nil! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 8/31/2012 13:38'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	"
	This is starting to work:
		| c |
		c _ Display getCanvas initTransformationsFor: World.
		World submorphsDo: [ :m | c fullDraw: m ].
	"

	"To replace #fullDrawOn:"
	self flag: #jmvVer3.

	aMorph visible ifFalse: [^ self].
	(self isVisible: aMorph morphFullBoundsInWorld) ifFalse: [^ self].		"Needs fullBounds 'in owner' if inside a scroller"

	self into: aMorph.

	aMorph isKnownFailing 
		ifTrue: [ aMorph drawErrorOn: self ]
		ifFalse: [ aMorph fullDrawOn: self ].

	self outOf: aMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 8/31/2012 12:56'!
initTransformationsFor: aWorld

	"Maybe fold into senders"
	self flag: #jmvVer2.

	currentTransformation _ aWorld location.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ]! !


!Morph methodsFor: 'drawing' stamp: 'jmv 8/31/2012 13:46'!
drawErrorOn: aCanvas
	"The morph (or one of its submorphs) had an error in its drawing method."
	| tl br w |
	w _ 10.
	tl _ 0@0.
	br _ self morphExtent.
	aCanvas
		zzframeAndFillRectangle: (tl corner: br)
		fillColor: Color red
		borderWidth: w
		borderColor: Color yellow.

	aCanvas zzline: tl +w to: br -w width: w color: Color yellow.
	aCanvas zzline: br x - w @ w to: w @ (br y - w) width: w color: Color yellow.! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 8/31/2012 13:27'!
isOrthoRectangularMorph
	"Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and
	specified by my bounds instance variable.
	If true, #containsPoint: can simply check #bounds."
	^false! !


!EllipseMorph methodsFor: 'drawing' stamp: 'jmv 8/31/2012 13:50'!
drawOn: aCanvas 

	| r bc bw |
	r _ 0@0 extent: self morphExtent..
	bw _ borderWidth.
	bc _ borderColor.
	aCanvas isShadowDrawing
		ifTrue: [
			bw _ 0.
			bc _ nil ].
	aCanvas zzfillOval: r color: color borderWidth: bw borderColor: bc.
! !


!MenuMorph methodsFor: 'drawing' stamp: 'jmv 8/31/2012 13:55'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas zzroundRect: (0@0 extent: self morphExtent) color: color radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas zzfillRectangle: (0@0 extent: self morphExtent) colorOrInfiniteForm: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/31/2012 17:29'!
focusIndicatorRectangle

	| topLeft bottomRight |
	topLeft _ 0@0.
	bottomRight _ topLeft + self morphExtent.
	topLeft _ topLeft + borderWidth.
	bottomRight _ bottomRight - borderWidth.
	self vIsScrollbarShowing ifTrue: [
		bottomRight _ bottomRight x - self scrollBarClass scrollbarThickness@ bottomRight y].
	self hIsScrollbarShowing ifTrue: [
		bottomRight _ bottomRight x @ 
			(bottomRight y - self scrollBarClass scrollbarThickness)].
	^topLeft corner: bottomRight! !


!HierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 8/31/2012 17:35'!
drawLinesOn: aCanvas 
	| lColor |
	lColor _ self lineColor.

	self flag: #jmvVer2. "mhhhhh y todo el manoseo este en este metodo... esta bien? o mover el dibujado a cada morph?"
	aCanvas into: scroller.

	scroller submorphs do: [ :submorph | 
		(submorph isExpanded or: [
			(aCanvas isVisible: submorph morphBoundsInWorld) or: [
				submorph nextSibling notNil and: [ aCanvas isVisible: submorph nextSibling morphBoundsInWorld ] ] ]) ifTrue: [
			aCanvas into: submorph.
			submorph
				drawLinesOn: aCanvas
				lineColor: lColor.
			aCanvas outOf: submorph ] ].
	aCanvas outOf: scroller.! !

!HierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 8/31/2012 17:30'!
drawOn: aCanvas

	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			zzframeRectangle: self focusIndicatorRectangle 
			borderWidth: Preferences focusIndicatorWidth
			color: Theme current focusIndicator ].

	"selectedMorph is subMorph of scroller... scroller is an InnerHierarchicalListMorph (as per #innerMorphClass)... This should be drawn there, imo."
	selectedMorph  ifNotNil: [
		aCanvas
			fillRectangle: (selectedMorph morphBoundsInWorld intersect: scroller morphBoundsInWorld)
			colorOrInfiniteForm: (Theme current listHighlightFocused: self hasKeyboardFocus)].

	Preferences showLinesInHierarchyViews ifTrue:[
		self drawLinesOn: aCanvas ]! !

!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 8/31/2012 17:29'!
keyboardFocusChange: aBoolean
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw focus feedback"

	self redrawNeeded! !


!PluggableListMorph methodsFor: 'drawing' stamp: 'jmv 8/31/2012 17:30'!
drawOn: aCanvas
	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			zzframeRectangle: self focusIndicatorRectangle
			borderWidth: Preferences focusIndicatorWidth
			color: Theme current focusIndicator ].! !

!PluggableListMorph methodsFor: 'event handling' stamp: 'jmv 8/31/2012 17:29'!
keyboardFocusChange: aBoolean
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw focus feedback"

	self redrawNeeded! !


!TextModelMorph methodsFor: 'drawing' stamp: 'jmv 8/31/2012 17:30'!
drawOn: aCanvas 
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"

	| bw bc |
	super drawOn: aCanvas.
	bw _ Preferences focusIndicatorWidth.
	bc _ nil.
	self wantsFrameAdornments ifTrue: [
		model refusesToAccept
			ifTrue: [  "Put up feedback showing that code cannot be submitted in this state"
				bc _ Color tan]
			ifFalse: [
				self textMorph hasEditingConflicts
					ifTrue: [
						bw _ 3.
						bc _ Color red ] 
					ifFalse: [
						self textMorph hasUnacceptedEdits
							ifTrue: [
								bc _ Color red]]]].

	(drawKeyboardFocusIndicator and: [ self textMorph hasKeyboardFocus ]) ifTrue: [
		bc ifNil: [
			bc _ Theme current focusIndicator ]]
	ifFalse: [
		bc ifNotNil: [
			bc _ bc alphaMixed: 0.4 with: Color white ]].
	bc ifNotNil: [
		aCanvas zzframeRectangle: self focusIndicatorRectangle borderWidth: bw color: bc ]! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 8/31/2012 13:11'!
assuredNonDisplayCanvas
	(canvas isNil or: [
		canvas drawsOnDisplay or: [
		(canvas extent ~= viewBox extent) or: [
		canvas form depth ~= Display depth]]])
			ifTrue: [
				"allocate a new offscreen canvas the size of the window"
				self canvas: (Display defaultCanvasClass withExtent: viewBox extent depth: Display depth)].
	^ self canvas! !

!methodRemoval: PluggableScrollPane #invalidateBorderFeedback!
PluggableScrollPane removeSelector: #invalidateBorderFeedback!
!methodRemoval: FormCanvas class #extent:depth:!
FormCanvas class removeSelector: #extent:depth:!
!methodRemoval: FormCanvas class #on:!
FormCanvas class removeSelector: #on:!
!methodRemoval: FormCanvas #reset!
FormCanvas removeSelector: #reset!
