'From Cuis 4.0 of 21 April 2012 [latest update: #1349] on 5 August 2012 at 1:12:21 am'!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
Morph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus eventListeners mouseListeners keyboardListeners mouseClickState mouseOverHandler lastMouseEvent targetOffset damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevBounds prevFullBounds '
	classVariableNames: 'DoubleClickTime '
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/4/2012 15:58'!
externalizeDistanceToWorld: aPoint
	"aPoint is a distance in own coordinates. Answer is in world coordinates."
	"Add scale factor!!"
	self flag: #jmvVer2.
	^aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/4/2012 16:12'!
morphBoundsInWorld
	"Return the bounds of this morph."
	
	"WORLD absolute bounds :("

	"remove senders and implementors"
	| answer |
	self flag: #jmvVer2.
	answer _ self morphPositionInWorld extent: self morphExtentInWorld.
	bounds = answer ifFalse: [
		#validateExtentAndBounds print.
		answer print.
		bounds print.
		thisContext printStack: 10 ].
	^answer
	! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/4/2012 16:04'!
morphExtentInWorld
	"eventually, remove"
	self flag: #jmvVer2.
	^self morphExtent! !


!Morph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 16:53'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self.
	All areas that might possibly be uncovered must be included."
	(self isOrthoRectangularMorph and: [ self isOpaqueMorph ]) ifTrue: [
		aRectangle areasOutside: self morphBoundsInWorld do: [ :r |  aCollection add: r ].
		^self ].
	aCollection add: aRectangle! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 16:53'!
drawErrorOn: aCanvas
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	w _ 10.
	r _ self morphBoundsInWorld truncated.
	aCanvas
		frameAndFillRectangle: r
		fillColor: Color red
		borderWidth: w
		borderColor: Color yellow.
	aCanvas line: r topLeft +w to: r bottomRight -w width: w color: Color yellow.
	aCanvas line: r topRight + (w negated @ w) to: r bottomLeft + (w @ w negated) width: w color: Color yellow.! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 16:19'!
drawOn: aCanvas

	aCanvas fillRectangle: self morphBoundsInWorld colorOrInfiniteForm: self color! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 16:54'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"

	self visible ifFalse: [^ self].
	(aCanvas isVisible: self fullBounds) ifFalse:[^self].		"Needs fullBounds 'in owner' if inside a scroller"
	self isKnownFailing ifTrue: [^self drawErrorOn: aCanvas].

	"Draw receiver itself"
	(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [
		aCanvas clipBy: self morphBoundsInWorld during: [ :c | self drawOn: c ]].	"Needs bounds 'in owner' if inside a scroller"
	self drawSubmorphsOn: aCanvas.
	self drawDropHighlightOn: aCanvas.
	self drawMouseDownHighlightOn: aCanvas! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 16:55'!
ownShadowForm
	"Return a form representing the 'shadow' of the receiver, without including submorphs 
	regardless of clipping"
	| canvas |
	canvas _ Display defaultCanvasClass forShadowOver: self morphBoundsInWorld.
	canvas clipBy: self morphBoundsInWorld during: [ :c | self drawOn: c ].
	^ canvas formWithOffset! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/4/2012 15:35'!
bounds
	"Return the bounds of this morph."
	
	"WORLD absolute bounds :("

	"remove senders and implementors"
	self flag: #jmvVer2.

	^ bounds
! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/4/2012 15:38'!
innerBounds
	"Return the inner rectangle enclosed by the bounds of this morph excluding the space taken by its borders. For an unbordered morph, this is just its bounds."

"would be better In own coordinates!!"
	self flag: #jmvVer2.
	^ self bounds! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/4/2012 16:55'!
worldBoundsForHalo
	"Answer the rectangle to be used as the inner dimension of my halos.
	Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle."

	^ Preferences haloEnclosesFullBounds
		ifFalse: [ self morphBoundsInWorld ]
		ifTrue: [ self fullBounds ]! !

!Morph methodsFor: 'geometry eToy' stamp: 'jmv 8/5/2012 00:44'!
referencePosition
	"Return the current reference position of the receiver"
	"a rather ugly way to say #center . At least, we avoid false polymorphism"
	"remove some day"
	self flag: #jmvVer2.
	^self morphExtentInWorld // 2 + self morphPositionInWorld! !

!Morph methodsFor: 'geometry eToy' stamp: 'jmv 8/5/2012 00:44'!
referencePosition: aPoint
	"a rather ugly way to say #center: . Just for consistency with #referencePosition"
	"remove some day"
	self flag: #jmvVer2.
	self morphPositionInWorld: aPoint - (self morphExtentInWorld // 2)! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 8/4/2012 16:53'!
containsPoint: aPoint
	| shadow |
	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ self morphBoundsInWorld containsPoint: aPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	shadow _ self ownShadowForm.
	^(shadow pixelValueAt: aPoint - shadow offset) > 0! !

!Morph methodsFor: 'layout' stamp: 'jmv 8/4/2012 15:35'!
layoutSubmorphsAndComputeFullBounds
	"Return self. Recompute the layout if necessary."

	"Check senders. Many many not be needed. Others might be just to compute fullBounds, that we hope to elliminate!! Keep those that really need layout. of submorphs"
	self flag: #jmvVer2.

	fullBounds ifNotNil: [ ^self ].

	"Errors at this point can be critical so make sure we catch 'em all right"
	[
		self layoutSubmorphs.
		fullBounds _ self computeFullBounds.
	] on: Exception do: [ :ex |
		"This should do it unless you don't screw up the bounds"
		fullBounds _ self  bounds.
		ex pass ]! !

!Morph methodsFor: 'macpal' stamp: 'jmv 8/4/2012 16:53'!
flash
	Display flash: self morphBoundsInWorld! !

!Morph methodsFor: 'updating' stamp: 'jmv 8/4/2012 16:55'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Just ours, or include submorphs if we don't clip.
	Think about it. We don't to know about a specific rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"
	
	"This method is the only real use of ivar fullBounds, other than senders of #fullBounds"

	self invalidRect: (fullBounds ifNil: [ self morphBoundsInWorld ])! !


!BorderedMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:52'!
drawOn: aCanvas

	aCanvas fillRectangle: self morphBoundsInWorld colorOrInfiniteForm: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: borderColor! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:32'!
drawOn: aCanvas
	| rectangle w x0 y0 h y1 y2 scrollbarThickness p0 e |
	p0 _ self morphPositionInWorld.
	e _ self morphExtentInWorld.
	aCanvas frameAndFillRectangle: (p0 extent: e) fillColor: self color borderWidth: borderWidth borderColor: borderColor.
	x0 _ p0 x+1.
	y0 _ p0 y+1.
	w _ e x-2.
	scrollbarThickness _ ScrollBar scrollbarThickness.
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ w - scrollbarThickness.
		aCanvas
			frameRectangle: (p0 x + e x - scrollbarThickness@p0 y
				extent: scrollbarThickness @ e y)
			borderWidth: 1
			color: borderColor.
		aCanvas
			image: (FormCanvas arrowOfDirection: #up size: scrollbarThickness)
			at: p0 x + e x - scrollbarThickness@p0 y.
		aCanvas
			image: (FormCanvas arrowOfDirection: #down size: scrollbarThickness)
			at: p0 + e - scrollbarThickness.
		h _ e y - (2 * scrollbarThickness).
		y1 _ (1.0 * self firstVisible-1 / completer entryCount * h) ceiling + y0 + scrollbarThickness-1.
		y2 _ (1.0 * self lastVisible / completer entryCount * h) floor + y0 + scrollbarThickness -1.
		aCanvas
			fillRectangle: (p0 x + e x - scrollbarThickness+2@y1 corner: p0 x + e x-2 @ y2)
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

!AutoCompleterMorph methodsFor: 'event handling' stamp: 'jmv 8/5/2012 01:02'!
mouseUp: evt
	(self containsPoint: evt eventPosition)
		ifTrue: [
			self selected: 
				((evt eventPosition y - self morphPositionInWorld y // self class itemHeight) + 
					self firstVisible).
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !


!EllipseMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:55'!
drawOn: aCanvas 

	| r |
	r _ self morphBoundsInWorld.
	aCanvas isShadowDrawing
		ifTrue: [^ aCanvas fillOval: r color:  color borderWidth: 0 borderColor: nil].
	aCanvas fillOval: r color: color borderWidth: borderWidth borderColor: borderColor.
! !

!EllipseMorph methodsFor: 'geometry testing' stamp: 'jmv 8/5/2012 00:55'!
containsPoint: aPoint

	| radius other delta xOverY e |
	(self morphBoundsInWorld containsPoint: aPoint) ifFalse: [^ false].  "quick elimination"
	self morphExtentInWorld > (1@1)
		ifFalse: [^ true].  "Degenerate case -- code below fails by a bit"

	e _ self morphExtentInWorld.
	radius _ e y asFloat / 2.
	other _ e x asFloat / 2.
	delta _ aPoint - self morphPositionInWorld - (other@radius).
	xOverY _ e x asFloat / e y asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared! !


!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 16:18'!
drawOn: aCanvas
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas
				roundRect: self morphBoundsInWorld
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ].
	aCanvas
		fillRectangle: textPane morphBoundsInWorld
		colorOrInfiniteForm: (Theme current paneBackgroundFrom: color)! !


!FrameRateMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:11'!
drawOn: aCanvas
	super drawOn: aCanvas.
	meanStepDelta ifNotNil: [ | tl |
		tl _ self morphPositionInWorld.
		aCanvas drawString: lastStepDelta rounded printString at: tl font: StrikeFont default color: Color black.
		aCanvas drawString: meanStepDelta rounded printString at: tl + (0@14) font: StrikeFont default color: Color black.
		"aCanvas drawString: lastStepStamp printString at: bounds topLeft + (0@28) font: StrikeFont default color: Color black "
		]! !


!HaloMorph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 15:18'!
drawOn: aCanvas 
	"Draw this morph only if it has no target."

	target ifNil: [^super drawOn: aCanvas]! !

!HaloMorph methodsFor: 'updating' stamp: 'jmv 8/5/2012 00:35'!
redrawNeeded
	"Quicker to invalidate handles individually if target is large (especially the world)"

	self validatePositionAndBounds.
	self validateExtentAndBounds.
	extent > (200@200)
		ifTrue: [(target notNil and: [target ~~ self world]) ifTrue: [
					"Invalidate 4 outer strips first, thus subsuming separate damage."
					(self fullBounds areasOutside: target morphPositionInWorld) do:
						[ :r | self invalidRect: r ]].
				self submorphsDo: [:m | m redrawNeeded]]
		ifFalse: [ super redrawNeeded ]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 00:35'!
doGrow: evt with: growHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
"Como podria andar el grow de un morph embebido en otro? andara ahora?"
newExtent _ evt eventPosition - positionOffset - target morphPositionInWorld.
	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target morphExtent: newExtent.
	growHandle morphPosition: evt eventPosition - (growHandle morphExtent // 2).
	self someSubmorphPositionOrExtentChanged
! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:05'!
drawOn: aCanvas 
	"Draw the hand itself (i.e., the cursor)."
	 aCanvas
		stencil: Cursor move
		at: self morphPositionInWorld
		color: Color black .
	! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 8/4/2012 16:31'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| xcess |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [ ^self ].
	aHand world addMorphFront: self.
	self morphPosition: aHand morphPosition + (-6@20).
	xcess _ self morphPositionInWorld x + self morphExtentInWorld x - aHand world morphWidth.
	xcess > 0 ifTrue: [
		self morphPosition: self morphPosition - (xcess@0) ].
	aHand balloonHelp: self! !

!HoverHelpMorph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 16:27'!
drawOn: aCanvas

	| b |
	b _ self morphBoundsInWorld.
	aCanvas roundRect: b color: self color radius: 2.
	aCanvas
		paragraph: paragraph
		bounds: (b insetBy: 4)
		color: Color black
		selectionColor: (Theme current textHighlightFocused: false)! !


!ImageMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:04'!
drawOn: aCanvas

	aCanvas image: image at: self morphPositionInWorld! !


!InnerPluggableMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2012 01:05'!
adjustExtent

	self submorphBounds ifNotNil: [ :r |
		self morphExtent: r bottomRight - self morphPositionInWorld ]! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2012 01:06'!
adjustExtent
	"And reposition submorphs"
	| w p0 h y |
	"make all items wide, so selection indicator is wide too"
	w _ self desiredWidth.
	p0 _ self morphPositionInWorld.
	y _ 0.
	self submorphsDo: [ :m |
		h _ m morphHeight.
		m bounds: (p0 + (0@y) extent: w@h).
		y _ y + h ].
	self morphExtent: w@y! !


!InnerListMorph methodsFor: 'list management' stamp: 'jmv 8/5/2012 01:08'!
drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!!"
	| topLeft |
	topLeft _ self morphPositionInWorld + (0 @ (row - 1 * font height)).
	^ topLeft extent: self morphExtentInWorld x @ font height! !

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 8/5/2012 01:06'!
rowAtLocation: aPoint
	"return the number of the row at aPoint"
	| y y0 |
	y0 _ self morphPositionInWorld y.
	y _ aPoint y.
	y < y0 ifTrue: [ ^ 1 ].
	^((y - y0 // (font height)) + 1) min: listItems size max: 0! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:08'!
draw: item atRow: row on: canvas
	"display the given item at row row"
	| drawBounds f |
	drawBounds _ self drawBoundsForRow: row.
	drawBounds _ drawBounds intersect: self morphBoundsInWorld.
	f _ (item is: #Text) ifTrue: [ font emphasized: (item emphasisAt: 1) ] ifFalse: [ font ].
	canvas drawString: item in: drawBounds font: f color: (self colorForRow: row)! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:06'!
drawBackgroundForMulti: row on: aCanvas
	| selectionDrawBounds c |
	"shade the background darker, if this row is selected"

	selectionDrawBounds _ self drawBoundsForRow: row.
	selectionDrawBounds _ selectionDrawBounds intersect: self morphBoundsInWorld.
	c _ (selectedRow notNil and: [ row = selectedRow])
		ifTrue: [ Theme current listHighlightFocused: owner hasKeyboardFocus ]
		ifFalse: [ Theme current listMultiHighlightFocused: owner hasKeyboardFocus ].
	aCanvas fillRectangle: selectionDrawBounds colorOrInfiniteForm: c! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:06'!
drawSelectionOn: aCanvas
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: selectedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: self morphBoundsInWorld.
	aCanvas
		fillRectangle: selectionDrawBounds
		colorOrInfiniteForm: (Theme current listHighlightFocused: owner hasKeyboardFocus)! !


!InnerTextMorph methodsFor: 'anchors' stamp: 'jmv 8/5/2012 01:09'!
anchorMorph: aMorph at: aPoint
	| relPt |
	aMorph owner == self ifTrue: [ self removeMorph: aMorph ].
	self addMorphFront: aMorph.
	relPt _ aPoint - self morphPositionInWorld.
	editor insertMorph: aMorph at: relPt.
	self fit.! !

!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:10'!
debugDrawLineRectsOn: aCanvas
	"Shows where text line rectangles are"
	| tl |
	tl _ self morphPositionInWorld.
	self paragraph lines do: [ :line |
		aCanvas frameRectangle: (line rectangle translateBy: tl) borderWidth: 1 color: Color brown ]
! !

!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 16:32'!
drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		paragraph: self paragraph
		bounds: self morphBoundsInWorld
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus)! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 8/5/2012 01:10'!
clickAndHalf: evt
	self handleInteraction: [
		editor clickAndHalf: (evt translatedBy: self morphPositionInWorld negated) ].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 8/5/2012 01:08'!
doubleClickAndHalf: evt
	self handleInteraction: [
		editor doubleClickAndHalf: (evt translatedBy: self morphPositionInWorld negated) ].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 8/5/2012 01:10'!
mouseDown: event
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	event mouseButton2Pressed ifTrue: [^ self mouseButton2Activity].

	"If we don't focus, Get focus, and do nothing else (the user will need to click again to do further interaction)"
	self hasKeyboardFocus ifFalse: [
		^event hand newKeyboardFocus: self].

	super mouseDown: event.

	self handleInteraction: [editor mouseDown: (event translatedBy: self morphPositionInWorld negated)].

	event hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: event
		clkSel: nil
		clkNHalf: #clickAndHalf:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:
		tripleClkSel: nil! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 8/4/2012 16:37'!
mouseMove: evt
	evt mouseButton1Pressed ifFalse: [^ self enterClickableRegion: evt].
	self handleInteraction: [ editor mouseMove: (evt translatedBy: self morphPositionInWorld negated)].
	(evt eventPosition y - owner morphPositionInWorld y between: 0 and: owner morphExtentInWorld y) ifFalse: [
		owner scrollSelectionIntoView ]! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 8/5/2012 01:10'!
mouseUp: evt
	super mouseUp: evt.
	self pauseBlinking.
	self handleInteraction: [editor mouseUp: (evt translatedBy: self morphPositionInWorld negated)].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 01:09'!
extentForComposing
	self flag: #jmvVer2.	"like #extent ..."
	^wrapFlag
		ifTrue: [ self morphExtentInWorld x @ 9999999 ]
		ifFalse: [ 9999999@9999999 ]! !

!InnerTextMorph methodsFor: 'selection' stamp: 'jmv 8/5/2012 01:08'!
selectionRects
	"Paragraph assumes its topLeft is 0@0. We don't"
	| tl |
	tl _ self morphPositionInWorld.
	^self paragraph selectionRects collect: [ :r | r translateBy: tl ]! !


!LayoutAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 8/4/2012 16:39'!
handPoint

	^ hand morphPositionInWorld adhereTo: owner morphBoundsInWorld! !

!LayoutAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 8/5/2012 01:04'!
initialIndicatorBounds
	^self morphBoundsInWorld outsetBy: 1! !

!LayoutAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:04'!
drawOn: aCanvas
	aCanvas fillRectangle: self morphBoundsInWorld colorOrInfiniteForm: color borderWidth: 2 borderStyleSymbol: #raised! !


!MagnifierMorph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 16:40'!
hasTranslucentColor
	"I may show what's behind me, so tell the hand to don't cache"
	^self sourceRect intersects: self morphBoundsInWorld! !

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 8/4/2012 16:44'!
magnifiedForm
	| srcRect form neededExtent |
	lastPos _ self sourcePoint.
	srcRect _ self sourceRectFrom: lastPos.
	((srcRect intersects: self morphBoundsInWorld) and: [ RecursionLock == nil ])
		ifTrue: [
			RecursionLock _ self.
			"try to reuse form if appropriate"
			auxCanvas _ (auxCanvas notNil and: [ auxCanvas extent = srcExtent ])
				ifTrue: [
					"Just in case we go out of the Display"
					srcRect origin > (0@0) ifFalse: [
						auxCanvas form fillBlack ].
					FormCanvas on: auxCanvas form over: srcRect ]
				ifFalse: [ FormCanvas depth: 32 over: srcRect ].
			World drawOn: auxCanvas.
			World drawSubmorphsOn: auxCanvas.
			form _ auxCanvas form.
			RecursionLock _ nil]
		ifFalse: [
			"cheaper method if the source is not occluded"
			form _ Display copy: srcRect].
	"smooth if non-integer scale"
	neededExtent _ (srcExtent * magnification ) truncated.
	(magnifiedForm isNil or: [ magnifiedForm extent ~=  neededExtent ])
		ifTrue: [ magnifiedForm _ Form extent: neededExtent depth: 32 ].
	(WarpBlt current toForm: magnifiedForm)
		sourceForm: form;
		colorMap: (form colormapIfNeededFor: magnifiedForm);
		cellSize: (magnification isInteger ifTrue: [1] ifFalse: [2]);  "installs a new colormap if cellSize > 1"
		combinationRule: 3;
		copyQuad: form boundingBox innerCorners toRect: magnifiedForm boundingBox.
	^magnifiedForm.! !

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 8/4/2012 16:41'!
sourceRect
	"world global coordinates, etc"
	self flag: #jmvVer2.
	^self sourceRectFrom: self sourcePoint! !


!MenuLineMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:45'!
drawOn: aCanvas 
	| baseColor r |
	baseColor := owner color.
	r _ self morphBoundsInWorld.
	aCanvas
		fillRectangle: (r topLeft corner: r rightCenter)
		colorOrInfiniteForm: baseColor twiceDarker.
			
	aCanvas
		fillRectangle: (r leftCenter corner: r bottomRight)
		colorOrInfiniteForm: baseColor twiceLighter! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 8/4/2012 16:51'!
popUpAdjacentTo: rightOrLeftPoint forHand: hand from: sourceItem
	"Present this menu at the given point under control of the given hand."

	| delta tryToPlace selectedOffset |
	hand world startSteppingSubmorphsOf: self.
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition - self morphPosition.
	sourceItem owner owner addMorphFront: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self fullBounds amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPoint first value: false;
		value: rightOrLeftPoint last - (self morphWidth @ 0) value: false;
		value: rightOrLeftPoint first value: true! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 8/4/2012 16:50'!
adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p tl |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w _ submorphs inject: 0 into: [ :prev :each |
		prev max: (
			(each respondsTo: #minItemWidth)
				ifTrue: [each minItemWidth]
				ifFalse: [each morphWidth])].

	w _ w + 4.
	tl _ self morphPositionInWorld.
	p _ tl + 5.
	submorphs do: [ :m |
		m morphWidth: w.
		m morphPosition: p.
		p _ m morphPositionInWorld + (0@(m morphExtentInWorld y + 1)) ].
	
	self morphExtent: submorphs last morphBoundsInWorld bottomRight - tl + 5! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 8/4/2012 16:52'!
positionAt: aPoint relativeTo: aMenuItem
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| i yOffset sub delta |
	self adjustSubmorphsLayout.
	i _ 0.
	yOffset _ 0.
	[(sub _ self submorphs at: (i _ i + 1)) == aMenuItem]
		whileFalse: [ yOffset _ yOffset + sub morphHeight ].

	self morphPosition: aPoint - (2 @ (yOffset + 8)).

	"If it doesn't fit, show it to the left, not to the right of the hand."
	self morphBoundsInWorld right > owner world morphBoundsInWorld right
		ifTrue: [
			self moveRight: aPoint x + 1].

	"Make sure that the menu fits in the world."
	delta _ self morphBoundsInWorld amountToTranslateWithin:
		(owner world morphBoundsInWorld withHeight: ((owner world morphExtentInWorld y - 18) max: (ActiveHand morphPosition y) + 1)).
	delta = (0 @ 0) ifFalse: [ self morphPosition: self morphPosition + delta ]! !

!MenuMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:58'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: self morphBoundsInWorld color: color radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas fillRectangle: self morphBoundsInWorld colorOrInfiniteForm: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]! !


!MinimalStringMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:51'!
drawOn: aCanvas

	aCanvas drawString: contents in: self morphBoundsInWorld font: self fontToUse color: color! !


!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:04'!
drawOn: aCanvas
	self hasSelection ifTrue: [
		self drawSelectionOn: aCanvas ].
	self hasVisibleCaret ifTrue: [
		self drawCaretOn: aCanvas].
	aCanvas drawString: contents in: self morphBoundsInWorld font: self fontToUse color: color! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:03'!
drawSelectionOn: aCanvas
	| rightX leftX top bottom tl |

	tl _ self morphPositionInWorld.
	top _ tl y.
	bottom _ top + self baseFont height.
	leftX _ (self fontToUse widthOfString: contents from: 1 to: editor startIndex-1) + tl x.
	rightX _ (self fontToUse widthOfString: contents from: 1 to: editor stopIndex-1) + tl x.

	aCanvas
		fillRectangle: (leftX @ top corner: rightX @ bottom)
		colorOrInfiniteForm: (Theme current textHighlightFocused: self hasKeyboardFocus)! !


!Paragraph methodsFor: 'editing' stamp: 'jmv 8/4/2012 16:56'!
clickAt: clickPoint
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	| startBlock action target range boxes box t |
	action _ false.
	startBlock _ self characterBlockAtPoint: clickPoint.
	t _ model actualContents.
	(t attributesAt: startBlock stringIndex) do: [ :att | 
		att mayActOnClick ifTrue:
				[(target _ model) ifNil: [ target _ editor morph].
				range _ t rangeOf: att startingAt: startBlock stringIndex.
				boxes _ self selectionRectsFrom: (self characterBlockForIndex: range first) 
							to: (self characterBlockForIndex: range last+1).
				box _ boxes detect: [:each | each containsPoint: clickPoint] ifNone: nil.
				box ifNotNil: [
					box _ editor morph morphBoundsInWorld.
					editor morph allOwnersDo: [ :m | box _ box intersect: (m morphBoundsInWorld) ].
					Utilities
						awaitMouseUpIn: box
						repeating: nil
						ifSucceed: [(att actOnClickFor: target in: self at: clickPoint editor: editor) ifTrue: [action _ true]].
					Cursor currentCursor == Cursor webLink ifTrue:[Cursor normal show].
				]]].
	^ action! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:01'!
drawInconOn: aCanvas

	| theIcon |
	theIcon _ self magnifiedIcon.
	aCanvas
		image: theIcon
		multipliedBy: self iconColor
		at: self morphBoundsInWorld center - (theIcon extent //2)! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:00'!
drawRoundGradientLookOn: aCanvas
	| r colorForButton rect bottomFactor topFactor |

	rect _ self morphBoundsInWorld insetBy: 1@3.
	self isPressed
		ifFalse: [
			topFactor _ Theme current buttonGradientTopFactor.
			bottomFactor _ Theme current buttonGradientBottomFactor.
			self mouseIsOver
				ifTrue: [	
					colorForButton _ Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [
					colorForButton _ color ]]
		ifTrue: [
			topFactor _ Theme current buttonGradientBottomFactor.
			bottomFactor _ Theme current buttonGradientTopFactor.
			colorForButton _ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		Theme current useButtonGradient
			ifTrue: [
				aCanvas
					roundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				aCanvas roundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ]! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/4/2012 17:01'!
focusIndicatorRectangle

	| topLeft bottomRight |
	topLeft _ self morphPositionInWorld.
	bottomRight _ topLeft + self morphExtentInWorld.
	topLeft _ topLeft + borderWidth.
	bottomRight _ bottomRight - borderWidth.
	self vIsScrollbarShowing ifTrue: [
		bottomRight _ scrollBar morphPositionInWorld x -1@ bottomRight y].
	self hIsScrollbarShowing ifTrue: [
		bottomRight _ bottomRight x @ 
			(bottomRight y - self scrollBarClass scrollbarThickness)].
	^topLeft corner: bottomRight! !


!HierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 16:20'!
drawLinesOn: aCanvas 
	| lColor |
	lColor _ self lineColor.
	scroller submorphs do: [ :submorph | 
		(submorph isExpanded or: [
			(aCanvas isVisible: submorph morphBoundsInWorld) or: [
				submorph nextSibling notNil and: [ aCanvas isVisible: submorph nextSibling morphBoundsInWorld ] ] ]) ifTrue: [
			submorph
				drawLinesOn: aCanvas
				lineColor: lColor ] ]! !

!HierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 8/4/2012 16:20'!
drawOn: aCanvas

	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle 
			borderWidth: Preferences focusIndicatorWidth
			color: Theme current focusIndicator ].

	selectedMorph  ifNotNil: [
		aCanvas
			fillRectangle: (selectedMorph morphBoundsInWorld intersect: scroller morphBoundsInWorld)
			colorOrInfiniteForm: (Theme current listHighlightFocused: self hasKeyboardFocus)].

	Preferences showLinesInHierarchyViews ifTrue:[
		self drawLinesOn: aCanvas ]! !

!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 8/4/2012 16:25'!
itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	| ptY last |
	scroller hasSubmorphs ifFalse: [ ^nil ].
	(scroller fullBounds containsPoint: aPoint) ifFalse: [ ^nil ].
	ptY _ aPoint y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	scroller firstSubmorph morphPositionInWorld y > ptY ifTrue: [ ^nil ].
	last _ scroller lastSubmorph.
	last morphPositionInWorld y + last morphExtentInWorld y < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^scroller 
		findSubmorphBinary: [ :m |
			(m morphPositionInWorld y <= ptY and: [ m morphPositionInWorld y + m morphExtentInWorld y >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPositionInWorld y + (m morphExtentInWorld y // 2) > ptY ifTrue: [-1] ifFalse: [1]]]! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 8/4/2012 16:26'!
scrollSelectionIntoView
	selectedMorph ifNotNil: [
		self scrollToShow: selectedMorph morphBoundsInWorld ]! !


!PolygonMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:58'!
drawClippedBorderOn: aCanvas usingEnds: anArray 
	aCanvas clipBy: self morphBoundsInWorld during:[:cc| self drawBorderOn: cc usingEnds: anArray].! !

!PolygonMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:58'!
drawOn: aCanvas 
	"Display the receiver, a spline curve, approximated by straight line segments."
	| lineColor bigClipRect brush p1i p2i |
	vertices size < 1 ifTrue: [self error: 'a polygon must have at least one point'].
	closed & color isTransparent not ifTrue: [
		self filledForm colors: (Array with: Color transparent with: color).
		aCanvas image: self filledForm at: self morphPositionInWorld -1 ].
	lineColor _ borderColor. 
	bigClipRect _ aCanvas clipRect expandBy: borderWidth+1//2.
	brush _ nil.
	self lineSegmentsDo: [ :p1 :p2 |
		p1i _ p1 asIntegerPoint.  p2i _ p2 asIntegerPoint.
		(closed or: ["bigClipRect intersects: (p1i rect: p2i) optimized:"
					((p1i min: p2i) max: bigClipRect origin) <=
					((p1i max: p2i) min: bigClipRect corner)]) ifTrue: [
			borderWidth > 3
			ifTrue: [brush ifNil: [
						brush _ (ColorForm dotOfSize: borderWidth)
								colors: (Array with: Color transparent with: borderColor)].
					aCanvas line: p1i to: p2i brushForm: brush]
			ifFalse: [aCanvas line: p1i to: p2i
							width: borderWidth color: lineColor]]].
	self arrowForms ifNotNil: [
		self arrowForms do: [ :f |
			f colors: (Array with: Color transparent with: borderColor).
			aCanvas image: f at: f offset]]! !

!PolygonMorph methodsFor: 'geometry testing' stamp: 'jmv 8/5/2012 00:57'!
containsPoint: aPoint

	(self morphBoundsInWorld containsPoint: aPoint) ifFalse: [^ false].  "quick elimination"

	closed & color isTransparent not ifTrue:
		[^ (self filledForm pixelValueAt: aPoint - self morphPositionInWorld + 1) > 0].

	self lineSegmentsDo:
		[:p1 :p2 |
		(aPoint onLineFrom: p1 to: p2 within: (3 max: borderWidth+1//2) asFloat)
				ifTrue: [^ true]].

	self arrowForms do:
		[:f | (f pixelValueAt: aPoint - f offset) > 0 ifTrue: [^ true]].

	^ false! !

!PolygonMorph methodsFor: 'halo control' stamp: 'jmv 8/5/2012 00:57'!
rotationDegrees: degrees 
	| center |
	center _ self morphBoundsInWorld center.
	self setVertices: (vertices collect: [ :v |
		v inverseRotateBy: (degrees - self forwardDirection) degreesToRadians negated about: center ]).
	self forwardDirection: degrees! !

!PolygonMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 00:57'!
borderForm
	"A form must be created for drawing the border whenever the borderColor is translucent."

	| borderCanvas |
	borderForm ifNotNil: [^ borderForm].
	borderCanvas _ Display defaultCanvasClass forShadowOver: self morphBoundsInWorld.
	self drawBorderOn: borderCanvas.
	borderForm _ borderCanvas form.
	self arrowForms do:
		[:f |  "Eliminate overlap between line and arrowheads if transparent."
		borderForm copy: f boundingBox from: f to: f offset - self morphPosition rule: Form erase].
	^ borderForm! !

!PolygonMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 00:58'!
filledForm
	"Note: The filled form is actually 2 pixels bigger than bounds, and the point corresponding to this morphs' position is at 1@1 in the form.  This is due to the details of the fillig routines, at least one of which requires an extra 1-pixel margin around the outside.  Computation of the filled form is done only on demand."
	| bb origin |
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	closed ifFalse: [^ filledForm _ nil].
	filledForm ifNotNil: [^ filledForm].
	filledForm _ ColorForm extent: extent+2.

	"Draw the border..."
	bb _ (BitBlt current toForm: filledForm) sourceForm: nil; fillColor: Color black;
			combinationRule: Form over; width: 1; height: 1.
	origin _ self morphPositionInWorld asIntegerPoint-1.
	self lineSegmentsDo: [:p1 :p2 | bb drawFrom: p1 asIntegerPoint-origin
										to: p2 asIntegerPoint-origin].

	"Fill it in..."
	filledForm _ ColorForm mappingWhiteToTransparentFrom: filledForm anyShapeFill.

	borderColor mightBeTranslucent ifTrue: [
		"If border is stored as a form, then erase any overlap now."
		filledForm
			copy: self borderForm boundingBox
			from: self borderForm
			to: 1@1
			rule: Form erase ].

	^ filledForm! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 8/4/2012 17:02'!
initialFrameFor: aView initialExtent: initialExtent world: aWorld 
	"Find a plausible initial screen area for the supplied view, which should be a StandardSystemView, taking into account the 'reverseWindowStagger' Preference, the size needed, and other windows currently on the screen."

	| allOrigins screenRight screenBottom putativeOrigin putativeFrame allowedArea staggerOrigin otherFrames |
	Preferences reverseWindowStagger 
		ifTrue: 
			[^self 
				strictlyStaggeredInitialFrameFor: aView
				initialExtent: initialExtent
				world: aWorld].
	allowedArea := self maximumUsableAreaInWorld: aWorld.
	screenRight := allowedArea right.
	screenBottom := allowedArea bottom.
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [:w | w isCollapsed not]) 
						collect: [:w | w morphBoundsInWorld].
	allOrigins := otherFrames collect: [:f | f origin].
	(self standardPositionsInWorld: aWorld) do: 
			[:aPosition | 
			"First see if one of the standard positions is free"

			(allOrigins includes: aPosition) 
				ifFalse: 
					[^(aPosition extent: initialExtent) 
						translatedAndSquishedToBeWithin: allowedArea]].
	staggerOrigin := (self standardPositionsInWorld: aWorld) first.	"Fallback: try offsetting from top left"
	putativeOrigin := staggerOrigin.
	
	[putativeOrigin := putativeOrigin + StaggerOffset.
	putativeFrame := putativeOrigin extent: initialExtent.
	putativeFrame bottom < screenBottom 
		and: [putativeFrame right < screenRight]] 
			whileTrue: 
				[(allOrigins includes: putativeOrigin) 
					ifFalse: 
						[^(putativeOrigin extent: initialExtent) 
							translatedAndSquishedToBeWithin: allowedArea]].
	^(self scrollBarSetback @ self screenTopSetback extent: initialExtent) 
		translatedAndSquishedToBeWithin: allowedArea! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 8/4/2012 17:02'!
strictlyStaggeredInitialFrameFor: aStandardSystemView initialExtent: initialExtent world: aWorld 
	"This method implements a staggered window placement policy that I (di) like.
	Basically it provides for up to 4 windows, staggered from each of the 4 corners.
	The windows are staggered so that there will always be a corner visible."

	| allowedArea grid initialFrame otherFrames cornerSel corner delta putativeCorner free maxLevel |
	allowedArea := (self maximumUsableAreaInWorld: aWorld) 
				insetBy: (self scrollBarSetback @ self screenTopSetback extent: 0 @ 0).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel := allowedArea area > 300000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid := allowedArea area > 500000 ifTrue: [40] ifFalse: [20].
	initialFrame := 0 @ 0 extent: initialExtent.
	"min: (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))))
							min: 600@400"
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [:w | w isCollapsed not]) 
						collect: [:w | w morphBoundsInWorld].
	0 to: maxLevel
		do: 
			[:level | 
			1 to: 4
				do: 
					[:ci | 
					cornerSel := #(#topLeft #topRight #bottomRight #bottomLeft) at: ci.
					corner := allowedArea perform: cornerSel.
					"The extra grid//2 in delta helps to keep title tabs distinct"
					delta := ((maxLevel - level) * grid + (grid // 2)) @ (level * grid).
					1 to: ci - 1 do: [ :i | delta _ delta y negated @ delta x ].	"slow way"
					putativeCorner := corner + delta.
					free := true.
					otherFrames 
						do: [:w | free := free & ((w perform: cornerSel) ~= putativeCorner)].
					free 
						ifTrue: 
							[^(initialFrame align: (initialFrame perform: cornerSel)
								with: putativeCorner) translatedAndSquishedToBeWithin: allowedArea]]].
	"If all else fails..."
	^(self scrollBarSetback @ self screenTopSetback 
		extent: initialFrame extent) translatedAndSquishedToBeWithin: allowedArea! !


!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 8/5/2012 01:01'!
scrollAbsolute: aPoint
	| relativePoint v |
	relativePoint _ aPoint - self morphPositionInWorld.
	v _ (self isHorizontal
		ifTrue: [ relativePoint x ]
		ifFalse: [ relativePoint y ])
			- borderWidth - self buttonExtent * 1.0
				/ self freeSliderRoom.
	self setValue: v! !

!ScrollBar methodsFor: 'drawing' stamp: 'jmv 8/5/2012 01:02'!
drawOn: aCanvas

	aCanvas
		fillRectangle: self morphBoundsInWorld
		colorOrInfiniteForm: (color alphaMixed: 0.3 with: Color white)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!SketchMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:50'!
drawOn: aCanvas
	aCanvas image: originalForm at: self morphPositionInWorld! !

!SketchMorph methodsFor: 'geometry testing' stamp: 'jmv 8/5/2012 00:50'!
containsPoint: aPoint

	^ (self morphBoundsInWorld containsPoint: aPoint) and: [
		(originalForm isTransparentAt: aPoint - self morphPositionInWorld ) not ]
! !


!ColorPickerMorph methodsFor: 'accessing' stamp: 'jmv 8/5/2012 00:51'!
originalColor: colorOrSymbol 
	"Set the receiver's original color.  It is at this point that a command is launched to represent the action of the picker, in support of Undo."

	originalColor := (colorOrSymbol is: #Color) 
				ifTrue: [colorOrSymbol]
				ifFalse: [Color lightGreen].
	originalForm fill: RevertBox fillColor: originalColor.
	selectedColor := originalColor.
	self locationIndicator 
		referencePosition: self morphPositionInWorld + (self positionOfColor: originalColor)! !

!ColorPickerMorph methodsFor: 'other' stamp: 'jmv 8/5/2012 00:29'!
addToWorld: world near: box
	| goodLocation |
	goodLocation _ self bestPositionNear: box inWorld: world.
	world allMorphsDo:
		[:p | (p isMemberOf: ColorPickerMorph) ifTrue:
		[(p ~~ self and: [p owner notNil and: [p target == target]]) ifTrue:
			[(p selector == selector and: [p argument == argument])
				ifTrue: [^ p comeToFront  "uncover existing picker"]
				ifFalse: ["place second picker relative to first"
						goodLocation _ self bestPositionNear: p morphBoundsInWorld inWorld: world]]]].
	world addMorphFront: self.
	self morphPositionInOwner: goodLocation.
	self resizeMorph! !


!StringMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:45'!
drawOn: aCanvas

	aCanvas drawString: contents in: self morphBoundsInWorld font: self fontToUse color: color.! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:47'!
drawLinesToFirstChildOn: aCanvas lineColor: lineColor 
	"Draw line from me to next sibling"

	| vLineX vLineTop vLineBottom childBounds childCenter |
	childBounds := self firstChild toggleBounds.
	childCenter := childBounds center.
	vLineX := childCenter x - 1.
	vLineTop := self morphPositionInWorld y + self morphExtentInWorld y.
	self firstChild hasToggle
		ifTrue: [vLineBottom := childCenter y - 7]
		ifFalse: [vLineBottom := childCenter y].
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @ vLineBottom
		width: 1
		color: lineColor! !

!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 00:46'!
drawOn: aCanvas

	| tRect sRect columnRect columnScanner columnData columnLeft colorToUse |

	tRect := self toggleRectangle.
	sRect := self morphBoundsInWorld withLeft: tRect right + 4.
	self drawToggleOn: aCanvas in: tRect.
	colorToUse _ complexContents preferredColor ifNil: [color].
	(container columns isNil or: [(contents asString indexOf: Character tab) = 0]) ifTrue: [
		aCanvas drawString: contents asString in: sRect font: self fontToUse color: colorToUse.
	] ifFalse: [
		columnLeft _ sRect left.
		columnScanner _ ReadStream on: contents asString.
		container columns do: [ :width |
			columnRect _ columnLeft @ sRect top extent: width @ sRect height.
			columnData _ columnScanner upTo: Character tab.
			columnData isEmpty ifFalse: [
				aCanvas drawString: columnData in: columnRect font: self fontToUse color: colorToUse.
			].
			columnLeft _ columnRect right + 5.
		].
	]
! !

!IndentingListItemMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 00:48'!
toggleRectangle

	^self morphPositionInWorld + (12*indentLevel @ 0) extent: 12@self morphExtentInWorld y! !


!Taskbar methodsFor: 'stepping' stamp: 'jmv 8/4/2012 17:02'!
step

	"My dimensions are constrained live."
	| r |
	r _ World morphBoundsInWorld.
	r _ r left @ (r bottom -18) extent: r width@18.
	self morphBoundsInWorld = r ifFalse: [
		self bounds: r]! !


!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 8/4/2012 17:03'!
testHorizontalAlignment
	
	self should: [ taskbar morphPositionInWorld x = World morphPositionInWorld x ]! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 8/4/2012 17:04'!
testVerticalAlignment
	
	self should: [ taskbar morphBoundsInWorld bottom = World morphBoundsInWorld bottom ]! !


!TextEditor methodsFor: 'as yet unclassified' stamp: 'jmv 8/4/2012 17:04'!
visibleHeight

	^morph owner morphExtentInWorld y! !


!TextModelMorph methodsFor: 'editor access' stamp: 'jmv 8/4/2012 17:04'!
scrollSelectionIntoView
	"Scroll my text into view if necessary and return true, else return false"

	| rectToTest |
	rectToTest _ self editor pointBlock translateBy: self textMorph morphPositionInWorld.
	self scrollToShow: rectToTest! !


!WindowEdgeAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 8/4/2012 17:05'!
initialIndicatorBounds
	^owner morphBoundsInWorld! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 8/4/2012 17:05'!
drawHand: aHandMorph

	| bw r |
	Preferences fastDragWindowForMorphic ifTrue: [
		bw _ HandMorph fastDragBorderWidth // 2.
		r _ aHandMorph fullBounds.
		canvas frameRectangle: r borderWidth: bw color: Color black.
		canvas frameRectangle: (r insetBy: bw) borderWidth: bw color: Color white.
		canvas clipBy: aHandMorph morphBoundsInWorld during: [ :c | aHandMorph drawOn: c ]]
	ifFalse: [
		aHandMorph fullDrawOn: canvas ]! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 8/4/2012 17:05'!
drawInvalidAreasWorld: aWorld submorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."

	| initialRectsToRepair currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects reuse i n morph morphBounds morphClipRect |
	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair _ OrderedCollection new.
	(damageRecorder invalidRectsFullBounds: aWorld viewBox) do: [ :r |
		r ifNotNil: [ initialRectsToRepair addLast: r ]].
	damageRecorder reset.
	currentRectsToRepair _ OrderedCollection new.
	newRectsToRepair _ OrderedCollection withAll: initialRectsToRepair.
	morphsToDraw _ OrderedCollection new.
	rectsForEachMorph _ OrderedCollection new.
	thisMorphRects _ OrderedCollection new.
	n _ submorphs size.
	i _ 1.
	[ i <= n and: [ newRectsToRepair notEmpty ]] whileTrue: [
		morph _ submorphs at: i.
		morph visible ifTrue: [
			morphBounds _morph fullBounds.
			reuse _ currentRectsToRepair.
			currentRectsToRepair _ newRectsToRepair.
			newRectsToRepair _ reuse removeAll.
			currentRectsToRepair do: [ :r |
				(morphBounds intersects: r)
					ifTrue: [
						morphClipRect _ morphBounds intersect: r.
						thisMorphRects add: morphClipRect. "We could perhaps try and join adjacent rectangles in this collection..."
						morph addPossiblyUncoveredAreasIn: r to: newRectsToRepair ]
					ifFalse: [
						newRectsToRepair add: r ]].
			thisMorphRects ifNotEmpty: [
				morphsToDraw add: morph.
				rectsForEachMorph add: thisMorphRects.
				thisMorphRects _ OrderedCollection new.
			]].
		i _ i + 1 ].
	i > n  ifTrue: [
		newRectsToRepair do: [ :r |
			(canvas copyClipRect: r) clipBy: aWorld morphBoundsInWorld during: [ :c | aWorld drawOn: c ]]].
	morphsToDraw with: rectsForEachMorph reverseDo: [ :m :xrects |
		"Here we could merge all xrects into just one call... Most likely, that would be slower, though."
"		rr _ nil."
		xrects do: [ :r |
"			rr _ rr ifNil: [ r ] ifNotNil: [ r quickMerge: rr ]."
			m fullDrawOn: (canvas copyClipRect: r)
		].
"		(canvas copyClipRect: rr) fullDrawMorph: m "
	].
	
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair! !

!methodRemoval: Preferences class #showBoundsInHalo!
Preferences class removeSelector: #showBoundsInHalo!
!methodRemoval: HandMorph class #doubleClickTime!
HandMorph class removeSelector: #doubleClickTime!
!methodRemoval: HandMorph class #doubleClickTime:!
HandMorph class removeSelector: #doubleClickTime:!
!methodRemoval: HandMorph class #initialize!
HandMorph class removeSelector: #initialize!
HandMorph initialize!
!methodRemoval: HandMorph #cursorBounds!
HandMorph removeSelector: #cursorBounds!
!methodRemoval: HandMorph #savePatchFrom:!
HandMorph removeSelector: #savePatchFrom:!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
Morph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus eventListeners mouseListeners keyboardListeners mouseClickState mouseOverHandler lastMouseEvent targetOffset damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevBounds prevFullBounds'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
