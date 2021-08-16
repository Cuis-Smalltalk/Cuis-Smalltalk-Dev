'From Cuis 5.0 [latest update: #4752] on 13 August 2021 at 12:13:06 pm'!
!classDefinition: #BitBltBoundsFinderCanvas category: #'Morphic-Support'!
MorphicCanvas subclass: #BitBltBoundsFinderCanvas
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #BitBltCanvas category: #'Morphic-Support'!
MorphicCanvas subclass: #BitBltCanvas
	instanceVariableNames: 'boundsFinderCanvas '
	classVariableNames: 'AccessProtect AuxBlitter AuxForm CachedForms '
	poolDictionaries: ''
	category: 'Morphic-Support'!

!KernelMorph methodsFor: 'testing' stamp: 'jmv 8/12/2021 15:43:53'!
is: aSymbol
	^ aSymbol == #KernelMorph or: [ super is: aSymbol ]! !


!MorphicCanvas methodsFor: 'testing' stamp: 'jmv 8/12/2021 14:47:25'!
isBoundsFinderCanvas
	^false! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:46:21'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	BitBltCanvas doesn't keep track of Morph bounds as it draws them. But it is limited to drawing morphs where
	#morphLocalBounds is known. Use it, then."

	| boundingRect |
	boundingRect _ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:47:52'!
displayBoundsForHand: aHand

	^ aHand morphPosition-8 extent: aHand morphExtent.! !

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:03:41'!
fullAddCurrentRect: addCurrentBounds submorphs: trySubmorphs to: aDamageRecorder

	| invalidateOwns |
	invalidateOwns _ false.
	addCurrentBounds ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		invalidateOwns _ true ].
	trySubmorphs ifTrue: [
		(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphsReally not])
			ifTrue: [
				currentMorph submorphNeedsRedraw: false.
				currentMorph submorphsReverseDo: [ :m |
					self fullAddRedrawRect: m to: aDamageRecorder ]]
			ifFalse: [
				currentMorph fullRedrawNotNeeded.
				invalidateOwns _ true ]].
	invalidateOwns ifTrue: [
		aDamageRecorder recordInvalidRect: currentMorph displayBounds for: currentMorph ]! !

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:07:13'!
fullAddRedrawRect: aMorph to: aDamageRecorder
	| addCurrentBounds trySubmorphs |
	aMorph visible ifTrue: [
		addCurrentBounds _ aMorph isRedrawNeeded.
		trySubmorphs _ aMorph isSubmorphRedrawNeeded.
		(trySubmorphs or: [addCurrentBounds]) ifTrue: [
			self into: aMorph.
			self
				fullAddCurrentRect: addCurrentBounds
				submorphs: trySubmorphs
				to: aDamageRecorder.
			self outOfMorph
		]]! !

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 14:38:59'!
fullUpdateCurrentProtrudingBounds
	"Recursively update display bounds, for currentMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."

	currentMorph displayBoundsSetFrom: self.! !

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:10:54'!
fullUpdateProtrudingBounds: aMorph
	"Recursively update display bounds, for aMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."
	
	aMorph visible ifTrue: [
		self into: aMorph.
		self fullUpdateCurrentProtrudingBounds.
		self outOfMorph ].! !

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:15:58'!
updateBoundsIn: aWorldMorph addDamageTo: aDamageRecorder

	aWorldMorph submorphsDo: [ :morph |
		self fullAddRedrawRect: morph to: aDamageRecorder ].
	self updateHandsDisplayBounds: aWorldMorph.! !

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:16:04'!
updateHandsDisplayBounds: aWorldMorph
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aWorldMorph handsDo: [ :hand |
		hand hasSubmorphs ifTrue: [
			self into: hand.
			hand submorphsDo: [ :morph |
				morph displayBounds ifNil: [
					self fullUpdateProtrudingBounds: morph ]].
			self outOfMorph ]].! !

!BitBltBoundsFinderCanvas methodsFor: 'testing' stamp: 'jmv 8/12/2021 14:47:42'!
isBoundsFinderCanvas
	^true! !


!BitBltCanvas methodsFor: 'accessing' stamp: 'jmv 8/12/2021 14:25:05'!
boundsFinderCanvas
	^boundsFinderCanvas! !

!BitBltCanvas methodsFor: 'initialization' stamp: 'jmv 8/12/2021 14:26:28'!
initializeWithOrigin: aPoint

	super initializeWithOrigin: aPoint.
	boundsFinderCanvas _ BitBltBoundsFinderCanvas new initializeWithOrigin: aPoint.! !

!BitBltCanvas methodsFor: 'initialization' stamp: 'jmv 8/12/2021 15:26:38'!
world: aWorldMorph
	super world: aWorldMorph.
	boundsFinderCanvas world: aWorldMorph! !


!Morph methodsFor: 'geometry' stamp: 'jmv 8/13/2021 11:58:42'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us.
	See also #displayBoundsUpdateFrom:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			(aCanvas isBoundsFinderCanvas or: [c canvasToUse == aCanvas]) ifTrue: [
				self knowsOwnLocalBounds
					ifTrue: [
						"A good optimization in many cases.
						But for morphs where #drawOn: might not be able to find bounds (InnerTextMorph and LayoutMorph,
						for example) this is essential."
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphFromLocalBounds]
					ifFalse: [
						"Use the bounds bound by #drawOn. This method must therefore be called after calling #drawOn:."
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphAfterDraw.
						self wantsContour
							ifTrue: [
								 (aCanvas
									contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) 
									into: [ :contourArray :contourY0 :contourY1 |
										self setProperty: #contour toValue: contourArray.
										self setProperty: #contourY0 toValue: contourY0.
										self setProperty: #contourY1 toValue: contourY1.
								])]
							ifFalse: [
								self setProperty: #contour toValue: nil.
								self setProperty: #contourY0 toValue: nil.
								self setProperty: #contourY1 toValue: nil ]]]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/12/2021 14:48:41'!
displayBoundsUpdateFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us.
	To be called after #postDrawOn:. See also #displayBoundsSetFrom:
	This more complicated update is needed because the contour information from #drawOn: was possibly
	when drawing submorphs. So, the controur from #drawOn: needs to be retrieved, and updated with the
	new contour information from #postDrawOn:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			(aCanvas isBoundsFinderCanvas or: [c canvasToUse == aCanvas]) ifTrue: [
				| oldContour oldTop oldBottom |
				oldContour _ self valueOfProperty: #contour.
				oldTop _ self valueOfProperty: #contourY0.
				oldBottom _ self valueOfProperty: #contourY1.
				privateDisplayBounds _ privateDisplayBounds
					quickMerge: aCanvas boundingRectOfCurrentMorphAfterDraw.
				self wantsContour ifTrue: [
					(aCanvas
						contourOfCurrentMorphAfterDrawUpdate: oldContour
						oldTop: oldTop
						oldBottom: oldBottom
						into: [ :contourArray :contourY0 :contourY1 |
							self setProperty: #contour toValue: contourArray.
							self setProperty: #contourY0 toValue: contourY0.
							self setProperty: #contourY1 toValue: contourY1.
						])]]]].! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 8/12/2021 15:48:36'!
basicDisplayBounds

	^ self world canvas boundsFinderCanvas displayBoundsForHand: self! !


!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 8/12/2021 15:01:55'!
boundsFinderCanvas
	^self subclassResponsibility! !

!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/12/2021 16:33:24'!
frameRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol
	"
	(BitBltCanvas onForm: Display)	
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| tlColor brColor |
	tlColor _ aColor.
	brColor _ aColor.
	aSymbol == #raised ifTrue: [
		tlColor _ aColor quiteWhiter.
		brColor _ aColor quiteBlacker ].

	aSymbol == #inset ifTrue: [
		tlColor _ aColor quiteBlacker.
		brColor _ aColor quiteWhiter ].

	self frameRectangle: aRectangle topLeftColor: tlColor bottomRightColor: brColor borderWidth: borderWidth! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/12/2021 15:12:09'!
drawWorld: aWorldMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| visibleRootMorphs visibleRootsDamage worldDamage |
	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	self boundsFinderCanvas updateBoundsIn: aWorldMorph addDamageTo: aDamageRecorder.

	visibleRootMorphs _ aWorldMorph privateSubmorphs select: [ :m | m visible ].
	visibleRootsDamage _ Array new: visibleRootMorphs size.
	
	worldDamage _ self computeDamage: aWorldMorph repair: aDamageRecorder
		rootMorphs: visibleRootMorphs rootMorphsDamage: visibleRootsDamage.

	self drawWorldBackground: aWorldMorph rects: worldDamage.

	self drawWorld: aWorldMorph
		rootMorphs: visibleRootMorphs rootMorphsDamage: visibleRootsDamage
		allDamage: worldDamage.

	aDamageRecorder reset.
	^ worldDamage! !


!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 8/12/2021 16:30:30'!
line: pt1 to: pt2 width: wp color: c

	|  p1 p2 w |
	(wp > 0 and: [ c isTransparent not ]) ifTrue: [
		p1 _ (currentTransformation transform: pt1) rounded.
		p2 _ (currentTransformation transform: pt2) rounded.
		w _ (currentTransformation externalizeScalar: wp) rounded.
		self setPaintColor: c.
		engine
			width: w;
			height: w;
			drawFrom: p1 to: p2 ]! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/30/2014 19:58'!
image: aForm at: aPoint
	"Draw a translucent image using the best available way of representing translucency."

	self image: aForm
		at: aPoint
		sourceRect: aForm boundingBox! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 8/12/2021 16:30:14'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."

	| r p |
	p _ (currentTransformation transform: aPoint) rounded.
	r _ (form depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
	engine colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	engine image: aForm at: p sourceRect: sourceRect rule: r.
	(form depth = 32 and: [ aForm depth < 32 ]) ifTrue: [
		"If we blit to 32bpp from one of smaller depth,
		it will have zero in the alpha channel (until BitBlt is fixed!!)
		This is the same workaround as in #asFormOfDepth:"
		engine sourceForm: nil.
		engine combinationRule: 40. "fixAlpha:with:"
		engine copyBits ]! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 1/11/2020 15:19:17'!
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |
	p _ (currentTransformation transform: aPoint) rounded.
	self setPaintColor: aColor.
	engine colorMap: stencilForm maskingMap.
	engine stencil: stencilForm
		at: p
		sourceRect: sourceRect! !

!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 8/12/2021 16:25:55'!
ellipseCenter: center radius: radiusPointOrNumber borderWidth: mbw borderColor: mbc fillColor: morphFillColor

	| displayRectangle doBorder doFill |
	doBorder _ mbw > 0 and: [ mbc isTransparent not].
	doFill _ morphFillColor isTransparent not.
	doBorder | doFill ifTrue: [
		displayRectangle _ (currentTransformation externalizeRectangle: (Rectangle center: center extent: radiusPointOrNumber asPoint * 2)) rounded.
		"draw the border of the oval"
		doBorder ifTrue: [
			self setPaintColor: mbc.
			engine frameOval: displayRectangle borderWidth: mbw rounded].
		"fill the inside"
		doFill ifTrue: [
			self setPaintColor: morphFillColor.
			engine fillOval: (displayRectangle insetBy: mbw rounded) ]]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 3/10/2018 22:06:13'!
fillRectangle: aRectangle color: aColor
	"Fill the given rectangle."

	aColor isTransparent ifFalse: [
		self 
			frameAndFillRectangle: aRectangle
			fillColor: aColor
			borderWidth: 0
			borderColor: `Color transparent` ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/12/2021 16:28:24'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	(BitBltCanvas onForm: Display) fillRectangle: (10@10 extent: 100@100) tilingWith: (BitBltCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red. Display forceToScreen.
	"
	self class accessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		"Warning: aForm boundingBox is most likely different from AuxForm boundingBox!!"
		self fillRectangle: aRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/12/2021 16:29:04'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor

	| rect bw |
	r area = 0 ifTrue: [ ^self ].
	rect _ (currentTransformation externalizeRectangle: r) rounded.
	bw _ (currentTransformation externalizeScalar: borderWidth) rounded.

	"draw the border of the rectangle"
	borderColor isTransparent ifFalse: [
		self setPaintColor: borderColor.
		engine frameRect: rect borderWidth: bw ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		engine fillRect: (rect insetBy: bw) ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/12/2021 16:29:27'!
frameRectangle: r borderWidth: borderWidth color: borderColor
	"
	(BitBltCanvas onForm: Display)
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| rect bw |
	(borderWidth > 0 and: [ borderColor isTransparent not ]) ifTrue: [
		rect _ (currentTransformation externalizeRectangle: r) rounded.
		bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
		self setPaintColor: borderColor.
		engine
			frameRect: rect
			borderWidth: bw ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/12/2021 16:29:33'!
frameRectangle: aRectangle topLeftColor: tlColor bottomRightColor: brColor borderWidth: borderWidth
	"
	(BitBltCanvas onForm: Display)	
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| displayRectangle bw |
	bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	displayRectangle _ (currentTransformation externalizeRectangle: aRectangle) rounded.
	self
		frameRectangle: displayRectangle
		borderWidth: bw
		topLeftColor: tlColor
		bottomRightColor: brColor.! !

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 8/12/2021 16:30:58'!
roundRect: aRectangle color: aColor radius: r
	"
	(BitBltCanvas onForm: Display) roundRect: (10@10 extent: 200@100) color: Color red radius: 10.5. Display forceToScreen.
	"
	"radius is not scaled properly..."

	"top stripe"
	self
		image: (self class topLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self fillRectangle: ((aRectangle withHeight: r) insetBy: r@0) color: aColor.

	"center stripe"
	self fillRectangle: (aRectangle insetBy: (0 @ r corner: 0 @ r)) color: aColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@r negated)) extent: (aRectangle width - r - r@r)) color: aColor! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 8/12/2021 16:25:23'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| p1 |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ currentTransformation transform: aPoint rounded.
	p1 _ p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 _ p1 rounded.
	engine colorMap: nil.
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 8/12/2021 16:25:31'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"
	| p1 |
	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.

	aColor = `Color black` ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: `Color black`.
		font
			onBitBltCanvasEngine: engine
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: p1
			color: topColor ].
	aColor = `Color white` ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: `Color white`.
		font
			onBitBltCanvasEngine: engine
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: p1 + `0@2`
			color: bottomColor ].
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1 + `0@1`
		color: aColor.! !

!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 8/12/2021 16:32:25'!
windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	BitBltCanvas releaseClassCachedState; releaseClassState.
	(BitBltCanvas onForm: Display) windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green.
	Display forceToScreen
	"

	| bottomColor he tl tr |
	"top stripe"
	self
		image: (self class topLeftCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: lh) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.

	"left and right borders"
	tl _ aRectangle topLeft + (0@lh).
	tr _ aRectangle topRight + (bw negated@lh).
	he _ bw@(aRectangle height - lh - r).
	self fillRectangle: (tl extent: he) color: bottomColor.
	self fillRectangle: (tr extent: he) color: bottomColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1 borderWidth: bw)
		multipliedBy: bottomColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1 borderWidth: bw)
		multipliedBy: bottomColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@bw negated)) extent: (aRectangle width - r - r@bw)) color: bottomColor.

	"inside"
	self fillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) color: insideColor! !

!BitBltCanvas methodsFor: 'drawing - Global Coordinates' stamp: 'jmv 8/12/2021 16:28:47'!
frameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor
	"
	(BitBltCanvas onForm: Display)
		frameAndFillGlobalRect: (10@10 extent: 300@200)
		fillColor: Color green
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	engine
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth;
		combinationRule: (fillColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: fillColor;
		fillRect: (rect insetBy: borderWidth).
		! !

!BitBltCanvas methodsFor: 'drawing - Global Coordinates' stamp: 'jmv 8/12/2021 16:29:13'!
frameGlobalRect: rect borderWidth: borderWidth color: borderColor
	"
	(BitBltCanvas onForm: Display)
		frameGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"

	engine
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth.! !

!BitBltCanvas methodsFor: 'drawing - Global Coordinates' stamp: 'jmv 8/12/2021 16:29:43'!
frameReverseGlobalRect: rect borderWidth: borderWidth
	"
	(BitBltCanvas onForm: Display)
		frameReverseGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	engine
		sourceForm: nil;
		fillColor: `Color gray`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/11/2020 15:19:54'!
fillRectangle: aRectangle tilingWith: aForm sourceRect: patternBox rule: aCombinationRule

	| displayRectangle  additionalOffset clippedEngine targetTopLeft clipOffset ex 
	targetBox savedMap top left |
	
	ex _ patternBox extent.
	ex x = 0 ifTrue: [ ^self ].
	ex y = 0 ifTrue: [ ^self ].
	displayRectangle _ (currentTransformation externalizeRectangle: aRectangle) rounded.

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	additionalOffset _ `0@0`.
	clippedEngine _ engine clippedBy: displayRectangle.
	targetTopLeft _ clippedEngine clipRect topLeft truncateTo: ex.
	clipOffset _ displayRectangle topLeft - targetTopLeft.
	additionalOffset _ (clipOffset \\ ex) - ex.

	"do it iteratively"
	targetBox _ clippedEngine clipRect.
	savedMap _ clippedEngine colorMap.
	clippedEngine sourceForm: aForm;
		fillColor: nil;
		combinationRule: aCombinationRule;
		sourceRect: patternBox;
		colorMap: (aForm colormapIfNeededFor: clippedEngine destForm).
	top _ (targetBox top truncateTo: patternBox height) + additionalOffset y.
	left _  (targetBox left truncateTo: patternBox width) + additionalOffset x.

	left to: (targetBox right - 1) by: patternBox width do: [:x |
		top to: (targetBox bottom - 1) by: patternBox height do: [:y |
			clippedEngine destOrigin: x@y; copyBits]].
	clippedEngine colorMap: savedMap! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/14/2020 09:32:10'!
setClipRect: aRectangle
	"In targetForm coordinates"

	super setClipRect: aRectangle.
	engine clipRect: aRectangle! !

!methodRemoval: BitBltCanvas #textComposition:bounds:color:selectionColor: stamp: 'jmv 8/13/2021 11:51:46'!
BitBltCanvas removeSelector: #textComposition:bounds:color:selectionColor:!
!methodRemoval: BitBltCanvas #updatingMorphBoundsDo: stamp: 'jmv 8/13/2021 12:03:20'!
BitBltCanvas removeSelector: #updatingMorphBoundsDo:!
!methodRemoval: BitBltCanvas #displayBoundsForHand: stamp: 'jmv 8/13/2021 12:03:59'!
BitBltCanvas removeSelector: #displayBoundsForHand:!
!methodRemoval: MorphicCanvas #fullAddRedrawRect:to: stamp: 'jmv 8/13/2021 12:04:38'!
MorphicCanvas removeSelector: #fullAddRedrawRect:to:!
!methodRemoval: MorphicCanvas #fullUpdateCurrentProtrudingBounds stamp: 'jmv 8/13/2021 12:05:40'!
MorphicCanvas removeSelector: #fullUpdateCurrentProtrudingBounds!
!methodRemoval: MorphicCanvas #displayBoundsForHand: stamp: 'jmv 8/13/2021 12:04:02'!
MorphicCanvas removeSelector: #displayBoundsForHand:!
!methodRemoval: MorphicCanvas #updatingMorphBoundsDo: stamp: 'jmv 8/13/2021 12:03:20'!
MorphicCanvas removeSelector: #updatingMorphBoundsDo:!
!methodRemoval: MorphicCanvas #updateHandsDisplayBounds: stamp: 'jmv 8/13/2021 12:05:27'!
MorphicCanvas removeSelector: #updateHandsDisplayBounds:!
!methodRemoval: MorphicCanvas #fullAddCurrentRect:submorphs:to: stamp: 'jmv 8/13/2021 12:04:45'!
MorphicCanvas removeSelector: #fullAddCurrentRect:submorphs:to:!
!methodRemoval: MorphicCanvas #fullUpdateProtrudingBounds: stamp: 'jmv 8/13/2021 12:05:38'!
MorphicCanvas removeSelector: #fullUpdateProtrudingBounds:!
!classDefinition: #BitBltCanvas category: #'Morphic-Support'!
MorphicCanvas subclass: #BitBltCanvas
	instanceVariableNames: 'boundsFinderCanvas'
	classVariableNames: 'AccessProtect AuxBlitter AuxForm CachedForms'
	poolDictionaries: ''
	category: 'Morphic-Support'!

!BitBltCanvas reorganize!
('accessing' boundsFinderCanvas)
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:topLeftColor:bottomRightColor:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius:)
('drawing-text' drawString:from:to:atBaseline:font:color: drawStringEmbossed:from:to:at:font:color:)
('drawing-icons' drawCloseIcon drawCollapseIcon drawDownIcon drawExpandIcon drawExpandedAt: drawLeftIcon drawMenuIcon drawNotExpandedAt: drawPushPinIcon drawRightIcon drawUpIcon)
('drawing-windows' drawRoundedFrame:border:color:insideColor:labelHeight: roundBottomLeftCornerX:y:length:border:color: roundBottomRightCornerX:y:length:border:color: roundEdge:color: roundTopLeftCornerX:y:length:border:color: roundTopRightCornerX:y:length:border:color: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing - Global Coordinates' frameAndFillGlobalRect:fillColor:borderWidth:color: frameGlobalRect:borderWidth:color: frameReverseGlobalRect:borderWidth:)
('initialization' initializeWithOrigin: world:)
('private' buildAuxWith:multipliedWith: fillRectangle:tilingWith:sourceRect:rule: fontToUse: frameRectangle:borderWidth:topLeftColor:bottomRightColor: pvtDrawButtonFaceForm: setClipRect: setForm:preferSubPixelAntiAliasing: setPaintColor:)
('morphic' boundingRectOfCurrentMorphAfterDraw canDraw: drawCurrentAndSubmorphs drawHand morph:isAtPoint:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
BitBltCanvas allInstancesDo: [ :c | c instVarNamed: 'boundsFinderCanvas' put: (BitBltBoundsFinderCanvas new initializeWithOrigin: 0@0) ]!

