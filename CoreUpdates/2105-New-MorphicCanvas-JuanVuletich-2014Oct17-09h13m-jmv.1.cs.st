'From Cuis 4.2 of 25 July 2013 [latest update: #2093] on 17 October 2014 at 9:18:03.59902 am'!
!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'form clipLeft clipTop clipRight clipBottom shadowColor transformations currentTransformation cti currentMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #BitBltCanvas category: #'Morphic-Support'!
MorphicCanvas subclass: #BitBltCanvas
	instanceVariableNames: 'port'
	classVariableNames: 'AccessProtect AuxBlitter AuxForm CachedForms'
	poolDictionaries: ''
	category: 'Morphic-Support'!

!BitBltCanvas commentStamp: '<historical>' prior: 0!
origin is aPoint, and it is expressed relative to the form. Usually, the form doesn't cover the whole World, and origin is negative. For instance, if we just want to draw the part of the World in (100@100 extent: 50@50) to another form, then fom extent = 50@50, and origin = -100@-100.

clipRect is relative to the form. For instance, if we only need to draw the part in (110@110 extent: 20@20) to the form above, then clipRect is (10@10 extent: 20@20)

All public protocol and drawing services is relative to World. The idea is that we only care about origin/form when we set them. Afterwards, we don't need to care about them. All the operations are done as if the whole World was drawn on Display.!


!MorphicCanvas methodsFor: 'private' stamp: 'jmv 10/9/2014 22:19'!
canvasOrigin
	"Return the current origin for drawing operations"
	^ transformations first translation! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 10/9/2014 22:26'!
externalizeDisplayBounds: r from: aMorph
	"r is a Rectangle, expressed in aMorph's coordinate system.
	Answer another rectangle, that bounds r when translated to World coordinates"

	"Should translate only to whatever world or PasteUp we are displaying.
	Fix when implementing multiple Canvases (Displays) showing different
	(potentially nested Worlds)"
	| inOwners owner |
	self flag: #jmvVer2.

	inOwners _ aMorph location displayBoundsOfTransformOf: r.
	owner _ aMorph owner.
	^owner
		ifNotNil: [ self externalizeDisplayBounds: inOwners from: owner]
		ifNil: [ inOwners ]! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 9/30/2014 22:55'!
setClipRect: aRectangle
	"by convention, aRectangle includes left and top but does not include right and bottom.
	We do draw clipRight and clipBottom but not beyond.
	"
	"In form coordinates"

	clipLeft _ aRectangle left.
	clipTop _ aRectangle top.
	clipRight _ aRectangle right - 1.
	clipBottom _ aRectangle bottom - 1! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 10/9/2014 22:22'!
setOrigin: aPoint

	transformations first setTranslation: aPoint! !

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 10/8/2014 22:46'!
clipRect
	"Return the currently active clipping rectangle"
	"In form coordinates"

	^ clipLeft@clipTop corner: clipRight@clipBottom+1! !

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 10/8/2014 22:47'!
currentTransformation
	"Warning. Only valid inside a #drawOn: method"
	^currentTransformation! !

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 10/9/2014 22:21'!
depth

	^ form depth! !

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 10/9/2014 22:27'!
extent

	^ form extent! !

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 10/9/2014 22:21'!
form

	^ form! !

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 10/8/2014 22:38'!
newClipRect: aRectangleOrNil
	"aRectangle is in world coordinates.
	But ivar clipRect is relative to the form,
	For example, if we had previously been built like
		aCanvas on: someForm over: (100@100 extent 200@100)
	then our origin would be -100 @ -100.
	Then, a clipRect argument like (120@120 extent: 40@30) would mean affecting
	only (20@20 extent: 40@30) in our form"

	self setClipRect: (aRectangleOrNil
		ifNil: [ 0@0 corner: form extent ]
		ifNotNil: [ aRectangleOrNil translatedBy: self canvasOrigin ])! !

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 10/9/2014 22:23'!
shadowColor: aColor
	shadowColor _ aColor! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/14/2014 16:16'!
currentOwnerIfClips: currentMorphOwnerOrNil! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/9/2014 22:25'!
displayBoundsOf: aMorph
	"Answer a rectangle that completely bounds aMorph when drawn on our form.
	Might be larger than strictly required.
		- In Morphic 2, this could cause clipping artifacts. This doesn't usually happen because:
			a) Morphic 2 doesn't use scaling and rotation
			b) Most Morphic 2 morphs have rectangular shape.
		- In Morphic 3, clipping also considers the real shape of the owner morph. This avoids those artifacts."

	"Think about doing a BoundsFinderCanvas even for Morphic 2"
	self flag: #jmvVer2.

	^self externalizeDisplayBounds: (0@0 extent: aMorph morphExtent) from: aMorph! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/9/2014 22:30'!
displayFullBoundsOf: aMorph
	"Answer a rectangle that completely bounds aMorph and submorphs when drawn on our form.
	Might be larger than strictly required. See comment at #displayBoundsOf:"

	| r |
	"Think about doing a BoundsFinderCanvas even for Morphic 2"

	self flag: #jmvVer2.

	r _ self displayBoundsOf: aMorph.
	aMorph unclippedSubmorphsReverseDo: [ :m |
		m visible ifTrue: [
			r _ r merge: (self displayFullBoundsOf: m) ]].

	^r! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/9/2014 22:18'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| br w |
	br _ currentMorph morphExtent.
	w _ br > (20@20)
		ifTrue: [ 10 ]
		ifFalse: [ 1 ].
	self
		frameAndFillRectangle: (0@0 corner: br)
		fillColor: Color red
		borderWidth: w
		borderColor: Color yellow.
	self line: 0@0 to: (br-w) width: w color: Color yellow.
	self line: (br x-w) @ 0 to: 0 @ (br y - w) width: w color: Color yellow! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/9/2014 22:22'!
drawCurrentAsOutline

	(self isMorphVisible: currentMorph) ifTrue: [
		self
			reverseRectangleBorder: (0 @ 0 extent: currentMorph morphExtent)
			borderWidth: 2 ]! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/14/2014 16:17'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	| oldClipRect |
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	"preferiria ni preguntar... para M3 al menos"
	(self isFullMorphVisible: currentMorph) ifFalse: [
		self outOfMorph.
		^ self].

	currentMorph isKnownFailing ifTrue: [
		self drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [ Preferences cheapWindowReframe ]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].
	
	" --------------------------------------------------" 
	"Aca es donde hay que dibujar primero el morph, y despues los submorphs, sabiendo los bounds hallados al dibujar el morph!!!!!!!!!! (en M3)
	este es el mismo metodo que hace into: y outOfMorph    :D  "
	" --------------------------------------------------"
	"preferiria ni preguntar... para M3 al menos"
	(self isMorphVisible: currentMorph) ifTrue: [
		currentMorph drawOn: self ].

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')"
	currentMorph hasSubmorphs ifTrue: [
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self currentOwnerIfClips: currentMorph.
			oldClipRect _ self clipRect.
			self setClipRect: (oldClipRect intersect: self clippingRectForCurrentMorph).
			self fullDraw: clipped.
			self setClipRect: oldClipRect.
			self currentOwnerIfClips: nil ].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullDraw: m ].
		].
	self outOfMorph! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/9/2014 22:19'!
fullDrawHand: aHandMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	aHandMorph visible ifFalse: [^ self].
	self into: aHandMorph.
	aHandMorph fullDrawHandOn: self.
	self outOfMorph! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/8/2014 22:40'!
into: aMorph

	self intoLocation: aMorph location.
	currentMorph _ aMorph! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/8/2014 22:40'!
intoLocation: aMatrixTransform2x3

	| last |
	cti _ cti + 1.
	transformations size < cti
		ifTrue: [	
			currentTransformation _ currentTransformation composedWith: aMatrixTransform2x3.
			transformations add: currentTransformation ]
		ifFalse: [
			"reuse the instance"
			last _ currentTransformation.
			currentTransformation _ transformations at: cti.
			last composedWith: aMatrixTransform2x3 into: currentTransformation ]! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/8/2014 22:41'!
outOfLocation

	cti _ cti - 1.
	currentTransformation _ transformations at: cti! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/8/2014 22:44'!
outOfMorph

	self outOfLocation.
	currentMorph _ currentMorph owner! !

!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 10/8/2014 22:41'!
initialize
	super initialize.

	"We currently set up these only in #initialize.
	This is safe (wrt walkbacks during world redraw) because a new instance is created
	each time the world is redrawn. See #drawInvalidAreasWorld:submorphs:
	Maybe this cleanup should be in an aux method that can be called each time on an existing instance..."
	currentTransformation _ MatrixTransform2x3 identity.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ]! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 10/8/2014 22:44'!
drawString: s at: pt font: aFont color: aColor

	^ self drawString: s from: 1 to: s size at: pt font: aFont color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 10/8/2014 22:44'!
drawStringEmbossed: s at: aPoint font: fontOrNil color: c
	^self drawStringEmbossed: s from: 1 to: s size at: aPoint font: fontOrNil color: c! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 10/8/2014 22:45'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	aColor = Color black ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: Color black.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			font: fontOrNil
			color: topColor ].
	aColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: Color white.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint  + (0@2)
			font: fontOrNil
			color: bottomColor ].
	self
		drawString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint  + (0@1)
		font: fontOrNil
		color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 10/14/2014 22:31'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	| displayScanner leftInRun line boundsInWorld tl |

	tl _ boundsRect topLeft.
	boundsInWorld _ currentTransformation displayBoundsOfTransformOf: boundsRect.

	displayScanner _ MorphicScanner new 
		text: aTextComposition textComposed
		foreground: (shadowColor ifNil: [ c ])
		ignoreColorChanges: self isShadowDrawing.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aTextComposition lineIndexForPoint: (0@0 max: (clipLeft@clipTop)- boundsInWorld origin))
		to: (aTextComposition lineIndexForPoint: (boundsInWorld extent min: (clipRight@clipBottom+1) - boundsInWorld origin))
		do: [ :i |
			line _ aTextComposition lines at: i.
			aTextComposition
				displaySelectionInLine: line
				on: self
				textTopLeft: tl
				selectionColor: sc.
			leftInRun _ displayScanner displayLine: line textTopLeft: tl leftInRun: leftInRun  ]! !

!MorphicCanvas methodsFor: 'testing' stamp: 'jmv 10/9/2014 22:20'!
drawsOnDisplay
	^form == Display! !

!MorphicCanvas methodsFor: 'testing' stamp: 'jmv 10/8/2014 22:50'!
isShadowDrawing
	^ shadowColor notNil! !

!MorphicCanvas methodsFor: 'other' stamp: 'jmv 10/9/2014 22:27'!
contentsOfArea: aRectangle into: aForm
	| bb |
	bb _ BitBlt toForm: aForm.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height;
		copyBits.
	^aForm! !

!MorphicCanvas methodsFor: 'other' stamp: 'jmv 10/9/2014 22:25'!
showAt: pt invalidRects: updateRects
	| blt |
	blt _ (BitBlt toForm: Display)
		sourceForm: form;
		combinationRule: Form over.
	updateRects do:
		[:rect |
		blt sourceRect: rect;
			destOrigin: rect topLeft + pt;
			copyBits]! !

!MorphicCanvas methodsFor: 'printing' stamp: 'jmv 10/9/2014 22:29'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll:' on: '; print: form.! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 10/9/2014 22:51'!
depth: depth over: aRectangle

	^self on: (Form extent: aRectangle extent depth: depth) over: aRectangle! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 10/9/2014 22:53'!
forShadowOver: aRectangle
	"Color black represents one for 1bpp"

	^(self on: (Form extent: aRectangle extent depth: 1) over: aRectangle) 
		shadowColor: Color black! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 10/9/2014 22:52'!
on: aForm over: aRectangle
	"Warning: aForm extent must equal aRectangle extent"

	^ self new
		setForm: aForm;
		setOrigin: aRectangle topLeft negated! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 10/1/2014 11:50'!
onForm: aForm

	^ self new setForm: aForm! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 10/9/2014 22:52'!
withExtent: extent depth: depth

	^ self new setForm: (Form extent: extent depth: depth)! !


!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 9/30/2014 19:58'!
line: pt1 to: pt2 width: w color: c
	|  p1 p2 |
	p1 _ currentTransformation intTransform: pt1.
	p2 _ currentTransformation intTransform: pt2.
	self setPaintColor: c.
	port
		width: w;
		height: w;
		drawFrom: p1 to: p2! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/30/2014 19:58'!
image: aForm at: aPoint
	"Draw a translucent image using the best available way of representing translucency."

	self image: aForm
		at: aPoint
		sourceRect: aForm boundingBox! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/30/2014 19:58'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |
	p _ currentTransformation intTransform: aPoint.
	self isShadowDrawing ifTrue: [
		^self stencil: aForm at: aPoint sourceRect: sourceRect color: shadowColor ].
	r _ (self depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
	port colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	port image: aForm at: p sourceRect: sourceRect rule: r! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/30/2014 19:58'!
image: aForm multipliedBy: aColor at: aPoint
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	Display getCanvas image: (SystemWindow roundedCornerTR: 20)multipliedBy: Color red at: 20@20
	"
	AccessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		self image: AuxForm at: aPoint sourceRect: aForm boundingBox ]! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/30/2014 19:58'!
stencil: stencilForm at: aPoint color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	^self stencil: stencilForm
		at: aPoint
		sourceRect: stencilForm boundingBox
		color: aColor! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/30/2014 19:58'!
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |
	p _ currentTransformation intTransform: aPoint.
	self setPaintColor: aColor.
	port colorMap: stencilForm maskingMap.
	port stencil: stencilForm
		at: p
		sourceRect: sourceRect! !

!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 9/30/2014 19:58'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: mfc

	| displayRectangle |
	displayRectangle _ (currentTransformation displayBoundsOfTransformOf: (Rectangle center: mcx@mcy extent: mrx@mry * 2)) truncated.
	"draw the border of the oval"
	(mbw = 0 or: [mbc isTransparent]) ifFalse:[
		self setPaintColor: mbc.
		port frameOval: displayRectangle borderWidth: mbw].
	"fill the inside"
	mfc isTransparent ifFalse: [
		self setPaintColor: mfc.
		port fillOval: (displayRectangle insetBy: mbw)].
! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/30/2014 19:58'!
fillRectangle: aRectangle color: aColor
	"Fill the given rectangle."

	| color |

	color _ aColor.
	self isShadowDrawing ifTrue: [
		color _ shadowColor ].

	^self 
		frameAndFillRectangle: aRectangle
		fillColor: color
		borderWidth: 0
		borderColor: Color transparent! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/30/2014 19:58'!
fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

	self fillRectangle: (aRectangle insetBy: borderWidth) color: aColor.
	self frameRectangle: aRectangle color: baseColorForBorder borderWidth: borderWidth borderStyleSymbol: aSymbol! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 10/9/2014 23:00'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	Display getCanvas fillRectangle: (10@10 extent: 100@100) tilingWith: (BitBltCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red.
	"

	AccessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		"Warning: aForm boundingBox is most likely different from AuxForm boundingBox!!"
		self fillRectangle: aRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/30/2014 19:58'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect |

	r area = 0 ifTrue: [ ^self ].
	rect _ currentTransformation displayBoundsOfTransformOf: r.

	"draw the border of the rectangle"
	borderColor isTransparent ifFalse:[
		self setPaintColor: borderColor.
		port frameRect: rect borderWidth: borderWidth ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillRect: (rect insetBy: borderWidth) ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/30/2014 19:58'!
frameRectangle: r borderWidth: borderWidth color: borderColor
	"
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red
	"
	| rect |
	rect _ currentTransformation displayBoundsOfTransformOf: r.
	self setPaintColor: borderColor.
	port
		frameRect: rect
		borderWidth: borderWidth.! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/30/2014 19:58'!
frameRectangle: r color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol
	"
	Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white.
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 2
		borderStyleSymbol: #raised.
	"

	| displayRectangle bw |
		bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	aSymbol == #raised ifTrue: [
		displayRectangle _ currentTransformation displayBoundsOfTransformOf: r.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteWhiter
			bottomRightColor: aColor quiteBlacker ].

	aSymbol == #inset ifTrue: [
		displayRectangle _ currentTransformation displayBoundsOfTransformOf: r.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteBlacker
			bottomRightColor: aColor quiteWhiter ].
	
	"Unrecognized border style. Draw some border..."
	self frameRectangle: r borderWidth: bw color: aColor! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/30/2014 19:58'!
reverseRectangleBorder: r borderWidth: borderWidth
	"
	Display getCanvas
		reverseRectangleBorder: (10@10 extent: 300@200)
		borderWidth: 20
	"
	| rect |
	rect _ currentTransformation displayBoundsOfTransformOf: r.
	port
		sourceForm: nil;
		fillColor: Color gray;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth! !

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 10/1/2014 09:40'!
roundRect: aRectangle color: aColor radius: r
	"
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10
	"
	"radious is not scaled properly..."
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

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 10/1/2014 09:42'!
roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	"
	Display restore.
	BitBltCanvas clearFormsCache. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientBottom: 0.5 gradientHeight: 35
	"
	| bottomColor |
	"top stripe"
	self
		image: (self class topLeftCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topLeft.
	self
		image: (self class topRightCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topRight - (r@0).
	self
		fillRectangle: ((displayRectangle withHeight: h) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.
	"center stripe"
	self fillRectangle: (displayRectangle insetBy: (0 @ h corner: 0 @ r)) color: bottomColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomRight - (r@r) .
	self fillRectangle: ((displayRectangle bottomLeft + (r@r negated)) extent: (displayRectangle width - r - r@r)) color: bottomColor! !

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 10/1/2014 09:43'!
roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	"
	Display restore.
	BitBltCanvas clearFormsCache. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientCenter: 0.0 gradientBottom: 1.0 gradient1Height: 35
	"
	| h2 |
	"top stripe"
	self
		image: (self class topLeftCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: h1) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor.
	
	"bottom stripe"
	h2 _ aRectangle height - h1.
	self
		image: (self class bottomLeftCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft + (0@h1).
	self
		image: (self class bottomRightCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight + (r negated@h1).
	self
		fillRectangle: ((aRectangle topLeft + (r@h1)) extent: (aRectangle width-r-r@h2))
		tilingWith: (self class verticalGrayGradient: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.! !

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 10/1/2014 09:44'!
windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	Display getCanvas windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green
	"
	"top stripe"
	| bottomColor he tl tr |
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
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@bw negated)) extent: (aRectangle width - r - r@bw)) color: bottomColor.

	"inside"
	self fillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) color: insideColor! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/30/2014 19:58'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	| p1 font colorToUse |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^self ].
	p1 _ currentTransformation intTransform: aPoint.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ AbstractFont default ].
	colorToUse _ shadowColor ifNil: [ aColor ].
	font on:	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: colorToUse! !

!BitBltCanvas methodsFor: 'testing' stamp: 'jmv 9/30/2014 22:22'!
isFullMorphVisible: aMorph

	| o aRectangle |
	aRectangle _ aMorph morphFullBoundsInWorld.
	o _ self canvasOrigin.
	"Optimization"
	(aRectangle right + o x) < clipLeft			ifTrue: [^ false].
	(aRectangle left + o x) > (clipRight+1)		ifTrue: [^ false].
	(aRectangle bottom + o y) < clipTop		ifTrue: [^ false].
	(aRectangle top + o y) > (clipBottom+1)	ifTrue: [^ false].
	^ true
! !

!BitBltCanvas methodsFor: 'testing' stamp: 'jmv 9/30/2014 22:23'!
isMorphVisible: aMorph

	| o aRectangle |
	aRectangle _ aMorph morphBoundsInWorld.
	o _ self canvasOrigin.
	"Optimization"
	(aRectangle right + o x) < clipLeft			ifTrue: [^ false].
	(aRectangle left + o x) > (clipRight+1)		ifTrue: [^ false].
	(aRectangle bottom + o y) < clipTop		ifTrue: [^ false].
	(aRectangle top + o y) > (clipBottom+1)	ifTrue: [^ false].
	^ true
! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 9/30/2014 19:58'!
buildAuxWith: aForm multipliedWith: aColor
	| h w r |
	w _ aForm width.
	h _ aForm height.
	AuxForm
		ifNotNil: [
			w _ w max: AuxForm width.
			h _ h max: AuxForm height.
			(AuxForm width < w or: [ AuxForm height < h ]) ifTrue: [ AuxForm _ nil ]].
	AuxForm
		ifNil: [
			AuxForm _ Form extent: w@h depth: 32.
			AuxBlitter _ BitBlt toForm: AuxForm ].
	
	r _ aForm boundingBox.
	AuxForm fill: r fillColor: aColor.
	AuxBlitter
		sourceForm: aForm;
		combinationRule: Form rgbMul;
		sourceRect: r;
		copyBits.! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 9/30/2014 19:58'!
fillRectangle: aRectangle tilingWith: aForm sourceRect: patternBox rule: aCombinationRule
	"aRectangle is in form coordinates, no transformation is done."
	| displayRectangle  additionalOffset clippedPort targetTopLeft clipOffset ex 
	targetBox savedMap top left |
	
	ex _ patternBox extent.
	ex x = 0 ifTrue: [ ^self ].
	ex y = 0 ifTrue: [ ^self ].
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	additionalOffset _ 0@0.
	clippedPort _ port clippedBy: displayRectangle.
	targetTopLeft _ clippedPort clipRect topLeft truncateTo: ex.
	clipOffset _ displayRectangle topLeft - targetTopLeft.
	additionalOffset _ (clipOffset \\ ex) - ex.

	"do it iteratively"
	targetBox _ clippedPort clipRect.
	savedMap _ clippedPort colorMap.
	clippedPort sourceForm: aForm;
		fillColor: nil;
		combinationRule: aCombinationRule;
		sourceRect: patternBox;
		colorMap: (aForm colormapIfNeededFor: clippedPort destForm).
	top _ (targetBox top truncateTo: patternBox height) + additionalOffset y.
	left _  (targetBox left truncateTo: patternBox width) + additionalOffset x.

	left to: (targetBox right - 1) by: patternBox width do: [:x |
		top to: (targetBox bottom - 1) by: patternBox height do: [:y |
			clippedPort destOrigin: x@y; copyBits]].
	clippedPort colorMap: savedMap! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 9/30/2014 19:58'!
frameRectangle: rect borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor
	"
	rect and borderWidth are in form coordinates. No transformation is done.
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		topLeftColor: Color green
		bottomRightColor: Color red
	Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white
	"
	| w h |

	rect area = 0 ifTrue: [^ self].

	self setPaintColor: topLeftColor.

	port frameRectTopLeft: rect borderWidth: borderWidth.

	borderWidth isNumber
		ifTrue: [w _ h _ borderWidth]
		ifFalse: [w _ borderWidth x.   h _ borderWidth y].
	self setPaintColor: bottomRightColor.
	port 
		 frameRectRight: rect width: w;
		 frameRectBottom: rect height: h! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 9/30/2014 19:58'!
resetGrafPort
	"Private!! Create a new grafPort for a new copy."

	port _ GrafPort toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	port sourceX: 0; width: 0! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 9/30/2014 23:00'!
setClipRect: aRectangle
	"In form coordinates"

	super setClipRect: aRectangle.
	port clipRect: aRectangle! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 9/30/2014 22:26'!
setForm: aForm

	form _ aForm.
	self resetGrafPort.
	self newClipRect: nil.
	shadowColor _ nil! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 9/30/2014 19:58'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor |
	paintColor _ shadowColor ifNil: [ aColor ].
	paintColor ifNil: [ paintColor _ Color transparent ].
	(paintColor is: #Color) ifFalse: [
		^self error: 'Cannot install color' ].

	"Okay, so paintColor really *is* a color"
	port sourceForm: nil.
	(paintColor isOpaque or: [ self depth < 32]) ifTrue: [
		port fillColor: paintColor.
		port combinationRule: Form paint.
		^self ].

	"BitBlt setup for alpha mapped transfer"
	port fillColor: paintColor.
	port combinationRule: Form blend! !

!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 9/30/2014 22:57'!
clippingRectForCurrentMorph
	"In form coordinates"

"En M3, es el obtenido al dibujarlo, recien.
Dejar eso claro en el nombre. Eliminar 'clipping'"
	^self displayBoundsOf: currentMorph! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 9/30/2014 19:58'!
drawButtonIconFromCurrentMorph
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	currentMorph magnifiedIcon ifNotNil: [ :theIcon |
		self
			image: theIcon
			multipliedBy: currentMorph iconColor
			at: (currentMorph morphExtent - theIcon extent //2).
		^true ].
	^false! !


!BitBltCanvas class methodsFor: 'class initialization' stamp: 'jmv 10/1/2014 09:44'!
clearFormsCache
	"Keep those that are not synthetized
	BitBltCanvas clearFormsCache
	"
	| new |
	new _ Dictionary new.
	#(#up #down #left #right ) do: [ :symbol |
		new
			at: {symbol. #gradient}
			put: (self arrowWithGradientOfDirection: symbol) ].
	CachedForms _ new! !

!BitBltCanvas class methodsFor: 'class initialization' stamp: 'jmv 10/9/2014 23:02'!
initialize
	"
	BitBltCanvas initialize
	"
	Smalltalk addToShutDownList: self.
	AccessProtect _ Semaphore forMutualExclusion.
	self clearFormsCache! !

!BitBltCanvas class methodsFor: 'system startup' stamp: 'jmv 9/30/2014 19:58'!
shutDown
	AuxForm _ nil.
	AuxBlitter _ nil! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 9/30/2014 19:58'!
bottomLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^CachedForms
		at: { #bottomLeft. r. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				bottomLeftCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 9/30/2014 19:58'!
bottomRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^CachedForms
		at: { #bottomRight. r. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				bottomRightCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 9/30/2014 19:58'!
steButtonForm: extent
	^CachedForms
		at: { #steButton . extent }
		ifAbsentPut: [
			| form canvas |
			form _ Form extent: extent depth: 32.
			canvas _ form getCanvas.
			canvas
				roundRect: (0@0 extent: extent)
				color: (Color gray: 0.4)
				radius: 4.
			canvas
				roundRect: (1@1 extent: extent-2)
				color: Color white
				radius: 4
				gradientTop: 1.0
				gradientCenter: 0.73
				gradientBottom: 0.94
				gradient1Height: (extent y-8+1 max: extent y//2).
			form]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 9/30/2014 19:58'!
topLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^CachedForms
		at: { #topLeft. r. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				topLeftCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 9/30/2014 19:58'!
topRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^CachedForms
		at: { #topRight. r. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				topRightCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 9/30/2014 19:58'!
verticalGrayGradient: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^CachedForms
		at: { #vertical. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				verticalGradient: height
				topColor: (Color gray: gradientTopFactor)
				bottomColor: (Color gray: gradientBottomFactor) ]! !

!BitBltCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 9/30/2014 19:58'!
arrowOfDirection: aSymbol size: finalSizeInteger
	^CachedForms
		at: { aSymbol . finalSizeInteger }
		ifAbsentPut: [
			self buildArrowOfDirection: aSymbol size: finalSizeInteger ]! !

!BitBltCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 10/9/2014 23:01'!
arrowWithGradientOfDirection: aSymbol
	"
	BitBltCanvas clearFormsCache.
	(BitBltCanvas arrowWithGradientOfDirection: #up) display.
	(BitBltCanvas arrowWithGradientOfDirection: #down) display.
	(BitBltCanvas arrowWithGradientOfDirection: #left) display.
	(BitBltCanvas arrowWithGradientOfDirection: #right) display.
	"
	^CachedForms
		at: { aSymbol . #gradient }
		ifAbsentPut: [ | packName pack |
			packName _ #GUIFormsPack.
			pack _ (Smalltalk at: packName) new.
			(pack decodeContentFrom: pack contentMap)
				at: 'gradient', aSymbol capitalized, 'Arrow' ]! !

!BitBltCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 9/30/2014 19:58'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	 (self buildArrowOfDirection: #up size: 120) display
	"

	| resizeFactor resizedForm f e c bottomMargin topMargin |
	resizeFactor _ 4.
	e _ finalSizeInteger@finalSizeInteger*resizeFactor.
	f _ Form extent: e depth: 32.
	c _ Color gray: 0.5.
	topMargin _ finalSizeInteger * 3//4.
	bottomMargin _ finalSizeInteger * 4//4.
	0 to: e y-1-bottomMargin do: [ :y |
		0 to: e x -1 do: [ :x |
			(e x / 2 - 1 - x) abs * 2 + topMargin < y ifTrue: [
				f colorAt: x@y put: c
			]
		]
	].
	resizedForm _ f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.

	aSymbolDirection == #right ifTrue: [
		resizedForm _ resizedForm rotateBy: 90 ].
	aSymbolDirection == #down ifTrue: [
		resizedForm _ resizedForm rotateBy: 180 ].
	aSymbolDirection == #left ifTrue: [
		resizedForm _ resizedForm rotateBy:  270 ].
		
	aSymbolDirection == #up ifFalse: [
		resizedForm _ resizedForm
			copy: (resizedForm boundingBox insetBy: (resizedForm width - finalSizeInteger/ 2.0) rounded) ].
		
	^resizedForm! !

!BitBltCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 10/9/2014 23:02'!
buildArrowWith: insideForm borderForm: borderForm
	| extent translucentForm color insideSpec borderSpec border background alpha|
	"
	Display getCanvas
		image: (BitBltCanvas buildArrowWith: BitBltCanvas downInsideForm borderForm: BitBltCanvas downBorderForm)
		at: 20@20
	Display getCanvas
		image: (BitBltCanvas buildArrowWith:BitBltCanvas upInsideForm borderForm: BitBltCanvas upBorderForm)
		at: 40@20
	"
	extent _ insideForm extent - (1@2).
	translucentForm _ Form extent: insideForm extent depth: 32.
	0 to: extent x-1 do: [ :x |
		0 to: extent y-1 do: [ :y |
			insideSpec _ insideForm colorAt: x@(y+1).
			borderSpec _ borderForm colorAt: x@(y+1).
			insideSpec = (Color r: 0.0 g: 0.0 b: 1.0)
				ifTrue: [ color _ Color transparent ]
				ifFalse: [
					borderSpec = (Color r: 1.0 g: 0.0 b: 0.0)
						ifTrue: [ color _ insideSpec ]
						ifFalse: [
							border _ x < (extent x//2)
								ifTrue: [Color white]
								ifFalse: [Color black].
							background _ borderForm colorAt: extent x@(y+1).
							alpha _ borderSpec red asFloat - background red / (border red - background red).
							color _ border alpha: (alpha min: 1 max: 0) ]].
			translucentForm colorAt: x@y put: color ]].
	^translucentForm! !

(BitBltCanvas classPool associationAt: #CachedForms) value: (FormCanvas classPool associationAt: #CachedForms) value.
BitBltCanvas initialize!
