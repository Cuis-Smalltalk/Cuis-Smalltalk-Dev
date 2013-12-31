'From Cuis 4.0 of 21 April 2012 [latest update: #1439] on 10 September 2012 at 12:02:57 am'!

!FormCanvas methodsFor: 'drawing' stamp: 'jmv 9/10/2012 00:00'!
line: pt1 to: pt2 width: w color: c
	| offset p1 p2 |
	p1 _ currentTransformation transform: pt1.
	p2 _ currentTransformation transform: pt2.
	offset _ origin - (w // 2) asPoint.
	self setPaintColor: c.
	port
		width: w;
		height: w;
		drawFrom: (p1 rounded + offset) to: (p2 rounded + offset)! !

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/9/2012 23:58'!
image: aForm at: aPoint
	"Draw a translucent image using the best available way of representing translucency."

	self image: aForm
		at: aPoint
		sourceRect: aForm boundingBox! !

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/9/2012 23:59'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |
	p _ currentTransformation transform: aPoint.
	self isShadowDrawing ifTrue: [
		^self stencil: aForm at: aPoint sourceRect: sourceRect color: shadowColor ].
	r _ (self depth < 32 or: [ aForm mightBeTranslucent not]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint]
		ifFalse: [ Form blend ].
	self image: aForm
		at: p rounded
		sourceRect: sourceRect
		rule: r! !

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/9/2012 23:58'!
image: aForm multipliedBy: aColor at: aPoint
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	Display getCanvas image: (SystemWindow roundedCornerTR: 20)multipliedBy: Color red at: 20@20
	"
	AccessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		self image: AuxForm at: aPoint sourceRect: aForm boundingBox ]! !

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/9/2012 23:59'!
stencil: stencilForm at: aPoint color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	^self stencil: stencilForm
		at: aPoint
		sourceRect: stencilForm boundingBox
		color: aColor! !

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/9/2012 23:59'!
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |
	p _ currentTransformation transform: aPoint.
	self setPaintColor: aColor.
	port colorMap: stencilForm maskingMap.
	port stencil: stencilForm
		at: p + origin
		sourceRect: sourceRect.! !

!FormCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 9/9/2012 23:56'!
fillOval: r color: fillColor borderWidth: borderWidth borderColor: borderColor

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

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 23:55'!
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

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 23:56'!
fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

	self fillRectangle: (aRectangle insetBy: borderWidth) color: aColor.
	self frameRectangle: aRectangle color: baseColorForBorder borderWidth: borderWidth borderStyleSymbol: aSymbol! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 23:54'!
fillRectangle: aRectangle infiniteForm: anInfiniteForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of anInfiniteForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	Display getCanvas fillRectangle: (10@10 extent: 100@100) infiniteForm: (SystemWindow titleGradient: 12) multipliedBy: Color red.
	"

	| f displayRectangle |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	f _ anInfiniteForm form.
	AccessProtect critical: [
		self buildAuxWith: f multipliedWith: aColor.
		self fillRectangle: displayRectangle tilingWith: AuxForm sourceRect: f boundingBox rule: Form paint ]! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 23:55'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect |
	rect _ (currentTransformation displayBoundsOfTransformOf: r) translatedBy: origin.
	"draw the border of the rectangle"
	borderColor isTransparent ifFalse:[
		self setPaintColor: borderColor.
		port frameRect: rect borderWidth: borderWidth ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillRect: (rect insetBy: borderWidth) ]! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 23:55'!
frameRectangle: r borderWidth: borderWidth color: borderColor
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

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 23:56'!
frameRectangle: r color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol
	"
	Display getCanvas zzfillRectangle: (10@10 extent: 300@200) colorOrInfiniteForm: Color white.
	Display getCanvas
		zzframeRectangle: (10@10 extent: 300@200)
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

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/9/2012 23:58'!
roundRect: aRectangle color: aColor radius: r
	"
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10
	"
	"radious is not scaled properly..."
	"top stripe"
	self
		image: (FormCanvas topLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (FormCanvas topRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self fillRectangle: ((aRectangle withHeight: r) insetBy: r@0) color: aColor.

	"center stripe"
	self fillRectangle: (aRectangle insetBy: (0 @ r corner: 0 @ r)) color: aColor.
	
	"bottom stripe"
	self
		image: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@r negated)) extent: (aRectangle width - r - r@r)) color: aColor! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/9/2012 23:59'!
roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	"
	Display restore.
	FormCanvas clearFormsCache. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientBottom: 0.5 gradientHeight: 35
	"
	| bottomColor |
	"top stripe"
	self
		image: (FormCanvas topLeftCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topLeft.
	self
		image: (FormCanvas topRightCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topRight - (r@0).
	self
		fillRectangle: ((displayRectangle withHeight: h) insetBy: r@0)
		infiniteForm: (FormCanvas verticalGrayGradient: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.
	"center stripe"
	self fillRectangle: (displayRectangle insetBy: (0 @ h corner: 0 @ r)) color: bottomColor.
	
	"bottom stripe"
	self
		image: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomLeft - (0@r).
	self
		image: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomRight - (r@r) .
	self fillRectangle: ((displayRectangle bottomLeft + (r@r negated)) extent: (displayRectangle width - r - r@r)) color: bottomColor! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/9/2012 23:59'!
roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	"
	Display restore.
	FormCanvas clearFormsCache. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientCenter: 0.0 gradientBottom: 1.0 gradient1Height: 35
	"
	| h2 |
	"top stripe"
	self
		image: (FormCanvas topLeftCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (FormCanvas topRightCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: h1) insetBy: r@0)
		infiniteForm: (FormCanvas verticalGrayGradient: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor.
	
	"bottom stripe"
	h2 _ aRectangle height - h1.
	self
		image: (FormCanvas bottomLeftCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft + (0@h1).
	self
		image: (FormCanvas bottomRightCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight + (r negated@h1).
	self
		fillRectangle: ((aRectangle topLeft + (r@h1)) extent: (aRectangle width-r-r@h2))
		infiniteForm: (FormCanvas verticalGrayGradient: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/9/2012 23:59'!
windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	Display getCanvas windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green
	"
	"top stripe"
	| bottomColor he tl tr |
	self
		image: (FormCanvas topLeftCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (FormCanvas topRightCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: lh) insetBy: r@0)
		infiniteForm: (FormCanvas verticalGrayGradient: lh gradientTop: topFactor gradientBottom: bottomFactor)
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
		image: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@bw negated)) extent: (aRectangle width - r - r@bw)) color: bottomColor.

	"inside"
	self fillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) color: insideColor! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2012 23:45'!
drawString: s at: pt font: aFont color: aColor

	^ self drawString: s from: 1 to: s size at: pt font: aFont color: aColor! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2012 23:45'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: c
	| font p1 |
	p1 _ currentTransformation transform: aPoint.
	port colorMap: nil.
	font _ fontOrNil ifNil: [StrikeFont default].
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: (origin + p1)
		strikeFont: font
		kern: font baseKern negated! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2012 23:47'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: font color: c kern: kern

	| p1 |
	p1 _ currentTransformation transform: aPoint.
	port colorMap: nil.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: (origin + p1)
		strikeFont: font
		kern: kern! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2012 23:47'!
drawString: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: c
	| font portRect bounds |
	bounds _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	port colorMap: nil.
	portRect _ port clipRect.
	port clipByX1: bounds left + origin x 
		y1: bounds top + origin y 
		x2: bounds right + origin x 
		y2: bounds bottom + origin y.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [port clipRect: portRect. ^self].
	port clipWidth = 0 ifTrue: [port clipRect: portRect. ^self].
	font _ fontOrNil ifNil: [StrikeFont default].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: (bounds topLeft + origin)
		strikeFont: font
		kern: font baseKern negated.
	port clipRect: portRect! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2012 23:47'!
drawString: s in: boundsRect font: fontOrNil color: c
	^self drawString: s from: 1 to: s size in: boundsRect font: fontOrNil color: c! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2012 23:48'!
drawStringEmbossed: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: aColor
	| font portRect insideColor bounds |
	bounds _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	port colorMap: nil.
	portRect _ port clipRect.
	port clipByX1: bounds left + origin x 
		y1: bounds top + origin y 
		x2: bounds right + origin x 
		y2: bounds bottom + origin y.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	font _ fontOrNil ifNil: [StrikeFont default].
	insideColor _ shadowColor ifNil: [ aColor ].
	insideColor = Color black ifFalse: [ | topColor |
		topColor _ insideColor alphaMixed: 0.25 with: Color black.
		port installStrikeFont: font foregroundColor: topColor.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: (bounds topLeft + origin)
			strikeFont: font
			kern: font baseKern negated ].
	insideColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ insideColor alphaMixed: 0.22 with: Color white.
		port installStrikeFont: font foregroundColor: bottomColor.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: (bounds topLeft + origin + (0@2))
			strikeFont: font
			kern: font baseKern negated ].
	port installStrikeFont: font foregroundColor: insideColor.
	port
		displayString: aString asString
		from: firstIndex
		to: lastIndex
		at: (bounds topLeft + origin + (0@1))
		strikeFont: font
		kern: font baseKern negated.
	port clipRect: portRect! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2012 23:48'!
drawStringEmbossed: s in: boundsRect font: fontOrNil color: c
	^self drawStringEmbossed: s from: 1 to: s size in: boundsRect font: fontOrNil color: c! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2012 23:49'!
paragraph: aParagraph bounds: boundsRect color: c selectionColor: sc
	| displayScanner leftInRun line boundsInWorld tl |

	tl _ boundsRect topLeft.
	boundsInWorld _ currentTransformation displayBoundsOfTransformOf: boundsRect.
	self setPaintColor: c.

	displayScanner _ MorphicScanner new 
		text: aParagraph paragraphText
		foreground: (shadowColor ifNil: [ c ])
		ignoreColorChanges: self isShadowDrawing.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aParagraph lineIndexForPoint: (0@0 max: clipRect origin- boundsInWorld origin))
		to: (aParagraph lineIndexForPoint: (boundsInWorld extent min: clipRect corner - boundsInWorld origin))
		do: [ :i |
			line _ aParagraph lines at: i.
			aParagraph
				displaySelectionInLine: line
				on: self
				paragraphTopLeft: tl
				selectionColor: sc.
			leftInRun _ displayScanner displayLine: line paragraphTopLeft: tl leftInRun: leftInRun  ]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/10/2012 00:00'!
shadowForm
	"Return a 1-bit shadow of my submorphs.  Assumes submorphs is not empty"
	| bnds canvas |
	bnds _ Rectangle merging: (submorphs collect: [:m | m morphFullBoundsInWorld]).
	canvas _ Display defaultCanvasClass forShadowOver: bnds.
	canvas into: self.
	self drawSubmorphsOn: canvas.
	^ canvas form offset: bnds topLeft - self morphPositionInWorld! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/9/2012 23:43'!
displayBulletIfAppropriateFor: textLine paragraphTopLeft: paragraphTopLeft
	"paragraphTopLeft is relative to the morph currently being drawn"

	| paragraphEnd count pattern |
	paragraphStyle ifNotNil: [
		(textLine isFirstLine and: [ paragraphStyle isListStyle ]) ifTrue: [
			pattern _ paragraphStyle listBulletPattern.
			"Count how many paragraphs before this one already used the pattern"
			count _ 0.
			paragraphEnd _ textLine first-1.
			[
			paragraphEnd > 0 and: [ ((text paragraphStyleOrNilAt: paragraphEnd) ifNotNil: [ :ps | ps listBulletPattern ]) = pattern ]] whileTrue: [
				count _ count + 1.
				paragraphEnd _ text string endOfParagraphBefore: paragraphEnd ].
			"Our number in the list, is one more than the count of previous contiguous paragraphs with this pattern"
			self
				displayBulletParagraphTopLeft: paragraphTopLeft
				number: count + 1]]! !

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/9/2012 23:47'!
displayBulletParagraphTopLeft: paragraphTopLeft number: bulletNumber
	"paragraphTopLeft is relative to the morph currently being drawn"

	| pattern i c j s bullet bulletPos bulletSize prefix |
	pattern _ paragraphStyle listBulletPattern.
	bullet _ pattern.
	(i _ pattern indexOf: $%) > 0
		ifTrue: [ bullet _ bulletNumber asString]
		ifFalse: [
			(i _ pattern indexOf: $z) > 0
				ifTrue: [ bullet _ (Character value: 96 + bulletNumber) asString ]
				ifFalse: [
					(i _ pattern indexOf: $Z) > 0
						ifTrue: [ bullet _ (Character value: 64 + bulletNumber) asString ]]].
	prefix _ 0.
	i > 0 ifTrue: [
		c _ pattern at: i.
		j _ i.
		s _ pattern size.
		[ j <= s and: [ (pattern at: j) = c ] ] whileTrue: [ j _ j + 1 ].
		j _ j - 1.
		bulletSize _ j-i+1.
		prefix _ bulletSize - bullet size max: 0.
		bullet size > bulletSize ifTrue: [
			bullet _ bullet copyFrom: bullet size - bulletSize + 1 to: bullet size ].
		bullet _ (pattern copyFrom: 1 to: i-1), bullet, (pattern copyFrom: j+1 to: pattern size) ].
	bulletPos _ paragraphStyle firstIndent + paragraphTopLeft x + ((font widthOf: $9) * prefix)@destY.
	canvas
		drawString: bullet
		from: 1
		to: bullet size
		at: bulletPos
		font: font
		color: foregroundColor
		kern: kern! !

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/9/2012 23:47'!
displayLine: textLine paragraphTopLeft: paragraphTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"paragraphTopLeft is relative to the morph currently being drawn"
	| done stopCondition nowLeftInRun startIndex string lastPos priorFont |

	paraTopLeft _ paragraphTopLeft.
	line _ textLine.
	lineY _ line top + paragraphTopLeft y.
	lineHeight _ line lineHeight.
	rightMargin _ line rightMargin + paragraphTopLeft x.
	lastIndex _ line first.
	leftInRun <= 0 ifTrue: [self setStopConditions].
	leftMargin _ (line leftMarginForAlignment: alignment) + paragraphTopLeft x.
	destX _ runX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				""
				foregroundColor _ paragraphColor.
				priorFont _ font.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				priorFont ifNotNil: [ destX _ destX + priorFont descentKern ].
				destX _ destX - font descentKern.
				kern _ 0 - font baseKern.
				spaceWidth _ font widthOf: Character space.
				xTable _ font xTable.
				map _ font characterToGlyphMap.
				stopConditions _ DefaultStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]
				""
			]
		].
		self displayBulletIfAppropriateFor: textLine paragraphTopLeft: paragraphTopLeft.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine paragraphTopLeft: paragraphTopLeft.

	lastIndex _ line first.
	leftInRun <= 0
		ifTrue: [nowLeftInRun _ text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun _ leftInRun].
	runStopIndex _ lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount _ 0.
	done _ false.
	string _ text string.

	self placeEmbeddedObject.
	[ done ] whileFalse: [
		startIndex _ lastIndex.
		lastPos _ destX@destY.
		stopCondition _ self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions
			kern: kern.
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor
				kern: kern ].
		"see setStopConditions for stopping conditions for displaying."
		done _ self perform: stopCondition ].
	^ runStopIndex - lastIndex   "Number of characters remaining in the current run"! !


!Paragraph methodsFor: 'display' stamp: 'jmv 9/10/2012 00:00'!
displayInsertionMarkAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas paragraphLeft: paragraphLeft
	"x, top, bottom, paragraphLeft are relative to the morph currently being drawn."

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
			"Keep tweaking if needed!! For italics with descenders (i.e. p), cursor shows a bit to the left..."
			d _ isBold ifTrue: [ h // 8 ] ifFalse: [ h // 9].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x.
			x1 _ x].
	x0-paragraphLeft < halfW ifTrue: [
		x1 _ x1 - x0 + halfW+paragraphLeft.
		x0 _ halfW+paragraphLeft ].
	r _ extentForComposing x-halfW-1.
	r < (x1-paragraphLeft) ifTrue: [
		x0 _ x0 + r - x1+paragraphLeft.
		x1 _ r +paragraphLeft].

	lastCaretRect _ x0-halfW@ top corner: x1+halfW+1 @ (bottom+1).
	aCanvas
		line: x0@(bottom-halfW) to: x1@(top+halfW)
		width: w color: caretColor! !

!Paragraph methodsFor: 'display' stamp: 'jmv 9/9/2012 23:44'!
displaySelectionInLine: line on: aCanvas paragraphTopLeft: paragraphTopLeft  selectionColor: sc

	"paragraphTopLeft is relative to the morph currently being drawn"
	selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock |
		self
			displaySelectionStartBlock: startBlock
			stopBlock: stopBlock
			InLine: line
			on: aCanvas
			paragraphTopLeft: paragraphTopLeft
			selectionColor: sc ]! !

!Paragraph methodsFor: 'display' stamp: 'jmv 9/9/2012 23:53'!
displaySelectionStartBlock: startBlock stopBlock: stopBlock InLine: line on: aCanvas paragraphTopLeft: paragraphTopLeft  selectionColor: sc
	"paragraphTopLeft is relative to the morph currently being drawn"

	| leftX rightX idx caretFont t b caretAttributes |

	startBlock ifNil: [^self].	"No selection"
	startBlock = stopBlock 
		ifTrue: [
			"Only show caret on line where clicked"
			startBlock textLine first = line first ifFalse: [
				^self ].
			leftX _ paragraphTopLeft x + startBlock left.
			idx _ startBlock stringIndex.
			caretAttributes _ editor ifNotNil: [ editor currentAttributes ].
			caretFont _ caretAttributes
				ifNil: [ model actualContents fontAt: idx ]
				ifNotNil: [ model actualContents fontIfApplying: caretAttributes ].
			b _ paragraphTopLeft y + line top + line baseline + caretFont descent-1.
			t _ paragraphTopLeft y + line top + line baseline - caretFont ascent.
			showCaret ifTrue: [
				self
					displayInsertionMarkAtX: leftX
					top: t
					bottom: b
					emphasis: caretFont emphasis
					on: aCanvas
					paragraphLeft: paragraphTopLeft x ]]
		ifFalse: [
			"Test entire selection before or after here"
			(stopBlock stringIndex < line first 
				or: [startBlock stringIndex > (line last + 1)])
					ifTrue: [^self].	"No selection on this line"
			(stopBlock stringIndex = line first 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].	"Selection ends on line above"
			(startBlock stringIndex = (line last + 1) 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].
			lastCaretRect _ nil.
			leftX _  paragraphTopLeft x + (startBlock stringIndex < line first 
				ifTrue: [ line ]
				ifFalse: [ startBlock ]) left.
			rightX _  paragraphTopLeft x + ((stopBlock stringIndex > (line last + 1) or: [
					stopBlock stringIndex = (line last + 1) 
						and: [stopBlock textLine ~= line]]) 
				ifTrue: [line right]
				ifFalse: [stopBlock left]).
			aCanvas
				fillRectangle: (leftX @ (line top +  paragraphTopLeft y) corner: rightX @ (line bottom +  paragraphTopLeft y))
				color: sc ].	"Selection begins on line below"! !


!Color class methodsFor: 'examples' stamp: 'jmv 9/9/2012 23:51'!
experimentsTowarsANewColorPalette
"
self experimentsTowarsANewColorPalette
"
| selectedHue selectedSaturation selectedV selectedColor h s v color width height selectedChroma selectedLuminance |
width _ 300.
height _ 120.
selectedColor _ Color fromUser.
selectedHue _ selectedColor hue.
selectedSaturation _ selectedColor saturation.
selectedChroma _ selectedColor chroma.
selectedV _ selectedColor brightness.
selectedLuminance _ selectedColor luminance.
Display getCanvas fillRectangle: (0@0 extent: height@height) color: selectedColor.
0 to: height do: [ :y |
	v _ 1.0 - (y / height).
	0 to: height do: [ :x |
		s _ x / height.
		color _ Color basicNew setHue: selectedHue saturation: s brightness: v.
		Display colorAt: x@(y+height) put: color
	].
	Display forceToScreen
].
0 to: height do: [ :y | | c |
	v _ 1.0 - (y / height).
	s _ 1.0 - (y / height).
	c _ s.
	0 to: width do: [ :x |
		h _ x / width * 360.
		
		color _ Color basicNew setHue: h chroma: c luminance: selectedLuminance.
"		color _ Color basicNew setHue: h chroma: c brightness: selectedV."
		color ifNil: [ color _ Color black ].
"		color _ Color basicNew setHue: h saturation: s brightness: selectedV."
		Display colorAt: x+height@y put: color.
		
		color _ Color basicNew setHue: h chroma: selectedChroma luminance: v.
"		color _ Color basicNew setHue: h chroma: selectedChroma brightness: v."
		color ifNil: [ color _ Color black ].
"		color _ Color basicNew setHue: h saturation: selectedSaturation brightness: v."
		Display colorAt: x+height@(y+height) put: color.
		
	].
	Display forceToScreen
].! !


!FormCanvas class methodsFor: 'cached forms' stamp: 'jmv 9/9/2012 23:50'!
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


!Morph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:55'!
drawDropHighlightOn: aCanvas

	self highlightedForDrop ifTrue: [
		aCanvas frameRectangle: (0@0 extent: self morphExtent) borderWidth: 1 color: self dropHighlightColor ]! !

!Morph methodsFor: 'drawing' stamp: 'jmv 9/10/2012 00:00'!
drawErrorOn: aCanvas
	"The morph (or one of its submorphs) had an error in its drawing method."
	| tl br w |
	w _ 10.
	tl _ 0@0.
	br _ self morphExtent.
	aCanvas
		frameAndFillRectangle: (tl corner: br)
		fillColor: Color red
		borderWidth: w
		borderColor: Color yellow.

	aCanvas line: tl +w to: br -w width: w color: Color yellow.
	aCanvas line: br x - w @ w to: w @ (br y - w) width: w color: Color yellow.! !

!Morph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:56'!
drawMouseDownHighlightOn: aCanvas

	self highlightedForMouseDown ifTrue: [
		aCanvas frameRectangle: (0@0 extent: self morphExtent) borderWidth: 1 color: self color darker darker ]! !

!Morph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:53'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		fillRectangle: (0@0 extent: self morphExtent)
		color: self color! !


!BorderedRectMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:53'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		fillRectangle: (0@0 extent: extent)
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:57'!
drawOn: aCanvas
	| rectangle w y0 h y1 y2 scrollbarThickness |
	aCanvas frameAndFillRectangle: (0@0 extent: extent) fillColor: self color borderWidth: borderWidth borderColor: borderColor.
	y0 _ 1.
	w _ extent x-2.
	scrollbarThickness _ ScrollBar scrollbarThickness.
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ w - scrollbarThickness.
		aCanvas
			frameRectangle: (extent x - scrollbarThickness@0
				extent: scrollbarThickness @ extent y)
			borderWidth: 1
			color: borderColor.
		aCanvas
			image: (FormCanvas arrowOfDirection: #up size: scrollbarThickness)
			at: extent x - scrollbarThickness@0.
		aCanvas
			image: (FormCanvas arrowOfDirection: #down size: scrollbarThickness)
			at: 0@0 + extent - scrollbarThickness.
		h _ extent y - (2 * scrollbarThickness).
		y1 _ (1.0 * self firstVisible-1 / completer entryCount * h) ceiling + y0 + scrollbarThickness-1.
		y2 _ (1.0 * self lastVisible / completer entryCount * h) floor + y0 + scrollbarThickness -1.
		aCanvas
			fillRectangle: (extent x - scrollbarThickness+2@y1 corner:  extent x-2 @ y2)
			color: Color veryLightGray ].
	self firstVisible
		to: self lastVisible
		do: [ :index |
			rectangle _ 1@y0 extent: w@self class itemHeight.
			index = self selected
				ifTrue: [
					aCanvas fillRectangle: rectangle color: (Theme current listHighlightFocused: true) ].
			aCanvas
				drawString: (completer entries at: index) asString
				in: rectangle
				font: self class listFont
				color: Theme current text.
			y0 _ y0 + self itemHeight ]! !


!EllipseMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:56'!
drawOn: aCanvas 

	| r bc bw |
	r _ 0@0 extent: extent.
	bw _ borderWidth.
	bc _ borderColor.
	aCanvas isShadowDrawing
		ifTrue: [
			bw _ 0.
			bc _ nil ].
	aCanvas fillOval: r color: color borderWidth: bw borderColor: bc.
! !


!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:51'!
drawOn: aCanvas
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas
				roundRect: (0@0 extent: extent)
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ].
	aCanvas
		fillRectangle: (14@25 extent: extent-(28@62))
		color: (Theme current paneBackgroundFrom: color)! !


!FrameRateMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:45'!
drawOn: aCanvas
	super drawOn: aCanvas.
	meanStepDelta ifNotNil: [
		aCanvas drawString: lastStepDelta rounded printString at: 0@0 font: StrikeFont default color: Color black.
		aCanvas drawString: meanStepDelta rounded printString at: 0@14 font: StrikeFont default color: Color black.
		"aCanvas drawString: lastStepStamp printString at: bounds topLeft + (0@28) font: StrikeFont default color: Color black "
		]! !


!HaloHandleMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:58'!
drawOn: aCanvas

	aCanvas
		image: (self class circleForm: extent)
		multipliedBy: (color alpha: 0.57)
		at: 0@0! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:59'!
drawOn: aCanvas 
	"Draw the hand itself (i.e., the cursor)."
	"This method is only called when we are carrying morphs around..."
	 aCanvas
		stencil: Cursor move
		at: 0@0
		color: Color black! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/10/2012 00:01'!
nonCachingFullDrawOn: aCanvas
	| shadowForm |
	"A HandMorph has unusual drawing requirements:
		1. the hand itself (i.e., the cursor) appears in front of its submorphs
		2. morphs being held by the hand cast a shadow on the world/morphs below
	The illusion is that the hand plucks up morphs and carries them above the world."
	"Note: This version does not cache an image of the morphs being held by the hand.
	 Thus, it is slower for complex morphs, but consumes less space."

	submorphs isEmpty ifTrue: [^ self drawOn: aCanvas].  "just draw the hand itself"

	"Note: We use a shadow form here to prevent drawing
	overlapping morphs multiple times using the transparent
	shadow color."
	shadowForm _ self shadowForm.

	"draw shadows"
	aCanvas stencil: shadowForm at: shadowForm offset  + self shadowOffset color: (Color black alpha: 0.5).
	
	"draw morphs in front of shadows"
	self drawSubmorphsOn: aCanvas.
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:57'!
restoreSavedPatchOn: aCanvas 
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."

	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas image: savedPatch at: savedPatch offset.
		submorphs notEmpty ifTrue: [ ^self ].

		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		self invalidRect: (savedPatch offset extent: savedPatch extent + self shadowOffset).
		Sensor currentCursor == Cursor normal ifFalse: [ Cursor normal show ].	"show hardware cursor"
		savedPatch _ nil ]! !


!HierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:55'!
drawOn: aCanvas

	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle 
			borderWidth: Preferences focusIndicatorWidth
			color: Theme current focusIndicator ]! !


!HoverHelpMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:50'!
drawOn: aCanvas

	| r |
	r _ 0@0 extent: extent.
	aCanvas roundRect: r color: self color radius: 4.
	aCanvas
		paragraph: paragraph
		bounds: (r insetBy: 4)
		color: Color black
		selectionColor: (Theme current textHighlightFocused: false)! !


!ImageMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:57'!
drawOn: aCanvas

	aCanvas image: image at: 0@0! !


!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/10/2012 00:00'!
drawLineToggleToTextFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
	"If I am not the only item in my container, draw the line between:
		- my toggle (if any) or my left edge (if no toggle)
		- and my text left edge"

	| myBounds myCenter hLineY hLineLeft |
	anIndentingListItemMorph isSoleItem ifTrue: [ ^ self ].
	myBounds _ anIndentingListItemMorph toggleRectangle.
	myBounds _ anIndentingListItemMorph location displayBoundsOfTransformOf: myBounds.
	myCenter _ myBounds center.
	hLineY _ myCenter y.
	hasToggle
		ifTrue: [ hLineLeft _ myBounds right - 3 ]
		ifFalse: [ hLineLeft _ myCenter x - 1 ].
	"Draw line from toggle to text"
	aCanvas
		line: hLineLeft @ hLineY
		to: myBounds right + 0 @ hLineY
		width: 1
		color: lineColor! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/10/2012 00:00'!
drawLinesToFirstChildFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor
	"Draw line from me to next sibling"

	| vLineX vLineTop vLineBottom childCenter |
	childCenter _ anIndentingListItemMorph firstChild location externalizePosition:
			anIndentingListItemMorph firstChild toggleRectangle center.
	vLineX _ childCenter x - 1.
	vLineTop _ (anIndentingListItemMorph location
		externalizePosition: anIndentingListItemMorph morphExtent) y.
	anIndentingListItemMorph firstChild hasToggle
		ifTrue: [ vLineBottom _ childCenter y - 7 ]
		ifFalse: [ vLineBottom _ childCenter y ].
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @vLineBottom
		width: 1
		color: lineColor! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/10/2012 00:00'!
drawLinesToNextSiblingFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
	| vLineX myCenter vLineTop vLineBottom nextSibCenter |

	self flag: #jmvVer2. "complicated... not intuitive... who should draw this?"
	"the center of the toggle in our own coordinates (and not in those of child, that is not our child, but our sibling in the morphic hierarchy!!)"
	nextSibCenter _ anIndentingListItemMorph nextSibling location externalizePosition:
		anIndentingListItemMorph nextSibling toggleRectangle center.

	myCenter _ anIndentingListItemMorph location externalizePosition:
		 anIndentingListItemMorph toggleRectangle center.
	vLineX _ myCenter x - 1.
	hasToggle
		ifTrue: [ vLineTop _ myCenter y + 5 ]
		ifFalse: [ vLineTop _ myCenter y ].
	anIndentingListItemMorph nextSibling hasToggle
		ifTrue: [ vLineBottom _ nextSibCenter y - 7 ]
		ifFalse: [ vLineBottom _ nextSibCenter y ].
	"Draw line from me to next sibling"
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @ vLineBottom
		width: 1
		color: lineColor! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:48'!
draw: item atRow: row on: canvas
	"display the given item at row row"
	| drawBounds f |
	drawBounds _ self drawBoundsForRow: row.
	drawBounds _ drawBounds intersect: (0@0 extent: extent).
	f _ (item is: #Text) ifTrue: [ font emphasized: (item emphasisAt: 1) ] ifFalse: [ font ].
	canvas drawString: item in: drawBounds font: f color: (self colorForRow: row).! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:52'!
drawBackgroundForMulti: row on: aCanvas
	| selectionDrawBounds c |
	"shade the background darker, if this row is selected"
	selectionDrawBounds _ self drawBoundsForRow: row.
	selectionDrawBounds _ selectionDrawBounds intersect: (0@0 extent: extent).
	c _ (selectedRow notNil and: [ row = selectedRow])
		ifTrue: [ Theme current listHighlightFocused: owner hasKeyboardFocus ]
		ifFalse: [ Theme current listMultiHighlightFocused: owner hasKeyboardFocus ].
	aCanvas fillRectangle: selectionDrawBounds color: c! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:52'!
drawSelectionOn: aCanvas
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: selectedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: (0@0 extent: extent).
	aCanvas
		fillRectangle: selectionDrawBounds
		color: (Theme current listHighlightFocused: owner hasKeyboardFocus)! !


!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:55'!
debugDrawLineRectsOn: aCanvas
	"Shows where text line rectangles are"

	self paragraph lines do: [ :line |
		aCanvas
			frameRectangle: line rectangle
			borderWidth: 1
			color: Color brown ]
! !

!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:49'!
drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		paragraph: self paragraph
		bounds: (0@0 extent: extent)
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus)! !


!LayoutAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:53'!
drawOn: aCanvas

	aCanvas
		fillRectangle: (0@0 extent: extent)
		color: color
		borderWidth: 2
		borderStyleSymbol: #raised
		baseColorForBorder: color! !


!MagnifierMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:57'!
drawOn: aCanvas
	RecursionLock == self ifFalse: [
		super drawOn: aCanvas.		"border and fill"
		aCanvas isShadowDrawing ifFalse: [
			"Optimize because #magnifiedForm is expensive"
			aCanvas image: self magnifiedForm at: borderWidth@borderWidth]]! !


!MenuLineMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:52'!
drawOn: aCanvas 
	| baseColor r |
	baseColor _ owner color.
	r _ self morphBoundsInWorld.
	aCanvas
		fillRectangle: (r topLeft corner: r rightCenter)
		color: baseColor twiceDarker.
			
	aCanvas
		fillRectangle: (r leftCenter corner: r bottomRight)
		color: baseColor twiceLighter! !


!MenuMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:54'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: (0@0 extent: extent) color: color radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas fillRectangle: (0@0 extent: extent) color: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]! !


!MinimalStringMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:48'!
drawOn: aCanvas

	aCanvas drawString: contents in: (0@0 extent: extent) font: self fontToUse color: color! !


!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 9/10/2012 00:00'!
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
		line: x0@(bottom-halfW) to: x1@(top+halfW)
		width: w color: caretColor! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:48'!
drawOn: aCanvas
	self hasSelection ifTrue: [ self drawSelectionOn: aCanvas ].
	self hasVisibleCaret ifTrue: [ self drawCaretOn: aCanvas ].
	aCanvas
		drawString: contents
		in: (0@0 extent: extent)
		font: self fontToUse
		color: color! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:53'!
drawSelectionOn: aCanvas
	| rightX leftX bottom |

	bottom _ self baseFont height.
	leftX _ self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
	rightX _ self fontToUse widthOfString: contents from: 1 to: editor stopIndex-1.

	aCanvas
		fillRectangle: (leftX @ 0 corner: rightX @ bottom)
		color: (Theme current textHighlightFocused: self hasKeyboardFocus)! !


!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:57'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			"self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBounds
					during: [ :canvas | canvas image: backgroundImage multipliedBy: color at: bounds topLeft ]]
				ifFalse: [ aCanvas image: backgroundImage multipliedBy: color at: bounds topLeft ]"
			self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self zzclippingBounds
					during: [ :canvas | canvas image: backgroundImage at: 0@0 ]]
				ifFalse: [ aCanvas image: backgroundImage at: 0@0 ]]

		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [aCanvas drawsOnDisplay] and: [color class == TranslucentColor])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Squeak Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt current toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: 0@0 in: nil
						fillColor: color rule: Form over]
				ifFalse: [ super drawOn: aCanvas ]]! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:54'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: (0@0 extent: extent)
		color: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: c.

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:49'!
drawEmbossedLabelOn: aCanvas

	| availableW center colorForLabel f l labelMargin targetSize w x y |
	label ifNotNil: [
		colorForLabel _ Theme current buttonLabel.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel _ colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel _ colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f _ self fontToUse.
		center _ extent // 2.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin.
		availableW >= w
			ifTrue: [
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				targetSize _ label size * availableW // w.
				l _ label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize _ targetSize - 1.
					l _ label squeezedTo: targetSize ]].
		
		w _ f widthOfString: l.
		x _ center x - (w // 2).
		y _ center y - (f height // 2).
		aCanvas
			drawStringEmbossed: l
			in: (x@y extent: extent - (labelMargin*2-2@4))
			font: f
			color: colorForLabel ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:59'!
drawInconOn: aCanvas

	| theIcon |
	theIcon _ self magnifiedIcon.
	aCanvas
		image: theIcon
		multipliedBy: self iconColor
		at: (extent - theIcon extent //2)! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:48'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin |

	f _ self fontToUse.
	center _ extent // 2.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin - 1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f height // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			in: (x@y extent: extent - (labelMargin*2-2@4))
			font: f
			color: Theme current buttonLabel ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:50'!
drawRoundGradientLookOn: aCanvas
	| r colorForButton rect bottomFactor topFactor |

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
				rect _ (0@0 extent: extent) insetBy: 1@3.
				aCanvas
					roundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				rect _ (0@0 extent: extent) insetBy: 1@3.
				aCanvas roundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ]! !


!PluggableListMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:56'!
drawOn: aCanvas
	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: Preferences focusIndicatorWidth
			color: Theme current focusIndicator ].! !


!ProgressBarMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:53'!
drawOn: aCanvas

	super drawOn: aCanvas.
	aCanvas
		fillRectangle: (borderWidth@borderWidth extent: extent x - borderWidth - borderWidth * value @ extent y - borderWidth-borderWidth)
		color: progressColor! !


!RectangleIndicatorMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:56'!
drawOn: aCanvas
	| bw b |
	bw _ self defaultBorderWidth.
	b _ 0@0 extent: extent.
	aCanvas frameRectangle: b borderWidth: bw color: Color black.
	aCanvas frameRectangle: (b insetBy: bw) borderWidth: bw color: Color white! !


!ScrollBar methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:54'!
drawOn: aCanvas

	aCanvas
		fillRectangle: (0@0 extent: extent)
		color: (color alphaMixed: 0.3 with: Color white)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!String methodsFor: 'displaying' stamp: 'jmv 9/9/2012 23:45'!
displayOn: aDisplayMedium at: aPoint textColor: aColor
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, rendering the text in the designated color"

	aDisplayMedium getCanvas drawString: self at: aPoint font: nil color: aColor! !


!StringMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:48'!
drawOn: aCanvas
	aCanvas
		drawString: contents
		in: (0@0 extent: extent)
		font: self fontToUse
		color: color! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:57'!
drawOn: aCanvas

	| tRect sRect colorToUse sLeft aForm centeringOffset |
	isSelected ifTrue: [
		aCanvas
			fillRectangle: (0@0 extent: extent)
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].

	complexContents hasContents ifTrue: [
		tRect _ self toggleRectangle.
		aForm _ isExpanded 
			ifTrue: [ container expandedForm ]
			ifFalse: [ container notExpandedForm ].
		centeringOffset _ ((tRect height - aForm extent y) / 2.0) rounded.
		aCanvas 
			image: aForm 
			at: (tRect topLeft translatedBy: 0 @ centeringOffset) ].

	sLeft _ indentLevel * 12 + 16.
	sRect _ sLeft@0 extent: extent - (sLeft@0).
	colorToUse _ complexContents preferredColor ifNil: [ color ].
	aCanvas
		drawString: contents asString
		in: sRect
		font: self fontToUse
		color: colorToUse! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:57'!
drawOn: aCanvas 
	| stringColor stringBounds leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: (0@0 extent: extent) color: Theme current menuHighlight].
	leftEdge _ 0.

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + submorphs first morphWidth + 8 ].

	self hasIcon
		ifTrue: [| iconForm | 
			iconForm _ isEnabled ifTrue: [ self icon ] ifFalse: [ self icon asGrayScale ].
			aCanvas image: iconForm at: leftEdge+1 @ (extent y - iconForm height // 2).
			leftEdge _ leftEdge + iconForm width + self iconSeparation].

	stringBounds _  leftEdge @ 1 extent: extent.

	aCanvas
		drawString: contents
		in: stringBounds
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: SubMenuMarker
			at: extent x - 8 @ (extent y - SubMenuMarker height // 2) ]! !

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 9/9/2012 23:55'!
offImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.9) 
			borderWidth: 1 borderColor: Color black.
	^form! !

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 9/9/2012 23:55'!
onImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.8) 
			borderWidth: 1 borderColor: Color black;
		fillRectangle: (form boundingBox insetBy: 2) color: Color black.
	^form! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:54'!
drawClassicFrameOn: aCanvas color: titleColor
	"Window border encompasses title area. No round corners. No title gradient."

	aCanvas fillRectangle: (0@0 extent: extent) color: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self widgetsColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas fillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight) color: titleColor! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:49'!
drawLabelOn: aCanvas

	Theme current embossedTitles
		ifFalse: [
			aCanvas
				drawString: labelString
				in: self labelRectangle
				font: Preferences windowTitleFont
				color: Theme current windowLabel ]
		ifTrue: [
			aCanvas
				drawStringEmbossed: labelString
				in: self labelRectangleForEmbossed
				font: Preferences windowTitleFont
				color: Theme current windowLabel ]! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:51'!
drawRoundedFrameOn: aCanvas color: widgetsColor
	"Title area is not inside window borders"
	| bottomFactor topFactor |
	Theme current useWindowTitleGradient
		ifTrue: [
			topFactor _ Theme current titleGradientTopFactor.
			bottomFactor _ Theme current titleGradientBottomFactor ]
		ifFalse: [
			topFactor _ 1.
			bottomFactor _ 1 ].
	aCanvas
		windowFrame: (0@0 extent: extent)
		color: widgetsColor * Theme current titleGradientExtraLightness
		radius: Theme current roundedWindowRadius
		border: borderWidth
		labelHeight: self labelHeight + borderWidth
		gradientTop: topFactor
		gradientBottom: bottomFactor
		insideColor: color! !


!TextModelMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:56'!
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
		aCanvas frameRectangle: self focusIndicatorRectangle borderWidth: bw color: bc ]! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 9/9/2012 23:45'!
displayOn: aForm
	"
	Transcript displayOn: Display
	"
	| font count i string x y fh f bw r canvas |
	bw _ self borderWidth  .
	r _ innerRectangle outsetBy: bw + self padding.
	aForm border: r width: bw. 
	aForm fill: r fillColor: Color white.
	font _ StrikeFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.
	y _ innerRectangle top.
	f _ firstIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	i _ (lastIndex - count max: f) \\ self maxEntries + 1.
	canvas _ aForm getCanvas.
	canvas setOrigin: 0@0 clipRect: innerRectangle.
	[
		string _ entries at: i.	
		canvas drawString: string at: x@y font: font color: Color veryDarkGray.
		y _ y + fh.
		i = lastIndex
	] whileFalse: [ i _ i \\ self maxEntries + 1 ].

	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: Color veryDarkGray! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 9/9/2012 23:45'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas |
	font _ StrikeFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.

	string _ unfinishedEntry contents.
	y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerRectangle top.
	canvas _ aForm getCanvas.
	canvas setOrigin: 0@0 clipRect: innerRectangle.
	canvas drawString: string at: x@y font: font color: Color veryDarkGray! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:57'!
drawOn: aCanvas
	"
	Transcript
		showOnDisplay: true;
		bounds: bounds;
		displayOn: aCanvas form.
	"
	Transcript
		showOnDisplay: true;
		morphBoundsInWorld: (0@0 extent: self morphExtentInWorld);
		displayOn: form;
		morphBoundsInWorld: self morphBoundsInWorld.
	aCanvas image: form at: 0@0! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 9/9/2012 23:49'!
endEntry
	| c d cb |
	c _ self contents.
	Display extent ~= DisplayScreen actualScreenSize ifTrue: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame _ frame intersect: Display boundingBox.
		^ self clear; show: c].
	para
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	para composeAll.
	d _ para extent y - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ para characterBlockAtPoint:
			0@0 + (0@(d+StrikeFont default height)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		paragraph: para
		bounds: (4@4 + frame topLeft extent: Display extent)
		color: Color black
		selectionColor: Color blue! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:56'!
drawHand: aHandMorph
	"Draw a hand that carries morphs, or needs to be drawn by us, because of not being the hardware mouse pointer."
	| bw r |
	Preferences fastDragWindowForMorphic ifTrue: [
		bw _ HandMorph fastDragBorderWidth // 2.
		r _ aHandMorph morphFullBoundsInWorld.
		canvas frameRectangle: r borderWidth: bw color: Color black.
		canvas frameRectangle: (r insetBy: bw) borderWidth: bw color: Color white.
		canvas clipBy: aHandMorph morphBoundsInWorld during: [ :c | aHandMorph drawOn: c ]]
	ifFalse: [
		 canvas fullDraw: aHandMorph]! !

!methodRemoval: Paragraph #zzdisplayInsertionMarkAtX:top:bottom:emphasis:on:paragraphLeft:!
Paragraph removeSelector: #zzdisplayInsertionMarkAtX:top:bottom:emphasis:on:paragraphLeft:!
!methodRemoval: Paragraph #zzdisplaySelectionInLine:on:paragraphTopLeft:selectionColor:!
Paragraph removeSelector: #zzdisplaySelectionInLine:on:paragraphTopLeft:selectionColor:!
!methodRemoval: Paragraph #zzdisplaySelectionStartBlock:stopBlock:InLine:on:paragraphTopLeft:selectionColor:!
Paragraph removeSelector: #zzdisplaySelectionStartBlock:stopBlock:InLine:on:paragraphTopLeft:selectionColor:!
!methodRemoval: MorphicScanner #zzdisplayBulletIfAppropriateFor:paragraphTopLeft:!
MorphicScanner removeSelector: #zzdisplayBulletIfAppropriateFor:paragraphTopLeft:!
!methodRemoval: MorphicScanner #zzdisplayBulletParagraphTopLeft:number:!
MorphicScanner removeSelector: #zzdisplayBulletParagraphTopLeft:number:!
!methodRemoval: MorphicScanner #zzdisplayLine:paragraphTopLeft:leftInRun:!
MorphicScanner removeSelector: #zzdisplayLine:paragraphTopLeft:leftInRun:!
!methodRemoval: HandMorph #zzshadowForm!
HandMorph removeSelector: #zzshadowForm!
!methodRemoval: FormCanvas #zzdrawString:at:font:color:!
FormCanvas removeSelector: #zzdrawString:at:font:color:!
!methodRemoval: FormCanvas #zzdrawString:from:to:at:font:color:!
FormCanvas removeSelector: #zzdrawString:from:to:at:font:color:!
!methodRemoval: FormCanvas #zzdrawString:from:to:at:font:color:kern:!
FormCanvas removeSelector: #zzdrawString:from:to:at:font:color:kern:!
!methodRemoval: FormCanvas #zzdrawString:from:to:in:font:color:!
FormCanvas removeSelector: #zzdrawString:from:to:in:font:color:!
!methodRemoval: FormCanvas #zzdrawString:in:font:color:!
FormCanvas removeSelector: #zzdrawString:in:font:color:!
!methodRemoval: FormCanvas #zzdrawStringEmbossed:from:to:in:font:color:!
FormCanvas removeSelector: #zzdrawStringEmbossed:from:to:in:font:color:!
!methodRemoval: FormCanvas #zzdrawStringEmbossed:in:font:color:!
FormCanvas removeSelector: #zzdrawStringEmbossed:in:font:color:!
!methodRemoval: FormCanvas #zzfillOval:color:borderWidth:borderColor:!
FormCanvas removeSelector: #zzfillOval:color:borderWidth:borderColor:!
!methodRemoval: FormCanvas #zzfillRectangle:color:!
FormCanvas removeSelector: #zzfillRectangle:color:!
!methodRemoval: FormCanvas #zzfillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder:!
FormCanvas removeSelector: #zzfillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder:!
!methodRemoval: FormCanvas #zzfillRectangle:infiniteForm:multipliedBy:!
FormCanvas removeSelector: #zzfillRectangle:infiniteForm:multipliedBy:!
!methodRemoval: FormCanvas #zzframeAndFillRectangle:fillColor:borderWidth:borderColor:!
FormCanvas removeSelector: #zzframeAndFillRectangle:fillColor:borderWidth:borderColor:!
!methodRemoval: FormCanvas #zzframeRectangle:borderWidth:color:!
FormCanvas removeSelector: #zzframeRectangle:borderWidth:color:!
!methodRemoval: FormCanvas #zzframeRectangle:color:borderWidth:borderStyleSymbol:!
FormCanvas removeSelector: #zzframeRectangle:color:borderWidth:borderStyleSymbol:!
!methodRemoval: FormCanvas #zzimage:at:!
FormCanvas removeSelector: #zzimage:at:!
!methodRemoval: FormCanvas #zzimage:at:sourceRect:!
FormCanvas removeSelector: #zzimage:at:sourceRect:!
!methodRemoval: FormCanvas #zzimage:multipliedBy:at:!
FormCanvas removeSelector: #zzimage:multipliedBy:at:!
!methodRemoval: FormCanvas #zzline:to:width:color:!
FormCanvas removeSelector: #zzline:to:width:color:!
!methodRemoval: FormCanvas #zzparagraph:bounds:color:selectionColor:!
FormCanvas removeSelector: #zzparagraph:bounds:color:selectionColor:!
!methodRemoval: FormCanvas #zzroundRect:color:radius:!
FormCanvas removeSelector: #zzroundRect:color:radius:!
!methodRemoval: FormCanvas #zzroundRect:color:radius:gradientTop:gradientBottom:gradientHeight:!
FormCanvas removeSelector: #zzroundRect:color:radius:gradientTop:gradientBottom:gradientHeight:!
!methodRemoval: FormCanvas #zzroundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height:!
FormCanvas removeSelector: #zzroundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height:!
!methodRemoval: FormCanvas #zzstencil:at:color:!
FormCanvas removeSelector: #zzstencil:at:color:!
!methodRemoval: FormCanvas #zzstencil:at:sourceRect:color:!
FormCanvas removeSelector: #zzstencil:at:sourceRect:color:!
!methodRemoval: FormCanvas #zzwindowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:!
FormCanvas removeSelector: #zzwindowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:!
