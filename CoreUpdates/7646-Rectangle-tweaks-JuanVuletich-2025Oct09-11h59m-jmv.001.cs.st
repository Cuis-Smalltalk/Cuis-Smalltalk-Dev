'From Cuis7.5 [latest update: #7646] on 9 October 2025 at 12:00:44 pm'!

!BitBltCanvasEngine methodsFor: 'drawing support' stamp: 'jmv 10/9/2025 11:59:33'!
fillRect: rect

	rect hasPositiveExtent ifFalse: [^ self].
	destX := rect left.
	destY := rect top.
	sourceX := 0.
	sourceY := 0.
	width := rect width.
	height := rect height.
	self copyBits! !

!BitBltCanvasEngine methodsFor: 'drawing support' stamp: 'jmv 10/9/2025 11:59:37'!
frameRect: rect borderWidth: borderWidth
	"Paint a border whose rectangular area is defined by rect. The
	width of the border of each side is borderWidth."

	rect hasPositiveExtent ifFalse: [^ self].

	sourceX := 0.
	sourceY := 0.
	
	"for top and bottom, the following are the same"
	height := borderWidth. 
	width := rect width. 
	destX := rect left.

	"top"
	destY := rect top.
	self copyBits.

	"bottom"
	destY := rect bottom - borderWidth.
	self copyBits.

	"for left & right, the following are the same"
	height := rect height-borderWidth-borderWidth.
	width := borderWidth. 
	destY := rect top+borderWidth.

	"left"
	destX := rect left.
	self copyBits.

	"right"
	destX := rect right - borderWidth.
	self copyBits! !


!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 10/9/2025 12:00:00'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor

	| rect bw |
	r hasPositiveExtent ifFalse: [^ self].
	rect := (currentTransformation externalBoundingRectOf: r) roundedHAFZ.
	bw := (currentTransformation externalizeScalar: borderWidth) roundedHAFZ.

	"draw the border of the rectangle"
	(bw = 0 or: [borderColor isTransparent]) ifFalse: [
		self setPaintColor: borderColor.
		engine frameRect: rect borderWidth: bw ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		engine fillRect: (rect insetBy: bw) ]! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 10/9/2025 11:59:40'!
frameRectangle: rect borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor
	"rect and borderWidth are in targetForm coordinates. No transformation is done."
	"
	(BitBltCanvas onForm: Display)
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		topLeftColor: Color green
		bottomRightColor: Color red.
	(BitBltCanvas onForm: Display) fillRectangle: (100@100 extent: 300@200) color: Color white.
	Display forceToScreen.
	"
	| w h |

	rect hasPositiveExtent ifFalse: [^ self].

	self setPaintColor: topLeftColor.

	engine frameRectTopLeft: rect borderWidth: borderWidth.

	borderWidth isNumber
		ifTrue: [w := h := borderWidth]
		ifFalse: [w := borderWidth x.   h := borderWidth y].
	self setPaintColor: bottomRightColor.
	engine 
		 frameRectRight: rect width: w;
		 frameRectBottom: rect height: h! !


!HybridCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 10/9/2025 12:00:16'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	| rect |

	drawingOnMorphIDs ifTrue: [
		aRectangle hasPositiveExtent ifFalse: [^ self].
		rect := (currentTransformation externalBoundingRectOf: aRectangle) roundedHAFZ.
		engine combinationRule: 0.
		engine fillRect: rect.
		^self ].

	super fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor! !

