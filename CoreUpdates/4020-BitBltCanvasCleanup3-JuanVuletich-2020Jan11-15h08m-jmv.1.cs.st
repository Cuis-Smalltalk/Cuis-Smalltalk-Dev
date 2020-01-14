'From Cuis 5.0 [latest update: #4017] on 11 January 2020 at 3:14:31 pm'!
Smalltalk renameClassNamed: #GrafPort as: #BitBltCanvasEngine!
!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'form clipLeft clipTop clipRight clipBottom transformations currentTransformation cti currentMorph drawingMorphStack engine '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!AbstractFont methodsFor: 'BitBltCanvas support' stamp: 'jmv 1/11/2020 15:11:46'!
onBitBltCanvasEngine: engine displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| char fallbackFont x y answer |
	fallbackFont _ (FontFamily
		familyName: FontFamily defaultFamilyName
		aroundPointSize: self pointSize*0.9)
			emphasized: self emphasis.
	x _ p x.
	y _ p y.
	firstIndex to: lastIndex do: [ :i |
		char _ aString at: i.
		answer _ engine displayString: char asString from: 1 to: 1 at: x@y strikeFont: fallbackFont color: color.
		x _ x + (self widthOf: char) ].
	^answer! !


!StrikeFont methodsFor: 'BitBltCanvas support' stamp: 'jmv 1/11/2020 15:11:58'!
onBitBltCanvasEngine: engine displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position.
	Answer nil if nothing was done."

	^engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p
		strikeFont: self
		color: color! !


!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/11/2020 15:09:59'!
resetEngine
	"Private!! Create a new BitBltCanvasEngine for a new copy."

	engine _ BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	port _ engine! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 1/11/2020 15:08:13'!
fakeSphereAt: aPoint diameter: diameter color: aColor
	"Create a form which contains a round black dot."
	| bb rect d p |
	bb _ BitBltCanvasEngine toForm: Display.
	bb fillColor: aColor.
	bb combinationRule: Form blend.
	rect _ aPoint extent: diameter.
	bb fillOval: rect.
	p _ (diameter * 2 // 5) asPoint + aPoint.
	#(2 3 4 5 6 ) do: [ :i |
		"simulate facade by circles of gray"
		bb fillColor: `Color white alpha: 0.3`.
		d _ i * diameter // 10.
		rect _ Rectangle
			center: p
			extent: d.
		bb fillOval: rect ].
	DisplayScreen screenUpdateRequired: nil.
"
	Form fakeSphereAt: 50@50 diameter: 30 color:  (Color red alpha: 0.5)
"! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 1/11/2020 15:12:09'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| p1 font |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ (currentTransformation transform: aPoint) rounded.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ AbstractFont default ].
	^font
		onBitBltCanvasEngine: port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !

!BitBltCanvas methodsFor: 'initialization' stamp: 'jmv 1/11/2020 15:10:03'!
initializeWith: aForm origin: aPoint

	super initializeWith: aForm origin: aPoint.
	self resetEngine! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/11/2020 15:10:07'!
setForm: aForm
	super setForm: aForm.
	self resetEngine.
	self newClipRect: nil.! !

!methodRemoval: BitBltCanvas #resetGrafPort stamp: 'jmv 1/11/2020 15:10:09'!
BitBltCanvas removeSelector: #resetGrafPort!
!methodRemoval: StrikeFont #on:displayString:from:to:at:color: stamp: 'jmv 1/11/2020 15:12:23'!
StrikeFont removeSelector: #on:displayString:from:to:at:color:!
!methodRemoval: AbstractFont #on:displayString:from:to:at:color: stamp: 'jmv 1/11/2020 15:12:22'!
AbstractFont removeSelector: #on:displayString:from:to:at:color:!

!Form class reorganize!
('instance creation' dotOfSize: extent: extent:depth: extent:depth:bits: extent:depth:fromArray:offset: extent:fromArray:offset: extent:nativeDepthFrom: extent:nativeDepthFrom:bits: extent:offset: fakeSphereAt:diameter:color: fromBinaryStream: fromDisplay: fromFileEntry: fromFileNamed: fromFileNamedOrNil: fromUser fromUserWithExtent: verticalGradient:topColor:bottomColor:)
('mode constants' and blend blendAlpha erase erase1bitShape oldErase1bitShape oldPaint over paint paintAlpha reverse rgbMul under)
('examples' toothpaste: xorHack:)
('creation - anti aliased' bottomLeftCorner:height:gradientTop:gradientBottom: bottomLeftCorner:height:gradientTop:gradientBottom:borderWidth: bottomRightCorner:height:gradientTop:gradientBottom: bottomRightCorner:height:gradientTop:gradientBottom:borderWidth: topLeftCorner:height:gradientTop:gradientBottom: topRightCorner:height:gradientTop:gradientBottom:)
!

!BitBltCanvas reorganize!
('drawing' line:to:width:color:)
('drawing-icons' drawButtonIconFromCurrentMorph)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol: reverseRectangleBorder:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:from:to:at:font:color:)
('initialization' initializeWith:origin:)
('private' buildAuxWith:multipliedWith: fillRectangle:tilingWith:sourceRect:rule: frameRectangle:borderWidth:topLeftColor:bottomRightColor: resetEngine setClipRect: setForm: setPaintColor:)
('morphic' clippingRectForCurrentMorph)
('testing' isCurrentMorphVisible)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
BitBltCanvas allInstancesDo: [ :canvas | canvas resetEngine ]!

