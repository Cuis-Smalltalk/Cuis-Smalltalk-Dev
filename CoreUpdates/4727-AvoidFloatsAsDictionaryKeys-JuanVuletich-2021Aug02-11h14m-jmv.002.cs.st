'From Cuis 5.0 [latest update: #4726] on 2 August 2021 at 11:32:01 am'!

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 8/2/2021 11:19:09'!
image: aForm multipliedBy: aColor at: aPoint
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	(BitBltCanvas onForm: Display) image: ((Form dotOfSize: 50) asFormOfDepth: 32) multipliedBy: Color red at: 20@20. Display forceToScreen
	"
	aColor isTransparent ifFalse: [
		self class accessProtect critical: [
			self buildAuxWith: aForm multipliedWith: aColor.
			self image: AuxForm at: aPoint sourceRect: aForm boundingBox ]]! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 8/2/2021 11:19:23'!
stencil: stencilForm at: aPoint color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	"
	| dot |
	dot := ((Form dotOfSize: 32) asFormOfDepth: 1) offset: 0@0.
	dot displayAt: 20@20.
	(BitBltCanvas onForm: Display) stencil: dot at: 60@20 color: Color red.
	Display forceToScreen
	"
	^self stencil: stencilForm
		at: aPoint
		sourceRect: stencilForm boundingBox
		color: aColor! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/2/2021 11:16:46'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	(BitBltCanvas onForm: Display) fillRectangle: (10@10 extent: 100@100) tilingWith: (BitBltCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red. Display forceToScreen.
	"


	engine ifNil: [ ^nil ].

	self class accessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		"Warning: aForm boundingBox is most likely different from AuxForm boundingBox!!"
		self fillRectangle: aRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/2/2021 11:16:59'!
frameRectangle: r borderWidth: borderWidth color: borderColor
	"
	(BitBltCanvas onForm: Display)
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| rect bw |

	engine ifNil: [ ^nil ].

	(borderWidth > 0 and: [ borderColor isTransparent not ]) ifTrue: [
		rect _ (currentTransformation externalizeRectangle: r) rounded.
		bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
		self setPaintColor: borderColor.
		engine
			frameRect: rect
			borderWidth: bw ]! !

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 8/2/2021 11:15:28'!
roundRect: aRectangle color: aColor radius: r
	"
	(BitBltCanvas onForm: Display) roundRect: (10@10 extent: 200@100) color: Color red radius: 10.5. Display forceToScreen.
	"

	engine ifNil: [ ^nil ].

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

!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 8/2/2021 11:20:54'!
windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	BitBltCanvas releaseClassCachedState; releaseClassState.
	(BitBltCanvas onForm: Display) windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green.
	Display forceToScreen
	"
	"top stripe"
	| bottomColor he tl tr |

	engine ifNil: [ ^nil ].

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

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 8/2/2021 11:18:01'!
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

	rect area = 0 ifTrue: [^ self].

	self setPaintColor: topLeftColor.

	engine frameRectTopLeft: rect borderWidth: borderWidth.

	borderWidth isNumber
		ifTrue: [w _ h _ borderWidth]
		ifFalse: [w _ borderWidth x.   h _ borderWidth y].
	self setPaintColor: bottomRightColor.
	engine 
		 frameRectRight: rect width: w;
		 frameRectBottom: rect height: h! !


!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 8/2/2021 11:29:15'!
bottomLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^self cachedForms
		at: { #bottomLeft. r asInteger. height asInteger. (gradientTopFactor*100) asInteger. (gradientBottomFactor*100) asInteger}
		ifAbsentPut: [
			Form
				bottomLeftCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 8/2/2021 11:28:51'!
bottomLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor borderWidth: bw
	^self cachedForms
		at: { #bottomLeft. r asInteger. height asInteger. (gradientTopFactor*100) asInteger. (gradientBottomFactor*100) asInteger. bw asInteger}
		ifAbsentPut: [
			Form
				bottomLeftCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor
				borderWidth: bw ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 8/2/2021 11:28:58'!
bottomRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^self cachedForms
		at: { #bottomRight. r asInteger. height asInteger. (gradientTopFactor*100) asInteger. (gradientBottomFactor*100) asInteger}
		ifAbsentPut: [
			Form
				bottomRightCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 8/2/2021 11:29:31'!
bottomRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor borderWidth: bw
	^self cachedForms
		at: { #bottomRight. r asInteger. height asInteger. (gradientTopFactor*100) asInteger. (gradientBottomFactor*100) asInteger. bw asInteger}
		ifAbsentPut: [
			Form
				bottomRightCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor
				borderWidth: bw ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 8/2/2021 11:29:37'!
topLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^self cachedForms
		at: { #topLeft. r asInteger. height asInteger. (gradientTopFactor*100) asInteger. (gradientBottomFactor*100) asInteger}
		ifAbsentPut: [
			Form
				topLeftCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 8/2/2021 11:29:41'!
topRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^self cachedForms
		at: { #topRight. r asInteger. height asInteger. (gradientTopFactor*100) asInteger. (gradientBottomFactor*100) asInteger}
		ifAbsentPut: [
			Form
				topRightCorner: r
				height: height
				gradientTop: gradientTopFactor
				gradientBottom: gradientBottomFactor ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 8/2/2021 11:29:59'!
verticalGrayGradient: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^self cachedForms
		at: { #vertical. height asInteger. (gradientTopFactor*100) asInteger. (gradientBottomFactor*100) asInteger}
		ifAbsentPut: [
			Form
				verticalGradient: height
				topColor: (Color gray: gradientTopFactor)
				bottomColor: (Color gray: gradientBottomFactor) ]! !

!BitBltCanvas class methodsFor: 'cached button forms' stamp: 'jmv 8/2/2021 11:26:03'!
arrowOfDirection: aSymbol size: finalSizeInteger
	^self cachedForms
		at: { aSymbol . finalSizeInteger asInteger}
		ifAbsentPut: [
			self buildArrowOfDirection: aSymbol size: finalSizeInteger ]! !

!BitBltCanvas class methodsFor: 'cached button forms' stamp: 'jmv 8/2/2021 11:31:13'!
windowButtonIcon: aSymbol size: finalSizeInteger
	^self cachedForms
		at: { aSymbol . finalSizeInteger asInteger}
		ifAbsentPut: [
			| icon w h factor magnifiedExtent magnifiedIcon |
			icon _ Theme current perform: aSymbol.	
			magnifiedIcon _ icon.
			w _ icon width.
			h _ icon height.
			w*h = 0 ifFalse: [
				factor _ 1.0 * finalSizeInteger / w min: 1.0 * finalSizeInteger / h.
				factor = 1.0 ifFalse: [
					magnifiedExtent _ (icon extent * factor) rounded.
					magnifiedIcon _ icon magnifyTo: magnifiedExtent ]].
			magnifiedIcon ]! !

!methodRemoval: BitBltCanvas class #buildArrowWith:borderForm: stamp: 'jmv 8/2/2021 11:16:24'!
BitBltCanvas class removeSelector: #buildArrowWith:borderForm:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
BitBltCanvas releaseClassCachedState; releaseClassState.!

