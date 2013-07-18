'From Cuis 4.1 of 12 December 2012 [latest update: #1639] on 22 March 2013 at 8:54:32 am'!

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 3/20/2013 23:15'!
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
	port colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	port image: aForm at: p rounded sourceRect: sourceRect rule: r.! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 3/20/2013 23:11'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	Display getCanvas fillRectangle: (10@10 extent: 100@100) tilingWith: (FormCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red.
	"

	| displayRectangle |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	AccessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		"Warning: aForm boundingBox is most likely different from AuxForm boundingBox!!"
		self fillRectangle: displayRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]! !

!methodRemoval: FormCanvas #image:at:sourceRect:rule:!
FormCanvas removeSelector: #image:at:sourceRect:rule:!
