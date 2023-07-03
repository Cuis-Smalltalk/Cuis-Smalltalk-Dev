'From Cuis 6.0 [latest update: #5882] on 3 July 2023 at 11:35:22 am'!

!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 7/3/2023 08:33:34'!
fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder
	"
	(BitBltCanvas onForm: Display)
		fillRectangle: (10@10 extent: 300@200) 
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised
		baseColorForBorder: Color red.
	Display forceToScreen.
	"

	borderWidth = 0 ifTrue: [
		^ self fillRectangle: aRectangle color: aColor ].
	self fillRectangle: (aRectangle insetBy: borderWidth) color: aColor.
	self frameRectangle: aRectangle color: baseColorForBorder borderWidth: borderWidth borderStyleSymbol: aSymbol.! !


!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 7/3/2023 08:25:12'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor

	| rect bw |
	r area = 0 ifTrue: [ ^self ].
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

