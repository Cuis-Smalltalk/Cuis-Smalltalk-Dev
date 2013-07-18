'From Cuis 4.1 of 12 December 2012 [latest update: #1630] on 20 March 2013 at 12:14:25 am'!

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 3/20/2013 00:12'!
fillRectangle: aRectangle infiniteForm: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of anInfiniteForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	Display getCanvas fillRectangle: (10@10 extent: 100@100) infiniteForm: (SystemWindow titleGradient: 12) multipliedBy: Color red.
	"

	| displayRectangle |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	AccessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		self fillRectangle: displayRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]! !

!methodRemoval: ImageMorph #form!
ImageMorph removeSelector: #form!
!methodRemoval: Form #form!
Form removeSelector: #form!
!classRemoval: #InfiniteForm!
Smalltalk removeClassNamed: #InfiniteForm!
