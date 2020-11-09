'From Cuis 5.0 [latest update: #4443] on 9 November 2020 at 3:30:17 pm'!

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/9/2020 15:25:35'!
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

	engine ifNil: [ ^nil ].

	bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	displayRectangle _ (currentTransformation externalizeRectangle: aRectangle) rounded.
	self
		frameRectangle: displayRectangle
		borderWidth: bw
		topLeftColor: tlColor
		bottomRightColor: brColor.! !


!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/9/2020 15:28:39'!
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

	self fillRectangle: (aRectangle insetBy: borderWidth) color: aColor.
	self frameRectangle: aRectangle color: baseColorForBorder borderWidth: borderWidth borderStyleSymbol: aSymbol! !

!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/9/2020 15:26:49'!
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

	engine ifNil: [ ^nil ].

	tlColor _ aColor.
	brColor _ aColor.
	aSymbol == #raised ifTrue: [
		tlColor _ aColor quiteWhiter.
		brColor _ aColor quiteBlacker ].

	aSymbol == #inset ifTrue: [
		tlColor _ aColor quiteBlacker.
		brColor _ aColor quiteWhiter ].

	self frameRectangle: aRectangle topLeftColor: tlColor bottomRightColor: brColor borderWidth: borderWidth! !

!methodRemoval: BitBltCanvas #fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: stamp: 'jmv 11/9/2020 15:28:46'!
BitBltCanvas removeSelector: #fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder:!
!methodRemoval: BitBltCanvas #frameRectangle:color:borderWidth:borderStyleSymbol: stamp: 'jmv 11/9/2020 15:27:03'!
BitBltCanvas removeSelector: #frameRectangle:color:borderWidth:borderStyleSymbol:!
