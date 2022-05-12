'From Cuis 6.0 [latest update: #5141] on 9 May 2022 at 6:14:00 pm'!

!Fraction commentStamp: 'len 5/9/2022 18:06:15' prior: 0!
Fraction provides methods for dealing with rational numbers like 1/3 as true fractions (not approximations as a Float 0.33333...). All public arithmetic operations answer reduced fractions, or Integers when the denominator is 1.

Examples: (note the parentheses required to get the right answers in Smalltalk):

(2/3) + (2/3)
(6/4)		 		"Fractions are reduced to the smallest numerator and denominator possible"
(1 / -3)			"Denominator is kept positive, the sign is always in the numerator"
(2/3) + (1/3)	"When the denominator reduces to 1, the answer is an Integer"
!


!BitBltCanvas commentStamp: '<historical>' prior: 0!
origin is aPoint, and it is expressed relative to the form. Usually, the form doesn't cover the whole World, and origin is negative. For instance, if we just want to draw the part of the World in (100@100 extent: 50@50) to another form, then fom extent = 50@50, and origin = -100@ -100.

clipRect is relative to the form. For instance, if we only need to draw the part in (110@110 extent: 20@20) to the form above, then clipRect is (10@10 extent: 20@20)

All public protocol and drawing services is relative to World. The idea is that we only care about origin/form when we set them. Afterwards, we don't need to care about them. All the operations are done as if the whole World was drawn on Display.!


!Form methodsFor: 'transitions' stamp: 'len 5/9/2022 17:58:14'!
slideImage: otherImage at: topLeft delta: delta
	"
	Display slideImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse
		at: 40@40 delta: 3@ -4
	"
	| bb nSteps clipRect |
	bb _ otherImage boundingBox.
	clipRect _ topLeft extent: otherImage extent.
	nSteps _ 1.
	delta x = 0 ifFalse: [nSteps _ nSteps max: (bb width//delta x abs) + 1].
	delta y = 0 ifFalse: [nSteps _ nSteps max: (bb height//delta y abs) + 1].
	1 to: nSteps do: [ :i | 
			self 
				copyBits: bb from: otherImage
				at: delta*(i-nSteps) + topLeft
				clippingBox: clipRect rule: Form paint.
			Display forceToScreen.
		]! !

!Form methodsFor: 'transitions' stamp: 'len 5/9/2022 17:58:26'!
wipeImage: otherImage at: topLeft delta: delta
	"Display wipeImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse
		at: 40@40 delta: 0@ -2"

	self wipeImage: otherImage at: topLeft delta: delta clippingBox: nil.
! !


!Form class methodsFor: 'examples' stamp: 'len 5/9/2022 17:58:20'!
toothpaste: diam	
	"
	Display restoreAfter: [Form toothpaste: 30]
	"
	"Not completely unlike the ST-80 implementation :)
	Original comment:
	Draws wormlike lines by laying down images of spheres.
	See Ken Knowlton, Computer Graphics, vol. 15 no. 4 p352.
	Draw with mouse button down; terminate by option-click."
	| point queue color q colors colr colr2 |
	colors _ `Color red wheel: 12`.
	color _ 8.
	[ true ] whileTrue: [
		queue _ OrderedCollection new: 32.
		16 timesRepeat: [queue addLast: `-40@ -40`].
		Sensor waitButton.
		Sensor isMouseButton2Pressed ifTrue: [^ self].
		point _ Sensor mousePoint.
		colr _ colors atWrap: (color _ color + 5).  "choose increment relatively prime to colors size"
		colr2 _ colr alpha: 0.3.
		[Sensor isMouseButton1Pressed or: [queue size > 0]] whileTrue: [
			point _ point * 4 + Sensor mousePoint // 5.
			point _ point.
			Form fakeSphereAt: point diameter: diam color: colr.
			(q _ queue removeFirst) ifNil: [^ self].	"exit"
			Form fakeSphereAt: q diameter: diam color: colr2.
			Sensor isMouseButton1Pressed ifTrue: [queue addLast: point]]]! !

!Form class methodsFor: 'examples' stamp: 'len 5/9/2022 17:58:33'!
xorHack: size  "Display restoreAfter: [Form xorHack: 256]"
	"Draw a smiley face or stick figure, and end with option-click.
	Thereafter image gets 'processed' as long as you have button down.
	If you stop at just the right time, you'll see you figure upside down,
	and at the end of a full cycle, you'll see it perfectly restored.
	Dude -- this works in color too!!"
	| rect form i bb |
	rect _ 5@5 extent: size@size.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	Display border: (rect topRight - (0@2) extent: rect extent*2 + 4) width: 2.
	Form exampleSketch.
	form _ Form fromDisplay: rect.
	bb _ form boundingBox.
	i _ 0.
	[Sensor isMouseButton2Pressed] whileFalse:
		[[Sensor isMouseButton1Pressed] whileTrue:
			[i _ i + 1.
			(Array with: 0@1 with: 0@ -1 with: 1@0 with: -1@0) do:
				[:d | form copyBits: bb from: form at: d
					clippingBox: bb rule: Form reverse ].
			form displayAt: rect topLeft.
			i+2\\size < 4 ifTrue: [(Delay forMilliseconds: 300) wait]].
		(form magnify: form boundingBox by: 2@2) displayAt: rect topRight + (2@0).
		Sensor waitButton].! !


!Cursor class methodsFor: 'instance creation' stamp: 'len 5/9/2022 17:58:38'!
bottomLeftCursor
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1111111111111100
		2r1111111111111100
		2r0000000000000000
		2r0000000000000000)
	offset: `0@ -16`! !

!Cursor class methodsFor: 'instance creation' stamp: 'len 5/9/2022 17:58:43'!
bottomRightCursor
	^  Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r1111111111111100
		2r1111111111111100
		2r0000000000000000
		2r0000000000000000)
	offset: `-16@ -16`! !

!Cursor class methodsFor: 'instance creation' stamp: 'len 5/9/2022 17:58:46'!
cornerCursor
	"Answer the instance of me that is the shape of the bottom right corner 
	of a rectangle."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r1111111111111100
		2r1111111111111100
		2r0000000000000000
		2r0000000000000000)
	offset: `-16@ -16`! !

!Cursor class methodsFor: 'instance creation' stamp: 'len 5/9/2022 17:58:50'!
crossHairCursor
	"Answer the instance of me that is the shape of a cross."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0111111111111100
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000000000000
		2r0000000000000000)
	offset: `-7@ -7`! !

!Cursor class methodsFor: 'instance creation' stamp: 'len 5/9/2022 17:58:53'!
normalCursorWithMask
	"Cursor initNormalWithMask.  Cursor normal show"
	"Next two lines work simply for any cursor..."
	"
	self initNormal.
	NormalCursor _ CursorWithMask derivedFrom: NormalCursor.
	"
	"But for a good looking cursor, you have to tweak things..."
	^ ((CursorWithMask
		small1BitExtent: `16@16`
		pixelBits: #(
			2r0000000000000000
			2r0100000000000000
			2r0110000000000000
			2r0111000000000000
			2r0111100000000000
			2r0111110000000000
			2r0111111000000000
			2r0111111100000000
			2r0111111110000000
			2r0111110000000000
			2r0110110000000000
			2r0100011000000000
			2r0000011000000000
			2r0000001100000000
			2r0000001100000000
			2r0000000000000000)
		offset: `-1@ -1`)
	setMaskForm: 
		(Form
			small1BitExtent: `16@16`
			pixelBits: #(
				2r1100000000000000
				2r1110000000000000
				2r1111000000000000
				2r1111100000000000
				2r1111110000000000
				2r1111111000000000
				2r1111111100000000
				2r1111111110000000
				2r1111111111000000
				2r1111111111100000
				2r1111111000000000
				2r1110111100000000
				2r1100111100000000
				2r1000011110000000
				2r0000011110000000
				2r0000001110000000)
			offset: `0@0`))! !

!Cursor class methodsFor: 'instance creation' stamp: 'len 5/9/2022 18:03:20'!
resizeLeftCursor
	^ `(Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0000010010000000
		2r0000010010000000
		2r0000010010000000
		2r0001010010100000
		2r0011010010110000
		2r0111010010111000
		2r1111110011111100
		2r0111010010111000
		2r0011010010110000
		2r0001010010100000
		2r0000010010000000
		2r0000010010000000
		2r0000010010000000
		2r0000000000000000
		2r0000000000000000)
	offset: `-7@ -7`) withMask`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'len 5/9/2022 18:03:24'!
resizeTopCursor
	^ `(Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000100000000
		2r0000001110000000
		2r0000011111000000
		2r0000111111100000
		2r0000000100000000
		2r0111111111111100
		2r0000000000000000
		2r0000000000000000
		2r0111111111111100
		2r0000000100000000
		2r0000111111100000
		2r0000011111000000
		2r0000001110000000
		2r0000000100000000
		2r0000000000000000
		2r0000000100000000)
	offset: `-7@ -7`) withMask`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'len 5/9/2022 18:03:27'!
resizeTopLeftCursor
	^ `(Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0111110000010000
		2r0111100000100000
		2r0111000001000100
		2r0110100010001000
		2r0100010100010000
		2r0000001000100000
		2r0000010001000000
		2r0000100010000000
		2r0001000101000100
		2r0010001000101100
		2r0000010000011100
		2r0000100000111100
		2r0000000001111100
		2r0000000000000000
		2r0000000000000000)
	offset: `-7@ -7`) withMask`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'len 5/9/2022 18:03:31'!
resizeTopRightCursor
	^ `(Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0001000001111100
		2r0000100000111100
		2r0100010000011100
		2r0010001000101100
		2r0001000101000100
		2r0000100010000000
		2r0000010001000000
		2r0000001000100000
		2r0100010100010000
		2r0110100010001000
		2r0111000001000000
		2r0111100000100000
		2r0111110000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `-7@ -7`) withMask`.! !

!Cursor class methodsFor: 'instance creation' stamp: 'len 5/9/2022 18:03:35'!
squareCursor
	"Answer the instance of me that is the shape of a square."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000001111000000
		2r0000001111000000
		2r0000001111000000
		2r0000001111000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `-8@ -8`! !

!Cursor class methodsFor: 'instance creation' stamp: 'len 5/9/2022 18:03:38'!
targetCursor
	"Answer the instance of me that is the shape of a gunsight."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000011111000000
		2r0001100100110000
		2r0010000100001000
		2r0100000100000100
		2r0100001110000100
		2r1000000100000010
		2r1000100100100010
		2r1111111011111110
		2r1000100100100010
		2r1000000100000010
		2r0100001110000100
		2r0100000100000100
		2r0010000100001000
		2r0001100100110000
		2r0000011111000000
		2r0000000000000000)
	offset: `-7@ -7`! !


!CursorWithAlpha class methodsFor: 'constants' stamp: 'len 5/9/2022 18:03:43'!
biggerNormal
	"
	CursorWithAlpha biggerNormal show
	"

	^self constants at: #biggerNormal ifAbsentPut: [
		| form cursor packName pack |
		packName _ #GUIFormsPack.
		pack _ (Smalltalk at: packName) new.
		form _ (pack decodeContentFrom: pack contentMap) at: 'biggerNormalCursor'.
		cursor _ CursorWithAlpha extent: form extent depth: 32.
		form displayOn: cursor.
		cursor offset: -2@ -1.
		cursor preMultiplyAlpha.
		cursor ]! !


!Point methodsFor: 'point functions' stamp: 'len 5/9/2022 18:03:53'!
eightNeighbors
	^ (Array with: self + `1@0`
		with: self + `1@1`
		with: self + `0@1`
		with: self + `-1@1`) ,
	(Array with: self + `-1@0`
		with: self + `-1@ -1`
		with: self + `0@ -1`
		with: self + `1@ -1`)! !

!Point methodsFor: 'point functions' stamp: 'len 5/9/2022 18:03:57'!
fourNeighbors
	^ Array with: self + `1@0`
		with: self + `0@1`
		with: self + `-1@0`
		with: self + `0@ -1`! !


!Morph methodsFor: 'drawing' stamp: 'len 5/9/2022 18:04:09'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: `-75 @ -70 corner: 75 @ 70`
		color: `Color blue`! !

