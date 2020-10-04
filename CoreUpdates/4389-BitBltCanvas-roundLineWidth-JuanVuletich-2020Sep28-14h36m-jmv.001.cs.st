'From Cuis 5.0 [latest update: #4388] on 28 September 2020 at 3:59:52 pm'!

!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 9/28/2020 15:57:04'!
line: pt1 to: pt2 width: wp color: c
	|  p1 p2 w |

	engine ifNil: [ ^nil ].

	(wp > 0 and: [ c isTransparent not ]) ifTrue: [
		p1 _ (currentTransformation transform: pt1) rounded.
		p2 _ (currentTransformation transform: pt2) rounded.
		w _ (currentTransformation externalizeScalar: wp) rounded.
		self setPaintColor: c.
		engine
			width: w;
			height: w;
			drawFrom: p1 to: p2 ]! !

!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 9/28/2020 15:58:00'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor

	| displayRectangle doBorder doFill |

	engine ifNil: [ ^nil ].

	doBorder _ mbw > 0 and: [ mbc isTransparent not].
	doFill _ morphFillColor isTransparent not.
	doBorder | doFill ifTrue: [
		displayRectangle _ (currentTransformation externalizeRectangle: (Rectangle center: mcx@mcy extent: mrx@mry * 2)) rounded.
		"draw the border of the oval"
		doBorder ifTrue: [
			self setPaintColor: mbc.
			engine frameOval: displayRectangle borderWidth: mbw rounded].
		"fill the inside"
		doFill ifTrue: [
			self setPaintColor: morphFillColor.
			engine fillOval: (displayRectangle insetBy: mbw rounded) ]]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/28/2020 15:58:21'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect bw |


	engine ifNil: [ ^nil ].

	r area = 0 ifTrue: [ ^self ].
	rect _ (currentTransformation externalizeRectangle: r) rounded.
	bw _ (currentTransformation externalizeScalar: borderWidth) rounded.

	"draw the border of the rectangle"
	borderColor isTransparent ifFalse: [
		self setPaintColor: borderColor.
		engine frameRect: rect borderWidth: bw ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		engine fillRect: (rect insetBy: bw) ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/28/2020 15:58:33'!
frameRectangle: r borderWidth: borderWidth color: borderColor
	"
	Display getCanvas
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

