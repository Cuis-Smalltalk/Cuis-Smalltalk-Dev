'From Cuis 5.0 [latest update: #4589] on 10 May 2021 at 12:33:22 pm'!

!MorphicCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 5/10/2021 10:54:20'!
ellipseCenter: center radius: radiusPointOrNumber borderWidth: mbw borderColor: mbc fillColor: morphFillColor
	self subclassResponsibility ! !


!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 5/10/2021 10:53:57'!
ellipseCenter: center radius: radiusPointOrNumber borderWidth: mbw borderColor: mbc fillColor: morphFillColor

	| displayRectangle doBorder doFill |

	engine ifNil: [ ^nil ].

	doBorder _ mbw > 0 and: [ mbc isTransparent not].
	doFill _ morphFillColor isTransparent not.
	doBorder | doFill ifTrue: [
		displayRectangle _ (currentTransformation externalizeRectangle: (Rectangle center: center extent: radiusPointOrNumber asPoint * 2)) rounded.
		"draw the border of the oval"
		doBorder ifTrue: [
			self setPaintColor: mbc.
			engine frameOval: displayRectangle borderWidth: mbw rounded].
		"fill the inside"
		doFill ifTrue: [
			self setPaintColor: morphFillColor.
			engine fillOval: (displayRectangle insetBy: mbw rounded) ]]! !


!MorphicCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 5/10/2021 12:32:42'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor
	"Prefer #ellipseCenter:radius:borderWidth:borderColor:fillColor:"

	self ellipseCenter: mcx@mcy radius: mrx@mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor! !

!methodRemoval: BitBltCanvas #ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor: stamp: 'jmv 5/10/2021 12:32:53'!
BitBltCanvas removeSelector: #ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:!
