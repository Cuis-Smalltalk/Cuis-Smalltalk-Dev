'From Cuis 4.2 of 25 July 2013 [latest update: #2107] on 22 October 2014 at 11:13:58.451429 pm'!

!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 10/22/2014 23:05'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor

	| displayRectangle |
	displayRectangle _ (currentTransformation displayBoundsOfTransformOf: (Rectangle center: mcx@mcy extent: mrx@mry * 2)) truncated.
	"draw the border of the oval"
	(mbw = 0 or: [mbc isTransparent]) ifFalse:[
		self setPaintColor: mbc.
		port frameOval: displayRectangle borderWidth: mbw].
	"fill the inside"
	morphFillColor isTransparent ifFalse: [
		self setPaintColor: morphFillColor.
		port fillOval: (displayRectangle insetBy: mbw)].
! !

