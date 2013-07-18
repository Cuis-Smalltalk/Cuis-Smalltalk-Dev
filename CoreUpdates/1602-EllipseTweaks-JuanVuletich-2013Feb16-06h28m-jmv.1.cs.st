'From Cuis 4.1 of 12 December 2012 [latest update: #1600] on 16 February 2013 at 6:30:19 am'!

!EllipseMorph commentStamp: '<historical>' prior: 0!
A round BorderedMorph. Supports borderWidth and borderColor. 

EllipseMorph new borderWidth:10; borderColor: Color green; openInWorld.!


!FormCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 2/15/2013 17:19'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: mfc

	| displayRectangle |
	displayRectangle _ (currentTransformation displayBoundsOfTransformOf: (Rectangle center: mcx@mcy extent: mrx@mry * 2)) truncated.
	"draw the border of the oval"
	(mbw = 0 or: [mbc isTransparent]) ifFalse:[
		self setPaintColor: mbc.
		port frameOval: displayRectangle borderWidth: mbw].
	"fill the inside"
	mfc isTransparent ifFalse: [
		self setPaintColor: mfc.
		port fillOval: (displayRectangle insetBy: mbw)].
! !


!EllipseMorph methodsFor: 'drawing' stamp: 'jmv 2/15/2013 17:23'!
drawOn: aCanvas 

	| bc bw rx ry |
	bw _ borderWidth.
	bc _ borderColor.
	aCanvas isShadowDrawing
		ifTrue: [
			bw _ 0.
			bc _ nil ].
	self revisar. "The morph should be specified better!!"
	rx _ extent x //2.
	ry _ extent y // 2.
	aCanvas ellipseCenterX: rx y: ry rx: rx ry: ry borderWidth: bw borderColor: bc fillColor: color! !

!methodRemoval: FormCanvas #fillOval:color:borderWidth:borderColor:!
FormCanvas removeSelector: #fillOval:color:borderWidth:borderColor:!
