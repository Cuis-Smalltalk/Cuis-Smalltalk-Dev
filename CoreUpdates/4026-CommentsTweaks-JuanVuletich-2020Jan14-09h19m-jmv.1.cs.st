'From Cuis 5.0 [latest update: #4024] on 14 January 2020 at 9:33:11 am'!

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 1/14/2020 09:32:14'!
clipRect
	"Return the currently active clipping rectangle"
	"In targetForm coordinates"

	^ clipLeft@clipTop corner: clipRight@clipBottom+1! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 1/14/2020 09:32:17'!
setClipRect: aRectangle
	"by convention, aRectangle includes left and top but does not include right and bottom.
	We do draw clipRight and clipBottom but not beyond.
	"
	"In targetForm coordinates"

	clipLeft _ aRectangle left.
	clipTop _ aRectangle top.
	clipRight _ aRectangle right - 1.
	clipBottom _ aRectangle bottom - 1! !


!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/14/2020 09:32:06'!
frameRectangle: rect borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor
	"
	rect and borderWidth are in targetForm coordinates. No transformation is done.
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		topLeftColor: Color green
		bottomRightColor: Color red
	Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white
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

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/14/2020 09:32:10'!
setClipRect: aRectangle
	"In targetForm coordinates"

	super setClipRect: aRectangle.
	engine clipRect: aRectangle! !

!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 1/14/2020 09:32:00'!
clippingRectForCurrentMorph
	"In targetForm coordinates"

	^currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds! !

!BitBltCanvas methodsFor: 'testing' stamp: 'jmv 1/14/2020 09:30:46'!
isCurrentMorphVisible

	| aRectangle |
	currentMorph visible ifFalse: [ ^false ].
	"#clippingRectForCurrentMorph is valid even before drawing currentMorph, only in BitBltCanvas!!"
	aRectangle _ self clippingRectForCurrentMorph.
	aRectangle right < clipLeft		ifTrue: [^ false].
	aRectangle left > (clipRight+1)	ifTrue: [^ false].
	aRectangle bottom < clipTop		ifTrue: [^ false].
	aRectangle top > (clipBottom+1)	ifTrue: [^ false].
	^ true
! !

