'From Cuis 4.1 of 12 December 2012 [latest update: #1658] on 2 April 2013 at 11:04:27 pm'!

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 4/2/2013 22:58'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect |

	r area = 0 ifTrue: [ ^self ].
	rect _ currentTransformation displayBoundsOfTransformOf: r.

	"draw the border of the rectangle"
	borderColor isTransparent ifFalse:[
		self setPaintColor: borderColor.
		port frameRect: rect borderWidth: borderWidth ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillRect: (rect insetBy: borderWidth) ]! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 4/2/2013 23:02'!
frameRectangle: rect borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor
	"
	rect and borderWidth are in form coordinates. No transformation is done.
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

	port frameRectTopLeft: rect borderWidth: borderWidth.

	borderWidth isNumber
		ifTrue: [w _ h _ borderWidth]
		ifFalse: [w _ borderWidth x.   h _ borderWidth y].
	self setPaintColor: bottomRightColor.
	port 
		 frameRectRight: rect width: w;
		 frameRectBottom: rect height: h! !


!GrafPort methodsFor: 'drawing support' stamp: 'jmv 4/2/2013 23:02'!
fillRect: rect

	rect area = 0 ifTrue: [^ self].
	destX _ rect left.
	destY _ rect top.
	sourceX _ 0.
	sourceY _ 0.
	width _ rect width.
	height _ rect height.
	self copyBits! !

!GrafPort methodsFor: 'drawing support' stamp: 'jmv 4/2/2013 23:02'!
frameRect: rect borderWidth: borderWidth
	"Paint a border whose rectangular area is defined by rect. The
	width of the border of each side is borderWidth."

	rect area = 0 ifTrue: [^ self].

	sourceX _ 0.
	sourceY _ 0.
	
	"for top and bottom, the following are the same"
	height _ borderWidth. 
	width _ rect width. 
	destX _ rect left.

	"top"
	destY _ rect top.
	self copyBits.

	"bottom"
	destY _ rect bottom - borderWidth.
	self copyBits.

	"for left & right, the following are the same"
	height _ rect height. 
	width _ borderWidth. 
	destY _ rect top.

	"left"
	destX _ rect left.
	self copyBits.

	"right"
	destX _ rect right - borderWidth.
	self copyBits.! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 4/2/2013 22:28'!
addHandlesTo: aHaloMorph box: box
	"Add halo handles to the halo.  Apply the halo filter if appropriate"

	Preferences haloSpecifications do: [ :aSpec |
		(self
			wantsHaloHandleWithSelector: aSpec addHandleSelector
			inHalo: aHaloMorph) ifTrue: [
		aHaloMorph
			perform: aSpec addHandleSelector
			with: aSpec ]].
	aHaloMorph target
		addOptionalHandlesTo: aHaloMorph
		box: box! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 4/2/2013 22:27'!
addHandles

	self removeAllMorphs.  "remove old handles, if any"
	self morphBoundsInWorld: target worldBoundsForHalo.  "update my size"
	haloBox _ self basicBox.
	target addHandlesTo: self box: haloBox.
	self addNameString: (target printStringLimitedTo: 40).
	growingOrRotating _ false.
	self redrawNeeded! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 4/2/2013 22:39'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition nameBackground |
	nameBackground _ RectangleLikeMorph new
		color: (Color lightBlue alpha: 0.9).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color magenta.
	self addMorph: nameBackground.
	self addMorph: nameMorph.
	namePosition _ haloBox width - nameMorph morphWidth // 2 @ (haloBox height).
	nameMorph morphPosition: namePosition.
	nameBackground morphPosition: nameMorph morphPosition - 2.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	^nameMorph! !

!methodRemoval: HaloMorph #haloBox:!
HaloMorph removeSelector: #haloBox:!
