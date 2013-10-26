'From Cuis 4.0 of 21 April 2012 [latest update: #1410] on 26 August 2012 at 11:08:22 pm'!

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/26/2012 22:57'!
fillRectangle: aRectangle colorOrInfiniteForm: aColorOrInfiniteForm borderWidth: bw borderStyleSymbol: aSymbol
	"
	Display getCanvas
		fillRectangle: (10@10 extent: 300@200)
		colorOrInfiniteForm: (InfiniteForm verticalGradient: 24 topColor: Color green bottomColor:Color red)
		borderWidth: 5
		borderStyleSymbol: #raised
	"

	"Nicer. does frame with translucent black or white. Downside: requires proper color.
	Some buttons are actually transparent (should be fixed!!), and there is a trick to grab some
	opaque owner's color. And this is needed because (for instance) SystemWindow does NOT paint the inside with its color, but that color is needed to paint separators and button area. Something better is needed!!!!!!!!!!"
	"
	self fillRectangle: r color: .aColorOrInfiniteForm
	self frameRectangle: r borderWidth: bw borderStyleSymbol: aSymbol
	"

	self fillRectangle: (aRectangle insetBy: bw) colorOrInfiniteForm: aColorOrInfiniteForm.
	self frameRectangle: aRectangle color: aColorOrInfiniteForm asColor borderWidth: bw borderStyleSymbol: aSymbol! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/26/2012 23:00'!
fillRectangle: aRectangle colorOrInfiniteForm: aColorOrInfiniteForm borderWidth: bw borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

	"
	| if |
	if _ InfiniteForm verticalGradient: 24 topColor: Color green bottomColor:Color red.
	Display getCanvas
		fillRectangle: (10@10 extent: 300@200)
		colorOrInfiniteForm: if
		borderWidth: 5
		borderStyleSymbol: #raised
		baseColorForBorder: if asColor
	"

	"
Pretty ugly.
#fillRectangle:color::borderWidth:borderStyleSymbol:  is much better but has trouble with silly transparent morphs
	"
	
	self fillRectangle: (aRectangle insetBy: bw) colorOrInfiniteForm: aColorOrInfiniteForm.
	self frameRectangle: aRectangle color: baseColorForBorder borderWidth: bw borderStyleSymbol: aSymbol! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 8/26/2012 22:33'!
windowFrame: aRectangle color: aColor radius: r border: borderWidth labelHeight: h gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	Display getCanvas windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green
	"
	"top stripe"
	| bottomColor he tl tr |
	self
		image: (FormCanvas topLeftCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (FormCanvas topRightCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: h) insetBy: r@0)
		infiniteForm: (FormCanvas verticalGrayGradient: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.

	"left and right borders"
	tl _ aRectangle topLeft + (0@h).
	tr _ aRectangle topRight + (borderWidth negated@h).
	he _ borderWidth@(aRectangle height - h - r).
	self fillRectangle: (tl extent: he) colorOrInfiniteForm: bottomColor.
	self fillRectangle: (tr extent: he) colorOrInfiniteForm: bottomColor.
	
	"bottom stripe"
	self
		image: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@borderWidth negated)) extent: (aRectangle width - r - r@borderWidth)) colorOrInfiniteForm: bottomColor.

	"inside"
	self fillRectangle: (aRectangle insetBy: (borderWidth@h corner: borderWidth@borderWidth)) colorOrInfiniteForm: insideColor! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/26/2012 22:39'!
drawOn: aCanvas 
	"Draw the hand itself (i.e., the cursor)."
	"It looks like this method is only called when we are carrying morphs around..."
	 aCanvas
		stencil: Cursor move
		at: self morphPositionInWorld
		color: Color black! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/26/2012 22:46'!
needsToBeDrawn
	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor. This is the case if it (a) it is a remote hand, (b) it is showing a temporary cursor, or (c) it is not empty and there are any visible submorphs. If using the software cursor, ensure that the hardware cursor is hidden."
	"Details:  Return true if this hand has a saved patch to ensure that is is processed by the world. This saved patch will be deleted after one final display pass when it becomes possible to start using the hardware cursor again. This trick gives us one last display cycle to allow us to remove the software cursor and shadow from the display."
	
	"Note. We draw the hand as a regular morph (using #drawOn:), disabling the hardware cursor, when we carry submorphs. The reason is to lock the mouse pointer and the carried morph together. Otherwhise the carried morph would lag behind the mouse pointer.
	This method answers whether the regular #drawOn: drawing mechanism is used for us.
	
	Check senders. Hand drawing is handled explicitly by the world, because the Hand is not a submorph of the world!!"

	(savedPatch notNil
		or: [ submorphs anySatisfy: [ :ea | ea visible ] ] )
		ifTrue: [
			"using the software cursor; hide the hardware one"
			Sensor currentCursor == Cursor blank ifFalse: [Cursor blank show].
			^ true].

	^ false! !


!LayoutAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 8/26/2012 22:59'!
drawOn: aCanvas
	aCanvas fillRectangle: self morphBoundsInWorld colorOrInfiniteForm: color borderWidth: 2 borderStyleSymbol: #raised baseColorForBorder: color! !


!MorphicScanner methodsFor: 'stop conditions' stamp: 'jmv 8/26/2012 22:26'!
doNewLine
	"When a newLine is encountered, simply increment the pointer 
	into the paragraph."

	lastIndex _ lastIndex + 1.
	^false! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/26/2012 22:59'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: self morphBoundsInWorld
		colorOrInfiniteForm: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: c.

	self drawRegularLabelOn: aCanvas! !

!methodRemoval: FormCanvas #line:to:brushForm:!
FormCanvas removeSelector: #line:to:brushForm:!
!methodRemoval: FormCanvas #line:to:width:color:dashLength:secondColor:secondDashLength:startingOffset:!
FormCanvas removeSelector: #line:to:width:color:dashLength:secondColor:secondDashLength:startingOffset:!
