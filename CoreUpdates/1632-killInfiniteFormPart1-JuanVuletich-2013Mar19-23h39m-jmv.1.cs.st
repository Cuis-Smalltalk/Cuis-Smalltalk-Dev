'From Cuis 4.1 of 12 December 2012 [latest update: #1630] on 20 March 2013 at 12:09:49 am'!

!Form class methodsFor: 'instance creation' stamp: 'jmv 3/20/2013 00:08'!
verticalGradient: h topColor: topColor bottomColor: bottomColor
	"
	(Form 
		verticalGradient: 120
		topColor: Color lightBlue
		bottomColor: Color gray) display
	(Form 
		verticalGradient: 120
		topColor: Color white
		bottomColor: (Color gray: 0.85)) display
	"
	| form w c |
	w _ 200.
	form _ Form extent: w@h depth: 32.
	0 to: h-1 do: [ :y |
		c _ bottomColor mixed: (1.0 * y / (h-1)) with: topColor.
		0 to: w-1 do: [ :x |
			form colorAt: x@y put: c ]].
	^form! !


!BitBlt methodsFor: 'accessing' stamp: 'jmv 3/19/2013 23:46'!
fillColor: aColor 
	"The destForm will be filled with this color or pattern of colors.  May be an old Color, a new type Color, a Bitmap (see BitBlt comment), a Pattern, or a Form.  6/18/96 tk"

	aColor ifNil: [halftoneForm _ nil. ^ self].
	destForm ifNil: [self error: 'Must set destForm first'].
	halftoneForm _ destForm bitPatternFor: aColor ! !


!BitBlt class methodsFor: 'examples' stamp: 'jmv 3/19/2013 23:58'!
exampleTwo
	"This is to test painting with a gray tone. It also tests that the seaming with gray patterns is correct in the microcode. Lets you paint for a while and then automatically stops. This only works at Depth of 1."
	| f aBitBlt displayDepth |
	"create a small black Form source as a brush. "
	displayDepth _ Display depth.
	Display newDepth: 1.
	f _ Form extent: 20 @ 20.
	f fillBlack.
	"create a BitBlt which will OR gray into the display. "
	aBitBlt _ BitBlt
		destForm: Display
		sourceForm: f
		fillColor: Color gray
		combinationRule: Form over
		destOrigin: Sensor mousePoint
		sourceOrigin: 0 @ 0
		extent: f extent
		clipRect: Display boundingBox.
	"paint the gray Form on the screen for a while. "
	[Sensor isAnyButtonPressed] whileFalse: 
		[aBitBlt destOrigin: Sensor mousePoint.
		aBitBlt copyBits].
	Display newDepth: displayDepth.
	"BitBlt exampleTwo"! !

!BitBlt class methodsFor: 'private' stamp: 'jmv 3/19/2013 23:58'!
exampleAt: originPoint rule: rule fillColor: mask 
	"This builds a source and destination form and copies the source to the
	destination using the specifed rule and mask. It is called from the method
	named exampleOne. Only works with Display depth of 1"

	| s d border aBitBlt | 
	border _ Form extent: 32@32.
	border fillBlack.
	border fill: (1@1 extent: 30@30) fillColor: Color white.
	s _ Form extent: 32@32.
	s fillWhite.
	s fillBlack: (7@7 corner: 25@25).
	d _ Form extent: 32@32.
	d fillWhite.
	d fillBlack: (0@0 corner: 32@16).

	s displayOn: Display at: originPoint.
	border displayOn: Display at: originPoint rule: Form under.
	d displayOn: Display at: originPoint + (s width @0).
	border displayOn: Display at: originPoint + (s width @0) rule: Form under.

	d displayOn: Display at: originPoint + (s extent // (2 @ 1)). 
	aBitBlt _ BitBlt
		destForm: Display
		sourceForm: s
		fillColor: mask
		combinationRule: rule
		destOrigin: originPoint + (s extent // (2 @ 1))
		sourceOrigin: 0 @ 0
		extent: s extent
		clipRect: Display boundingBox.
	aBitBlt copyBits.
	border 
		displayOn: Display at: originPoint + (s extent // (2 @ 1))
		rule: Form under.

	"BitBlt exampleAt: 100@100 rule: 0 fillColor: nil"  ! !


!DisplayObject methodsFor: 'display box access' stamp: 'jmv 3/19/2013 23:59'!
boundingBox
	"Answer the rectangular area that represents the boundaries of the 
	receiver's space of information."

	self subclassResponsibility! !


!Form methodsFor: 'color mapping' stamp: 'jmv 3/19/2013 23:46'!
bitPatternFor: aColor
	"Return the pixel word for representing the given color on the receiver"
	^aColor bitPatternForDepth: self depth! !


!FormCanvas methodsFor: 'private' stamp: 'jmv 3/19/2013 23:45'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor |
	paintColor _ shadowColor ifNil: [ aColor ].
	paintColor ifNil: [ paintColor _ Color transparent ].
	(paintColor is: #Color) ifFalse: [
		^self error: 'Cannot install color' ].

	"Okay, so paintColor really *is* a color"
	port sourceForm: nil.
	(paintColor isOpaque or: [ self depth < 32]) ifTrue: [
		port fillColor: paintColor.
		port combinationRule: Form paint.
		^self ].

	"BitBlt setup for alpha mapped transfer"
	port fillColor: paintColor.
	self depth = 16
		ifTrue: [ port alphaBits: paintColor privateAlpha; combinationRule: 31 ]
		ifFalse: [ port combinationRule: Form blend ]! !


!FormCanvas class methodsFor: 'cached forms' stamp: 'jmv 3/20/2013 00:08'!
verticalGrayGradient: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^CachedForms
		at: { #vertical. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				verticalGradient: height
				topColor: (Color gray: gradientTopFactor)
				bottomColor: (Color gray: gradientBottomFactor) ]! !

!methodRemoval: InfiniteForm class #with:!
InfiniteForm class removeSelector: #with:!
!methodRemoval: InfiniteForm #asColor!
InfiniteForm removeSelector: #asColor!
!methodRemoval: InfiniteForm #bitPatternForDepth:!
InfiniteForm removeSelector: #bitPatternForDepth:!
!methodRemoval: InfiniteForm #computeBoundingBox!
InfiniteForm removeSelector: #computeBoundingBox!
!methodRemoval: InfiniteForm #direction!
InfiniteForm removeSelector: #direction!
!methodRemoval: InfiniteForm #displayOn:at:clippingBox:rule:fillColor:!
InfiniteForm removeSelector: #displayOn:at:clippingBox:rule:fillColor:!
!methodRemoval: InfiniteForm #displayOnPort:at:!
InfiniteForm removeSelector: #displayOnPort:at:!
!methodRemoval: InfiniteForm #dominantColor!
InfiniteForm removeSelector: #dominantColor!
!methodRemoval: InfiniteForm #isTiled!
InfiniteForm removeSelector: #isTiled!
!methodRemoval: InfiniteForm #isTransparent!
InfiniteForm removeSelector: #isTransparent!
!methodRemoval: InfiniteForm #mightBeTranslucent!
InfiniteForm removeSelector: #mightBeTranslucent!
!methodRemoval: InfiniteForm #normal!
InfiniteForm removeSelector: #normal!
!methodRemoval: InfiniteForm #offset!
InfiniteForm removeSelector: #offset!
!methodRemoval: InfiniteForm #origin!
InfiniteForm removeSelector: #origin!

!InfiniteForm reorganize!
('private' form:)
('fillstyle protocol' form)
!

!methodRemoval: ColorForm #displayOnPort:at:!
ColorForm removeSelector: #displayOnPort:at:!
!methodRemoval: Form #bitPatternForDepth:!
Form removeSelector: #bitPatternForDepth:!
!methodRemoval: Form #computeBoundingBox!
Form removeSelector: #computeBoundingBox!
!methodRemoval: Form #displayOnPort:at:!
Form removeSelector: #displayOnPort:at:!
!methodRemoval: DisplayObject #computeBoundingBox!
DisplayObject removeSelector: #computeBoundingBox!
!methodRemoval: Color #asColor!
Color removeSelector: #asColor!
!methodRemoval: Bitmap #bitPatternForDepth:!
Bitmap removeSelector: #bitPatternForDepth:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
FormCanvas clearFormsCache!

