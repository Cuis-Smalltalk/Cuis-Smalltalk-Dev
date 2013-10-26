'From Cuis 4.0 of 21 April 2012 [latest update: #1386] on 21 August 2012 at 8:34:35 pm'!

!FillInTheBlankMorph commentStamp: '<historical>' prior: 0!
A simple dialog with an entry field and accept / cancel buttons.!


!MenuLineMorph commentStamp: '<historical>' prior: 0!
Just a line for separating items in menus.!

!classDefinition: #RectangleLikeMorph category: #'Morphic-Kernel'!
Morph subclass: #RectangleLikeMorph
	instanceVariableNames: 'xtent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!RectangleLikeMorph commentStamp: '<historical>' prior: 0!
Hierarchy  for morph that are rectangle like. Including rectangles with rounded corners and such. The idea is that the 'xtent' ivar is all that's needed to establish our extent and shape. Subclasses can add things like 'roundedCornerRadious' or such.!

!classDefinition: #BorderedRectMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #BorderedRectMorph
	instanceVariableNames: 'borderWidth borderColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!BorderedRectMorph commentStamp: '<historical>' prior: 0!
BorderedMorph introduce borders to morph. Borders have the instanceVariables borderWidth and borderColor.
 
BorderedMorph new borderColor: Color red; borderWidth: 10; openInWorld.

BorderedMorph also have a varaity of border styles: simple, inset, raised, complexAltFramed, complexAltInset, complexAltRaised, complexFramed, complexInset, complexRaised.
These styles are set using the classes BorderStyle, SimpleBorder, RaisedBorder, InsetBorder and ComplexBorder.

BorderedMorph new borderColor: Color white; openInWorld.
BorderedMorph new borderColor: #inset; borderWidth: 2; openInWorld!


!BorderedRectMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 16:12'!
borderColor
	^ borderColor! !

!BorderedRectMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 16:12'!
borderColor: aColor
	borderColor = aColor ifFalse: [
		borderColor _ aColor.
		self redrawNeeded]! !

!BorderedRectMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 16:12'!
borderWidth: anInteger
	borderWidth = anInteger ifFalse: [
		borderColor ifNil: [ borderColor _ Color black ].
		borderWidth _ anInteger max: 0.
		self redrawNeeded ]! !

!BorderedRectMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 16:12'!
drawOn: aCanvas

	aCanvas fillRectangle: self morphBoundsInWorld colorOrInfiniteForm: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: borderColor! !

!BorderedRectMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:23'!
innerBounds
	"Return the inner rectangle enclosed by the bounds of this morph excluding the space taken by its borders. For an unbordered morph, this is just its bounds."

	^ self morphBoundsInWorld insetBy: (borderWidth ifNil: [0])! !

!BorderedRectMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 16:12'!
defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ Color black! !

!BorderedRectMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 16:12'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 2! !

!BorderedRectMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 16:29'!
initialize
	self flag: #jmvVer2.  "many users would be just happy with a RectangleLikeMorph... Check them!!"
	"initialize the state of the receiver"
	super initialize.
	"initialize the receiver state related to border"
	borderColor _ self defaultBorderColor.
	borderWidth _ self defaultBorderWidth! !

!BorderedRectMorph methodsFor: 'testing' stamp: 'jmv 8/21/2012 16:12'!
isOpaqueMorph
	"Any submorph that answers true to #isOrthoRectangularMorph (to optimize #containsPoint:)
	but is not an opaque rectangle covering bounds MUST answer false to this message"
	color mightBeTranslucent ifTrue: [
		^false ].
	borderWidth > 0 ifTrue: [
		borderColor mightBeTranslucent ifTrue: [
			^false ]].
	^true! !


!BorderedRectMorph class methodsFor: 'instance protocol testing' stamp: 'jmv 8/21/2012 16:12'!
gatherProtocols
	"See comment at #is:"

	^super gatherProtocols, #(BorderedMorph)! !


!BorderedMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:34'!
innerBounds
	"Return the inner rectangle enclosed by the bounds of this morph excluding the space taken by its borders. For an unbordered morph, this is just its bounds."

	^ self morphBoundsInWorld insetBy: (borderWidth ifNil: [0])! !

!BorderedMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 16:12'!
initialize
	"initialize the state of the receiver"
	super initialize.
	"initialize the receiver state related to border"
	borderColor _ self defaultBorderColor.
	borderWidth _ self defaultBorderWidth! !


!FormCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 8/21/2012 16:23'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	"

	| resizeFactor outerBox arrowMorph resizedForm f |
	resizeFactor _ 4.
	outerBox _ BorderedRectMorph new.
	outerBox
		morphExtent: finalSizeInteger asPoint * resizeFactor;
		borderWidth: 0;
		color: Color transparent.
	
	arrowMorph _ self buildArrowIn: outerBox morphBoundsInWorld.
	outerBox addMorphFront: arrowMorph.
	arrowMorph morphPositionInOwner: 12@8.	"not a clue why these numbers work..."
	
	
	f _ outerBox imageForm: 32.
	resizedForm _ f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.

	aSymbolDirection == #right ifTrue: [
		resizedForm _ resizedForm rotateBy: 90 ].
	aSymbolDirection == #down ifTrue: [
		resizedForm _ resizedForm rotateBy: 180 ].
	aSymbolDirection == #left ifTrue: [
		resizedForm _ resizedForm rotateBy:  270 ].
		
	aSymbolDirection == #up ifFalse: [
		resizedForm _ resizedForm
			copy: (resizedForm boundingBox insetBy: (resizedForm width - finalSizeInteger/ 2.0) rounded) ].
		
	^resizedForm! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 8/21/2012 16:23'!
addNameBeneath: outerRectangle string: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition w nameBackground |
	w _ self world ifNil: [ target world ].
	nameBackground _ BorderedRectMorph new
		borderWidth: 0;
		color: (Color lightBlue alpha: 0.9).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color magenta.
	self addMorph: nameBackground.
	self addMorph: nameMorph.
	namePosition _ outerRectangle bottomCenter - ((nameMorph morphWidth // 2) @ (self handleSize negated // 2 - 1)).
	nameMorph morphPosition: (namePosition min: w viewBox bottomRight - nameMorph morphExtent y + 5).
	nameBackground morphBoundsInWorld: (nameMorph morphBoundsInWorld outsetBy: 2).
	^nameMorph! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/21/2012 16:23'!
example1
"
	self example1
"
| pane row |
pane _ LayoutMorph newColumn separation: 5.
pane color: Color red.

row _ LayoutMorph newRow.
row
	color: Color red;
	addMorph: (RectangleMorph new color: (Color h: 60 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (RectangleMorph new color: (Color blue))
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (RectangleMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (RectangleMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (RectangleMorph new color: (Color h: 60 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ LayoutMorph newRow separation: 5.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color blue))
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ LayoutMorph newRow separation: 5.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/21/2012 16:23'!
example11
"
	self example11
"
| pane row |
pane _ LayoutMorph newColumn separation: 5.
pane color: Color red.

row _ LayoutMorph newRow separation: 5.
row
	color: Color red;
	addMorph: (RectangleMorph new color: (Color h: 60 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 10);
	addAdjusterMorph; 
	addMorph: (RectangleMorph new color: (Color blue))
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (RectangleMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (RectangleMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (RectangleMorph new color: (Color h: 60 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ LayoutMorph newRow separation: 5.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color blue))
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ LayoutMorph newRow separation: 5.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/21/2012 16:24'!
example13
	"
	self example13
	"
	| pane row innerRow |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	innerRow _ (LayoutMorph newRow separation: 5) color: Color red.
	innerRow
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 minorDirectionPadding: #center);
addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 minorDirectionPadding: #top);
addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/21/2012 16:24'!
example2
	"
	self example2
	"
	| pane row |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 minorDirectionPadding: #bottom);
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 minorDirectionPadding: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/21/2012 16:24'!
example3
	"
	self example3
	"
	| pane row innerRow |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	innerRow _ (LayoutMorph newRow separation: 5) color: Color red.
	innerRow
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: BorderedRectMorph new layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 minorDirectionPadding: #center);
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/21/2012 16:24'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane _ LayoutMorph newRow separation: 5. "1"
pane color: Color lightGreen; morphBoundsInWorld: (120 @ 50 extent: 180 @ 100).
pane addMorph: (StringMorph contents: '1').

rect1 := RectangleMorph new color: (Color lightOrange); 
	morphWidth: 20; morphHeight: 30.
pane addMorph: rect1.
rect2 := RectangleMorph new color: (Color cyan); 
	morphWidth: 20; morphHeight: 30.
pane addMorph: rect2.
pane openInWorld.


pane _ LayoutMorph newRow separation: 5. "2"
pane color: Color lightGreen; morphBoundsInWorld: (320 @ 50 extent: 180 @ 100).
pane addMorph: (StringMorph contents: '2').

rect1 := RectangleMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect1.
rect2 := BorderedRectMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane openInWorld.


pane _ LayoutMorph newRow separation: 5. "3"
pane color: Color lightGreen; morphBoundsInWorld: (520 @ 50 extent: 180 @ 100).
pane addMorph: (StringMorph contents: '3').

rect1 := BorderedRectMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
rect2 := BorderedRectMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect2.
pane openInWorld.! !


!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 8/21/2012 16:28'!
testLayout1
	"
	self new testLayout1
	"
	| pane row1 row2 row3 r1c1 r1c2 r1c3 r1c4 r1c5 r2c1 r2c2 r2c3 r3c1 r3c2 r3c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row1 _ LayoutMorph newRow separation: 5.
	row1 color: Color red;
		addMorph: (r1c1 _ BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 10);
		addMorph: (r1c2 _ BorderedRectMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r1c3 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r1c4 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.15);
		addMorph: (r1c5 _ BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
	pane addMorph: row1 layoutSpec: LayoutSpec useAll.
	row2 _ LayoutMorph newRow separation: 5.
	row2 color: Color red;
		addMorph: (r2c1 _ BorderedRectMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r2c2 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r2c3 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.2).
	pane addMorph: row2 layoutSpec: LayoutSpec useAll.
	row3 _ LayoutMorph newRow separation: 5.
	row3 color: Color red;
		addMorph: (r3c1 _ BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
		addMorph: (r3c2 _ BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40);
		addMorph: (r3c3 _ BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row3 layoutSpec: (LayoutSpec fixedHeight: 60).
	pane openInWorld; morphExtent: 408@300.
	World doOneCycleNow.

	self assert: row1 morphWidth = (pane morphWidth - 10).
	self assert: r1c1 morphWidth class == SmallInteger.
	self assert: r1c1 morphHeight class == SmallInteger.
	self assert: r1c1 morphWidth = 10.
	self assert: r1c1 morphHeight = (row1 morphHeight - 10).
	self assert: r1c2 morphWidth = 200.
	self assert: r1c2 morphHeight = (row1 morphHeight - 10).
	self assert: r1c3 morphWidth = (r1c2 morphWidth / 0.8 * 0.4) rounded.
	self assert: r1c3 morphHeight = (row1 morphHeight - 10).
	self assert: r1c4 morphWidth = (r1c2 morphWidth / 0.8 * 0.15) rounded.
	self assert: r1c4 morphHeight = (row1 morphHeight - 10).
	self assert: r1c5 morphWidth = 20.
	self assert: r1c5 morphHeight = 20.

	self assert: row2 morphWidth = (pane morphWidth - 10).
	self assert: r2c1 morphWidth = 216.
	self assert: r2c1 morphHeight = (row2 morphHeight - 10).
	self assert: r2c2 morphWidth = (r2c1 morphWidth / 0.8 * 0.4) rounded.
	self assert: r2c2 morphHeight = (row2 morphHeight - 10).
	self assert: r2c3 morphWidth = (r2c1 morphWidth / 0.8 * 0.2) rounded.
	self assert: r2c3 morphHeight = (row2 morphHeight - 10).

	self assert: row3 morphWidth = (pane morphWidth - 10).
	self assert: row3 morphHeight = 60.
	self assert: r3c1 morphWidth = 20.
	self assert: r3c1 morphHeight = (row3 morphHeight - 10 * 0.8) rounded.
	self assert: r3c2 morphWidth = (row3 morphWidth - 10 - 20 - 10 - 30 * 0.5) rounded.
	self assert: r3c2 morphHeight = 40.
	self assert: r3c3 morphWidth = 30.
	self assert: r3c3 morphHeight = (row3 morphHeight - 10).

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 8/21/2012 16:25'!
testLayout2
	"
	self new testLayout2
	"
	| pane row c1 c2 c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (c1 _ BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 minorDirectionPadding: #bottom);
		addMorph: (c2 _ BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (c3 _ BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 minorDirectionPadding: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane openInWorld; morphExtent: 400@300.
	World doOneCycleNow.

	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = (pane morphHeight - 10 * 0.9) rounded.
	self assert: c1 morphBoundsInWorld bottom = (row morphBoundsInWorld bottom - 5) description: 'Should be at bottom'.
	self assert: c1 morphWidth = 20.
	self assert: c1 morphHeight = (row morphHeight - 10 * 0.8) rounded.
	self assert: c2 morphBoundsInWorld top = (row morphBoundsInWorld top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = 256.
	self assert: c2 morphHeight = 40.
	self assert: ((c3 morphBoundsInWorld top - row morphBoundsInWorld top) - (row morphBoundsInWorld bottom - c3 morphBoundsInWorld bottom)) abs < 2 description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (c1 morphHeight / 0.8 * 0.7) rounded.

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 8/21/2012 16:25'!
testLayout3
	"
	self new testLayout3
	"
	| pane row innerRow i1 i2 i3 c2 c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	innerRow _ LayoutMorph newRow color: Color red;  separation: 5.
	innerRow
		addMorph: (i1 _ BorderedRectMorph new  borderWidth: 0)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i2 _ BorderedRectMorph new  borderWidth: 0)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i3 _ BorderedRectMorph new  borderWidth: 0)
			layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 minorDirectionPadding: #center);
		addMorph: (c2 _ BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (c3 _ BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 200).
	pane openInWorld; morphExtent: 400@300.
	World doOneCycleNow.

	self assert: row morphBoundsInWorld left = (pane morphBoundsInWorld left + 5).
	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 200.
	self assert: innerRow morphBoundsInWorld left = (row morphBoundsInWorld left + 5).
	self assert: (innerRow morphBoundsInWorld top - row morphBoundsInWorld top) = (row morphBoundsInWorld bottom - innerRow morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: innerRow morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: innerRow morphHeight = 30.

	self assert: i1 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 5).
	self assert: (i1 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i1 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i1 morphWidth = 10.
	self assert: i1 morphHeight = 10.
	self assert: i2 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 20).
	self assert: (i2 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i2 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i2 morphWidth = 10.
	self assert: i2 morphHeight = 10.
	self assert: i3 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 35).
	self assert: (i3 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i3 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i3 morphWidth = (innerRow morphWidth - 40).
	self assert: i3 morphHeight = 10.

	self assert: c2 morphBoundsInWorld top = (row morphBoundsInWorld top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: c2 morphHeight = 40.
	self assert: (c3 morphBoundsInWorld top - row morphBoundsInWorld top) = (row morphBoundsInWorld bottom - c3 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (row morphHeight - 10).

	pane delete! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 8/21/2012 16:26'!
addStayUpIcons
	| closeBox pinBox w |
	
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	closeBox _ PluggableButtonMorph model: self action: #delete.
	closeBox icon: Theme current closeIcon.
	pinBox _ PluggableButtonMorph model: self action: #stayUp.
	pinBox icon: Theme current pushPinIcon.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 42.
	self addMorphFront: 
		(LayoutMorph newRow
			morphHeight: (titleMorph morphHeight max: 19);
			morphWidth: w;	"Make room for buttons"
			color: Color transparent;
			addMorph: closeBox fixedWidth: 20;
			addMorph: (BorderedRectMorph new borderWidth: 0; color: Color transparent) fixedWidth: 4;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (BorderedRectMorph new borderWidth: 0; color: Color transparent) fixedWidth: 4;
			addMorph: pinBox fixedWidth: 20).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 8/21/2012 16:26'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w |
	
	titleMorph _ BorderedRectMorph new.
	self setTitleParametersFor: titleMorph.
	pp _ 8@2.
	aString asString linesDo: [ :line |
		s _ StringMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s.
		s morphPositionInOwner: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphHeight: pp y; morphWidth: w + 8.
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !


!ScrollBar methodsFor: 'initialize' stamp: 'jmv 8/21/2012 16:28'!
initializeSlider
	"initialize the receiver's slider"

	sliderShadow _ BorderedRectMorph new.
	sliderShadow borderWidth: 0.
	self addMorph: sliderShadow.
	sliderShadow hide.
		
	slider _ self sliderClass new.
	slider model: self.
	slider grabSelector: #sliderGrabbed.
	slider dragSelector: #scrollAbsolute:.
	slider action: #sliderReleased.
	self addMorph: slider.

	self computeSlider! !

