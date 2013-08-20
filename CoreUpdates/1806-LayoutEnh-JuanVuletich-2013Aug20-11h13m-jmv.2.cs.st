'From Cuis 4.2 of 25 July 2013 [latest update: #1805] on 20 August 2013 at 11:19:42.55497 am'!

!Morph methodsFor: 'layout-properties' stamp: 'jmv 8/20/2013 10:55'!
layoutSpecOrNil
	"Layout specific. Return the layout spec describing where the  
	receiver should appear in a proportional layout.
	Answer nil if none!!"
	^self hasExtension
		ifTrue: [ self extension layoutSpec ]! !


!SystemWindow methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/20/2013 11:01'!
addMorph: aMorph 
	"Add a submorph to our client area."

	layoutMorph addMorph: aMorph! !

!SystemWindow methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/20/2013 11:01'!
addMorph: aMorph layoutSpec: aLayoutSpec
	"Add a submorph to our client area."

	layoutMorph addMorph: aMorph layoutSpec: aLayoutSpec! !


!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/20/2013 11:15'!
addMorph: aMorph
	"Add a submorph, at the bottom or right, with a default LayoutSpec if none was provided."

	aMorph layoutSpecOrNil ifNil: [ 
		aMorph layoutSpec: LayoutSpec new ].
	self addMorphFront: aMorph! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/20/2013 11:16'!
example7
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example7
	"
	"============================================"
	| c colorHexValue colorName r w |
	w := SystemWindow new.
	r := LayoutMorph newRow separation: 30 @ 10.
	c := LayoutMorph newColumn separation: 20 @ 10.
	colorHexValue := StringMorph contents:  'F97306'.
	colorName := StringMorph contents: 'cornSilk'.

	r addMorph: colorHexValue.
	r addMorph: colorName.
	c addMorph: RectangleLikeMorph new.
	c addMorph: r.
	w addMorph: c .
	w openInWorld.
	"============================================"! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/20/2013 10:50'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	
	self initializeLabelArea.
	extent _ 300 @ 200.
	updatablePanes _ #().

	adjusters _ Dictionary new.
	adjusters at: #topAdjuster put: WindowEdgeAdjustingMorph forTop.
	adjusters at: #bottomAdjuster put: WindowEdgeAdjustingMorph forBottom.
	adjusters at: #leftAdjuster put: WindowEdgeAdjustingMorph forLeft.
	adjusters at: #rightAdjuster put: WindowEdgeAdjustingMorph forRight.
	adjusters at: #topLeftAdjuster put: WindowEdgeAdjustingMorph forTopLeft.
	adjusters at: #bottomLeftAdjuster put: WindowEdgeAdjustingMorph forBottomLeft.
	adjusters at: #topRightAdjuster put: WindowEdgeAdjustingMorph forTopRight.
	adjusters at: #bottomRightAdjuster put: WindowEdgeAdjustingMorph forBottomRight.
	adjusters do: [ :m |
		self addMorphFront: m ].

	"by default"
	self beColumn! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/20/2013 10:51'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| spacing box |
	spacing _ self boxExtent x + 2.

	box _ self createCloseBox.
	self addMorphFront: box.
	box morphPosition: 2@2.
	box morphExtent: self boxExtent.

	box _ self createCollapseBox.
	self addMorphFront: box.
	box morphPosition: spacing+2@2.
	box morphExtent: self boxExtent.

	box _ self createExpandBox.
	self addMorphFront: box.
	box morphPosition: spacing*2+2@2.
	box morphExtent: self boxExtent.

	box _ self createMenuBox.
	self addMorphFront: box.
	box morphPosition: spacing*3+2@2.
	box morphExtent: self boxExtent.! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 8/20/2013 10:50'!
beColumn
	layoutMorph
		ifNotNil: [ layoutMorph beColumn ]
		ifNil: [
			layoutMorph _ LayoutMorph newColumn.
			self addMorphFront: layoutMorph ]! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 8/20/2013 10:50'!
beRow
	layoutMorph
		ifNotNil: [ layoutMorph beRow ]
		ifNil: [
			layoutMorph _ LayoutMorph newRow.
			self addMorphFront: layoutMorph ]! !


!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/20/2013 10:48'!
addMorph: aMorph layoutSpec: aLayoutSpec

	"Add a submorph, at the bottom or right, with aLayoutSpec"
	aMorph layoutSpec: aLayoutSpec.
	self addMorphFront: aMorph! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/20/2013 11:19'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane _ LayoutMorph newRow separation: 5. "1"
pane addMorph: (StringMorph contents: '1').

rect1 := BorderedRectMorph new color: (Color lightOrange); 
	morphWidth: 20; morphHeight: 30.
pane addMorphFront: rect1.
rect2 := BorderedRectMorph new color: (Color cyan); 
	morphWidth: 20; morphHeight: 30.
pane addMorphFront: rect2.
pane
	color: Color lightGreen;
	openInWorld;
	morphPosition: 120 @ 50;
	morphExtent: 180 @ 100.

pane _ LayoutMorph newRow separation: 5. "2"
pane addMorph: (StringMorph contents: '2').

rect1 := BorderedRectMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect1.
rect2 := BorderedRectMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane
	color: Color lightGreen;
	openInWorld;
	morphPosition: 320 @ 50;
	morphExtent: 180 @ 100.


pane _ LayoutMorph newRow separation: 5. "3"
pane addMorph: (StringMorph contents: '3').

rect1 := BorderedRectMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
rect2 := BorderedRectMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect2.
pane
	color: Color lightGreen;
	openInWorld;
	morphPosition: 520 @ 50;
	morphExtent: 180 @ 100! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 8/20/2013 10:58'!
initialize
	super initialize.
	self separation: 0.
	labelMorph _ StringMorph contents: '' font: AbstractFont default.
	subLabelMorph _ StringMorph contents: '' font: AbstractFont default.
	progress _ ProgressBarMorph new.
	progress morphExtent: 200 @ 15.
	self addMorphFront: labelMorph.
	self addMorphFront: subLabelMorph.
	self addMorph: progress fixedHeight: 15.! !

