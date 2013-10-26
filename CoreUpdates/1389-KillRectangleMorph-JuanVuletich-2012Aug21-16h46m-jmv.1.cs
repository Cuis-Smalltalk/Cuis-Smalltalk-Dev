'From Cuis 4.0 of 21 April 2012 [latest update: #1386] on 21 August 2012 at 4:49:26 pm'!

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/21/2012 16:25'!
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
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (BorderedRectMorph new color: (Color blue))
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
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

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/21/2012 16:26'!
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
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec fixedWidth: 10);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color blue))
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
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

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/21/2012 16:26'!
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

rect1 := BorderedRectMorph new color: (Color lightOrange); 
	morphWidth: 20; morphHeight: 30.
pane addMorph: rect1.
rect2 := BorderedRectMorph new color: (Color cyan); 
	morphWidth: 20; morphHeight: 30.
pane addMorph: rect2.
pane openInWorld.


pane _ LayoutMorph newRow separation: 5. "2"
pane color: Color lightGreen; morphBoundsInWorld: (320 @ 50 extent: 180 @ 100).
pane addMorph: (StringMorph contents: '2').

rect1 := BorderedRectMorph new color: (Color lightOrange);
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

!classRemoval: #RectangleMorph!
Smalltalk removeClassNamed: #RectangleMorph!
