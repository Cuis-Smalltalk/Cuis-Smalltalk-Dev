'From Cuis7.5 [latest update: #7294] on 27 June 2025 at 2:25:24 pm'!
Smalltalk renameClassNamed: #LayoutSpec as: #LayoutSizeSpec!

!PlacedMorph methodsFor: 'layout-properties' stamp: 'jmv 6/27/2025 14:24:58'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec := LayoutSizeSpec useAll.
	layoutSpec morph: self.

	^ layoutSpec ! !


!BoxMorph methodsFor: 'layout-properties' stamp: 'jmv 6/27/2025 14:24:58'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec := LayoutSizeSpec keepMorphExtent.
	layoutSpec morph: self.

	^ layoutSpec! !


!LayoutMorph methodsFor: 'convenience methods' stamp: 'jmv 6/27/2025 14:24:58'!
addAdjusterAndMorph: aMorph fixedHeight: aNumber
	"Convenience method.
	Add others as necessary."
	self addAdjusterAndMorph: aMorph layoutSpec: (LayoutSizeSpec fixedHeight: aNumber)! !

!LayoutMorph methodsFor: 'convenience methods' stamp: 'jmv 6/27/2025 14:24:57'!
addAdjusterAndMorph: aMorph proportionalHeight: aNumber
	"Convenience method.
	Add others as necessary."
	self addAdjusterAndMorph: aMorph layoutSpec: (LayoutSizeSpec proportionalHeight: aNumber)! !

!LayoutMorph methodsFor: 'convenience methods' stamp: 'jmv 6/27/2025 14:24:57'!
addAdjusterAndMorph: aMorph proportionalWidth: aNumber
	"Convenience method.
	Add others as necessary."
	self addAdjusterAndMorph: aMorph layoutSpec: (LayoutSizeSpec proportionalWidth: aNumber)! !

!LayoutMorph methodsFor: 'convenience methods' stamp: 'jmv 6/27/2025 14:24:57'!
addMorph: aMorph fixedHeight: aNumber
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSizeSpec fixedHeight: aNumber)! !

!LayoutMorph methodsFor: 'convenience methods' stamp: 'jmv 6/27/2025 14:24:58'!
addMorph: aMorph fixedWidth: aNumber
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSizeSpec fixedWidth: aNumber)! !

!LayoutMorph methodsFor: 'convenience methods' stamp: 'jmv 6/27/2025 14:24:57'!
addMorph: aMorph proportionalHeight: aNumber
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSizeSpec proportionalHeight: aNumber)! !

!LayoutMorph methodsFor: 'convenience methods' stamp: 'jmv 6/27/2025 14:24:58'!
addMorph: aMorph proportionalWidth: aNumber
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSizeSpec proportionalWidth: aNumber)! !

!LayoutMorph methodsFor: 'convenience methods' stamp: 'jmv 6/27/2025 14:24:57'!
addMorphKeepMorphHeight: aMorph
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSizeSpec new useMorphHeight)! !

!LayoutMorph methodsFor: 'convenience methods' stamp: 'jmv 6/27/2025 14:24:58'!
addMorphUseAll: aMorph
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: LayoutSizeSpec useAll! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 6/27/2025 14:24:57'!
addAdjusterMorph
	"So the user can adjust layout"
	"twekear para ui grande..."

	| thickness |
	thickness := Theme current layoutAdjusterThickness.

	direction == #horizontal ifTrue: [
		self
			addMorph: LayoutAdjustingMorph new
			layoutSpec: (LayoutSizeSpec fixedWidth: thickness) ].

	direction == #vertical ifTrue: [
		self
			addMorph: LayoutAdjustingMorph new
			layoutSpec: (LayoutSizeSpec fixedHeight: thickness)]! !

!LayoutMorph methodsFor: 'layout in owner' stamp: 'jmv 6/27/2025 14:24:58'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec := LayoutSizeSpec useAll.
	layoutSpec morph: self.

	^ layoutSpec ! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:58'!
example1
"
	self example1
"
| pane row |
pane := (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row := LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSizeSpec fixedWidth: 10); 
	addMorph: (ColoredBoxMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.15);
	addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSizeSpec fixedWidth: 200 fixedHeight: 200).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSizeSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSizeSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSizeSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSizeSpec fixedHeight: 60).
pane position: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:57'!
example10
"
	self example10 openInWorld
"
| pane row |
pane := (LayoutMorph newColumn separation: 5)	name: #example10.
pane color: Color red.

row := LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSizeSpec fixedWidth: 10); 
	addAdjusterMorph; 
	addMorph: (ColoredBoxMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSizeSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSizeSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSizeSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSizeSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSizeSpec fixedHeight: 60).
^ pane! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:58'!
example101VG
"
	self example101VG
"
| pane row |
pane := (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row := LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (Sample09Clock new name: #A)
		layoutSpec: (LayoutSizeSpec fixedWidth: 100); 
	addMorph: (Sample01Star new name: #B)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (Sample03Smiley new name: #C)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.15);
	addMorph: (Sample03Smiley new name: #E)
		layoutSpec: (LayoutSizeSpec fixedWidth: 200 fixedHeight: 200).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color lightRed;
	addMorph: (Sample02Bezier new name: #F)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (Sample02Bezier new  name: #G)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (Sample02Bezier new  name: #H)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSizeSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSizeSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSizeSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSizeSpec fixedHeight: 60).
pane position: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:57'!
example11
"
	self example11
"
| pane row |
pane := (LayoutMorph newColumn separation: 5)	name: #example11.
pane color: Color red.

row := LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSizeSpec fixedWidth: 10); 
	addAdjusterMorph; 
	addMorph: (ColoredBoxMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSizeSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSizeSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSizeSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSizeSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSizeSpec fixedHeight: 60).
pane position: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:57'!
example13
	"
	self example13
	"
	| pane row innerRow |
pane := (LayoutMorph newColumn separation: 5)	name: #example13.
	pane color: Color red.
	row := (LayoutMorph newRow separation: 5) name: #Row.
	innerRow := (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (ColoredBoxMorph new name: #Box1)
			layoutSpec: (LayoutSizeSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (ColoredBoxMorph new name: #Box2)
			layoutSpec: (LayoutSizeSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (ColoredBoxMorph new name: #Bar)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSizeSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSizeSpec fixedWidth: 5); 
		addMorph: (ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSizeSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSizeSpec fixedWidth: 5); 
		addMorph: (ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSizeSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSizeSpec proportionalHeight: 0.9).
	pane position: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:57'!
example1b
"
Based on #example1, but using some ImageMorph instead of RectangleLikeMorph, so extent is not adjustable
	self example1b
"
| pane row |
pane := (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row := LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSizeSpec fixedWidth: 10); 
	addMorph: (ImageMorph new name: #B);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.15);
	addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSizeSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (ImageMorph new name: #G);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (ImageMorph new name: #J);
	addMorph: (ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSizeSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSizeSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSizeSpec fixedHeight: 60).
pane position: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:57'!
example2
	"
	self example2
	"
	| pane row |
	pane := (LayoutMorph newColumn separation: 5)	name: #example2.
	pane color: Color red.
	row := (LayoutMorph newRow separation: 5) name: #Row.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (ColoredBoxMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #A)
			layoutSpec: (LayoutSizeSpec fixedWidth: 20 proportionalHeight: 0.8 offAxisEdgeWeight: #rightOrBottom);
		addMorph: (ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #B)
			layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: (ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #C)
			layoutSpec: (LayoutSizeSpec fixedWidth: 30 proportionalHeight: 0.7 offAxisEdgeWeight: #center).
	pane addMorph: row layoutSpec: (LayoutSizeSpec proportionalHeight: 0.9).
	pane position: 200@180 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:57'!
example20
"
	self example20
"
| column |

column := (LayoutMorph newColumn separation: 5) name: #Column.
column
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8 fixedHeight: 20);
	addMorph: (ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSizeSpec fixedWidth: 40 proportionalHeight: 0.5);
	addMorph: (ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 1.0 fixedHeight: 30).
column position: 150@130 extent: 400@300.
column openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:57'!
example3
	"
	self example3
	"
	| pane row innerRow |
pane := (LayoutMorph newColumn separation: 5)	name: #example3.
	pane color: Color red.
	row := (LayoutMorph newRow separation: 5) name: #Row.
	innerRow := (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (ColoredBoxMorph new name: #Box1)
			layoutSpec: (LayoutSizeSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (ColoredBoxMorph new name: #Box2)
			layoutSpec: (LayoutSizeSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (ColoredBoxMorph new name: #Bar)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSizeSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: (ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSizeSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: (ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSizeSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSizeSpec proportionalHeight: 0.9).
	pane position: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:57'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane := LayoutMorph newRow separation: 5. "1"
pane addMorph: (LabelMorph contents: '1').

rect1 := ColoredBoxMorph new color: (Color lightOrange); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect1.
rect2 := ColoredBoxMorph new color: (Color cyan); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect2.
pane
	color: Color lightGreen;
	position: 120 @ 50 extent: 180 @ 100;
	openInWorld.

pane := LayoutMorph newRow separation: 5. "2"
pane addMorph: (LabelMorph contents: '2').

rect1 := ColoredBoxMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSizeSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect1.
rect2 := ColoredBoxMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSizeSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane
	color: Color lightGreen;
	position: 320 @ 50 extent: 180 @ 100;
	openInWorld.


pane := LayoutMorph newRow separation: 5. "3"
pane addMorph: (LabelMorph contents: '3').

rect1 := ColoredBoxMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSizeSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
rect2 := ColoredBoxMorph new color: (Color cyan);
	layoutSpec: (LayoutSizeSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect2.
pane
	color: Color lightGreen;
	position: 520 @ 50 extent: 180 @ 100;
	openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:57'!
exampleEdgesColumn
	"Show 2 cases :1 useEdgeSpace true and 1 false"
	" self exampleEdgesColumn "
	| withEdge noEdge upSpec downSpec |
	withEdge := self newColumn :: padding: (120 @ 40);  gap: 20.
	noEdge := self newColumn ::  padding: (0 @ 0);  gap: 20.
	upSpec := LayoutSizeSpec keepMorphExtent :: offAxisEdgeWeight: 0.
	upSpec proportionalWidth: 0.8 minimum: 200; proportionalHeight: 0.8 minimum: 200.
	downSpec := LayoutSizeSpec keepMorphExtent :: offAxisEdgeWeight: 1.
	downSpec proportionalWidth: 0.5 minimum: 200; proportionalHeight: 0.5 minimum: 200.
	
	withEdge addMorph: (ColoredBoxMorph new :: color: Color blue;  layoutSpec: upSpec).
	noEdge addMorph: (ColoredBoxMorph new :: color: Color blue;  layoutSpec: upSpec).
	withEdge addMorph: (LabelMorph new :: contents: 'WITH Padding').
	noEdge addMorph: (LabelMorph new :: contents: 'withOUT Padding').
	withEdge addMorph: (ColoredBoxMorph new :: layoutSpec: downSpec).
	noEdge addMorph: (ColoredBoxMorph new ::  layoutSpec: downSpec).
	
	withEdge openInWorld.
	noEdge openInWorld.! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/27/2025 14:24:57'!
exampleEdgesRow
	"Show 2 cases: 1 useEdgeSpace true and 1 false"
	" self exampleEdgesRow "
	| withEdge noEdge upSpec downSpec |
	withEdge := self newRow :: padding: (120 @ 40);  gap: 20.
	noEdge := self newRow :: padding: (0 @ 0);  gap: 20.
	upSpec := LayoutSizeSpec keepMorphExtent :: offAxisEdgeWeight: 0.
	upSpec proportionalWidth: 0.8 minimum: 100; proportionalHeight: 0.8 minimum: 100.
	downSpec := LayoutSizeSpec keepMorphExtent :: offAxisEdgeWeight: 1.
	downSpec proportionalWidth: 0.5 minimum: 100; proportionalHeight: 0.5 minimum: 100.
	
	withEdge addMorph: (ColoredBoxMorph new :: color: Color blue;  layoutSpec: upSpec).
	noEdge addMorph: (ColoredBoxMorph new :: color: Color blue;  layoutSpec: upSpec).
	withEdge addMorph: (LabelMorph new :: contents: 'WITH Padding').
	noEdge addMorph: (LabelMorph new :: contents: 'withOUT Padding').
	withEdge addMorph: (ColoredBoxMorph new :: layoutSpec: downSpec).
	noEdge addMorph: (ColoredBoxMorph new ::  layoutSpec: downSpec).
	
	withEdge openInWorld.
	noEdge openInWorld.! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 6/27/2025 14:24:57'!
initialize
	super initialize.
	viewBox := LayoutMorph newRow color: self defaultColor.
	self
		addMorph: (clock := UpdatingLabelMorph initializedInstance)
		layoutSpec:  (LayoutSizeSpec morphWidthProportionalHeight: 0.5).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSizeSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			offAxisEdgeWeight: #rightOrBottom).
	viewBox separation: self defaultHeight // 8.
	self setBalloonText: [ 
		'About this system:' bold, `String newLineString`,
		Smalltalk systemInformationString ].
	clock
		setProperty: #handlesMouseDown: toValue: true;
		setProperty: #'mouseButton1Down:localPosition:' 
		toValue: [ :evt : pos | | menu taskbarEdge point | 
			self runningWorld ifNotNil: [ :w |
				taskbarEdge := self positionInWorld y.
				menu := (Preferences at: #worldMenu) getMenuIn: w at: w activeHand.	
				menu hide.
				w addMorph: menu. ": Otherwise we do not have a proper extent"
				point := 0 @ (taskbarEdge - menu extentInWorld y).
				menu popUpAt: point allowKeyboard: false ] ]! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 6/27/2025 14:24:57'!
addStayUpIcons
	| closeButton pinButton w buttonHW |
	(Preferences at: #optionalButtons) ifFalse: [ ^self ].
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	buttonHW := Theme current titleBarButtonsExtent x.
	closeButton := PluggableButtonMorph model: self action: #delete.
	closeButton iconDrawSelector: #drawCloseIcon; noButtonShape.
	pinButton := PluggableButtonMorph model: self action: #stayUp.
	pinButton iconDrawSelector: #drawPushPinIcon; noButtonShape.
	w := (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + (buttonHW*2).
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: buttonHW * 12 // 10);
			color: `Color transparent`;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedWidth: buttonHW//10;
			addMorph: (closeButton layoutSpec: (LayoutSizeSpec fixedWidth: buttonHW fixedHeight: buttonHW));
			addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: pinButton fixedWidth: buttonHW;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedWidth: buttonHW//3).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 6/27/2025 14:24:57'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout updateReqButton |
	requirements := PluggableListMorph
		withModel: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
		
	deleteReqButton := (PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'delete')
							setBalloonText: 'Remove selected Feature requirement'.
	deleteReqButton color: self widgetsColor.
	updateReqButton := (PluggableButtonMorph 
							model: requirements model
							action: #updateSelectedRequirement 
							label: 'update')
							setBalloonText: 'Update requirement to current Feature revision'.
	updateReqButton color: self widgetsColor.
							
	buttonLayout := LayoutMorph newRow.
	buttonLayout 
		addMorph: deleteReqButton 
		layoutSpec: (LayoutSizeSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop);
		color: self widgetsColor quiteWhiter;
		addMorph: updateReqButton 
		layoutSpec: (LayoutSizeSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop);
		color: self widgetsColor quiteWhiter.
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LayoutMorph newRow.
	^ reqLayout 
		doAdoptWidgetsColor;
		addMorph: requirements 
			layoutSpec: (LayoutSizeSpec 
							proportionalWidth: 0.8
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #leftOrTop);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSizeSpec 
							proportionalWidth: 0.2 
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #rightOrBottom);
		color: `Color transparent`;
		yourself
		! !


!LayoutSizeSpec class methodsFor: 'instance creation' stamp: 'jmv 6/27/2025 14:24:57'!
fixedWidth: aNumber fixedHeight: otherNumber
	^ LayoutSizeSpec new
		fixedWidth: aNumber;
		fixedHeight: otherNumber! !

!LayoutSizeSpec class methodsFor: 'instance creation' stamp: 'jmv 6/27/2025 14:24:57'!
fixedWidth: aNumber fixedHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber
	^ LayoutSizeSpec new
		fixedWidth: aNumber;
		fixedHeight: otherNumber;
		offAxisEdgeWeight: aSymbolOrNumber! !


!PlacedMorph reorganize!
('accessing' location location:)
('geometry' adjustExtent adjustExtentFor: allocHeightForFactor:minLayoutExtentCache: allocWidthForFactor:minLayoutExtentCache: externalizeBoundingRectOf: externalizeBoundsToWorld: externalizeDistance: fitInto: internalizeDistance: morphAlign:with: morphPosition morphPosition: orbitBy: positionInWorld: referencePosition referencePositionInOwner rotateBy: rotation rotation: rotation:scale: rotationCenter scale scale: scaleBy:)
('initialization' initialize openInWorld:)
('dropping/grabbing' aboutToBeGrabbedBy:)
('layout' minimumLayoutExtent: minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth)
('geometry testing' hasOwnLocation isOrAnyOwnerIsRotated yAxisPointsUp)
('private' fixYAxisDirection privateOwner: privatePosition:)
('layout-properties' layoutSpec layoutSpec:)
('testing' isPlacedMorph isProportionalHeight isProportionalWidth)
('category-name' internalize:)
('class membership' externalize:)
!


!BoxMorph reorganize!
('initialization' defaultExtent initialize)
('drawing' drawOn:)
('geometry' ensureMinimumExtent extentChanged: extentInOwner: extentInWorld extentInWorld: fitInto: fullBoundsInOwner heightInOwner: localBounds localBoundsForError minimumExtent morphBottomLeft morphBottomRight morphExtent morphExtent: morphHeight morphHeight: morphTopLeft morphTopRight morphWidth morphWidth: position:extent: privateExtent: rotationCenter topLeftInOwner widthInOwner:)
('geometry services' coversLocalPoint: coversPixel: includesPixel:)
('geometry testing' hasVariableExtent knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('layout' minItemWidth)
('layout-properties' layoutSpec)
('testing' is:)
('halos and balloon help' haloShowsCoordinateSystem okayToResizeEasily okayToRotateEasily okayToScaleEasily)
('window like behavior' fullScreen resize:)
('private' adjustExtentFor: basicAdjustExtentFor:)
!


!LayoutMorph reorganize!
('accessing' addMorphFrontFromWorldPosition: adoptWidgetsColor: axisEdgeWeight axisEdgeWeight: borderWidth: direction direction: gap gap: padding: separation:)
('adjust' adjustBy:at: adjustHorizontallyBy:at: adjustVerticallyBy:at:)
('convenience methods' addAdjusterAndMorph:fixedHeight: addAdjusterAndMorph:layoutSpec: addAdjusterAndMorph:proportionalHeight: addAdjusterAndMorph:proportionalWidth: addMorph:fixedHeight: addMorph:fixedWidth: addMorph:proportionalHeight: addMorph:proportionalWidth: addMorphKeepMorphHeight: addMorphUseAll: addMorphs: addMorphs:widthProportionalTo:)
('geometry' desiredLayoutHeight extentChanged: minimumExtent minimumExtent: someSubmorphPositionOrExtentChanged)
('initialization' beColumn beRow defaultBorderWidth defaultColor defaultGap doAdoptWidgetsColor doClipSubmorphs initialize)
('layout' heightsFor:within:minLayoutExtentCache: layoutBounds layoutSubmorphs layoutSubmorphsHorizontallyIn: layoutSubmorphsIfNeeded layoutSubmorphsVerticallyIn: offAxisHeightFor:within:minLayoutExtentCache: offAxisWidthFor:within:minLayoutExtentCache: offAxisXOffsetFor:within: offAxisYOffsetFor:within: submorphsToLayout widthsFor:within:minLayoutExtentCache:)
('submorphs-add/remove' addAdjusterMorph addMorph:layoutSpec: addMorphBack: addMorphFront: removedMorph:)
('testing' is: isColumn isRow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex:position:)
('layout in owner' layoutSpec)
('geometry testing' clipsSubmorphs)
('drawing' drawOn:)
!


!LayoutMorph class reorganize!
('instance creation' initializedInstance new newColumn newRow)
('examples' example1 example10 example101VG example11 example13 example1b example2 example20 example3 example6 example7 exampleEdgesColumn exampleEdgesRow iconsExample launcherExample)
('new-morph participation' categoryInNewMorphMenu)
!


!TaskbarMorph reorganize!
('accessing' buttonFor: scale)
('events' screenSizeChanged)
('initialization' defaultColor defaultHeight defaultScale initialize)
('testing' is: isSticky)
('services' aboutToCollapse: addButtonFor: refreshTaskbarFor: removeButtonFor: restoreAll wasDeleted: wasOpened:)
('submorphs-add/remove' delete noteNewOwner:)
('menu' getMenu)
('resize' scale: scaleNormal scaleSmall scaleX2 scaleX4)
('notifications' fontPreferenceChanged)
('geometry' basicDisplayBounds)
('event handling testing' handlesMouseDown:)
!


!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary defaultLayerNumber items itemsDo: label lastItem stayUp stayUp:)
('construction' add:action: add:action:argument: add:action:balloonText: add:action:icon: add:action:icon:enabled: add:subMenu: add:target:action: add:target:action:argument: add:target:action:argument:icon: add:target:action:argumentList: add:target:action:icon: add:targetHighlight:action:argumentList: addItemFromDictionary: addItemFromDictionary:targeting: addItemFromDictionaryOrNil: addItemsFromDictionaries: addLabel: addLine addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:action:argumentList: defaultTarget: labels:lines:selections:)
('control' activeSubmenu: deleteIfPopUp: popUpAdjacentTo:from: popUpAt:allowKeyboard: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem: wantsToBeDroppedInto:)
('dropping/grabbing' justDroppedInto:event:)
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('events-processing' handleFocusEvent:)
('geometry' fontPreferenceChanged)
('initialization' defaultBorderWidth defaultColor delete initialize intoWorld:)
('keyboard control' displayFiltered: keyboardFocusChange: moveSelectionDown:event:)
('menu' removeStayUpBox)
('modal control' invokeModal invokeModal: isModalInvokationDone isModalInvokationDone: modalSelection modalSelection:)
('testing' isIncludedInTaskbar)
('private' adjustSubmorphsLayout fitInWorld selectedItem)
('drawing' drawOn:)
('misc' activate:)
('modal progress dialog' displayAt:during: informUserAt:during:)
('halos and balloon help' addHalo:)
!


!CodePackageListWindow reorganize!
('GUI building' buildButtonPane buildMorphicWindow buildRequirementsPane initialExtent windowColor)
('commands' addRequirement browse browseChanges createPackage deletePackage)
('geometry' fontPreferenceChanged)
!


!LayoutSizeSpec class reorganize!
('instance creation' fixedExtent: fixedHeight: fixedWidth: fixedWidth:fixedHeight: fixedWidth:fixedHeight:offAxisEdgeWeight: fixedWidth:proportionalHeight: fixedWidth:proportionalHeight:offAxisEdgeWeight: keepMorphExtent morphHeightFixedWidth: morphHeightProportionalWidth: morphWidthFixedHeight: morphWidthProportionalHeight: proportionalHeight: proportionalWidth: proportionalWidth:fixedHeight: proportionalWidth:fixedHeight:offAxisEdgeWeight: proportionalWidth:proportionalHeight: proportionalWidth:proportionalHeight:offAxisEdgeWeight: useAll)
!

