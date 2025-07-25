'From Cuis7.5 [latest update: #7372] on 25 July 2025 at 10:01:19 am'!
!classDefinition: #FormLayoutMorph category: #'Morphic-Layouts'!
LayoutMorph subclass: #FormLayoutMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/25/2025 09:56:26'!
initializedInstance

	^LinearLayoutMorph newRow! !

!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/25/2025 09:57:23'!
new

	^self error: 'Use LayoutMorph #newRow, #newColumn or #newForm'.! !

!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/25/2025 09:58:04'!
newColumn
	^LinearLayoutMorph basicNew initializeColumn! !

!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/25/2025 09:56:48'!
newForm
	^FormLayoutMorph basicNew initializeForm! !

!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/25/2025 09:58:09'!
newRow
	^LinearLayoutMorph basicNew initializeRow! !

!LayoutMorph class methodsFor: 'new-morph participation' stamp: 'jmv 7/25/2025 09:58:18'!
categoryInNewMorphMenu
	^ 'Layouts'! !

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:38'!
example1
"
	self example1
"
| pane row |
pane := (LinearLayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row := LinearLayoutMorph newRow name: #Row1.
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

row := (LinearLayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LinearLayoutMorph newRow separation: 5) name: #Row3.
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:38'!
example10
"
	self example10 openInWorld
"
| pane row |
pane := (LinearLayoutMorph newColumn separation: 5)	name: #example10.
pane color: Color red.

row := LinearLayoutMorph newRow name: #Row1.
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

row := (LinearLayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LinearLayoutMorph newRow separation: 5) name: #Row3.
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:38'!
example101VG
"
	self example101VG
"
| pane row |
pane := (LinearLayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row := LinearLayoutMorph newRow name: #Row1.
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

row := (LinearLayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color lightRed;
	addMorph: (Sample02Bezier new name: #F)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (Sample02Bezier new  name: #G)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (Sample02Bezier new  name: #H)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LinearLayoutMorph newRow separation: 5) name: #Row3.
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:38'!
example11
"
	self example11
"
| pane row |
pane := (LinearLayoutMorph newColumn separation: 5)	name: #example11.
pane color: Color red.

row := LinearLayoutMorph newRow name: #Row1.
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

row := (LinearLayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LinearLayoutMorph newRow separation: 5) name: #Row3.
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:38'!
example13
	"
	self example13
	"
	| pane row innerRow |
pane := (LinearLayoutMorph newColumn separation: 5)	name: #example13.
	pane color: Color red.
	row := (LinearLayoutMorph newRow separation: 5) name: #Row.
	innerRow := (LinearLayoutMorph newRow separation: 5) name: #InnerRow;
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:38'!
example1b
"
Based on #example1, but using some ImageMorph instead of RectangleLikeMorph, so extent is not adjustable
	self example1b
"
| pane row |
pane := (LinearLayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row := LinearLayoutMorph newRow name: #Row1.
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

row := (LinearLayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (ImageMorph new name: #G);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LinearLayoutMorph newRow separation: 5) name: #Row3.
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:38'!
example2
	"
	self example2
	"
	| pane row |
	pane := (LinearLayoutMorph newColumn separation: 5)	name: #example2.
	pane color: Color red.
	row := (LinearLayoutMorph newRow separation: 5) name: #Row.
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:38'!
example20
"
	self example20
"
| column |

column := (LinearLayoutMorph newColumn separation: 5) name: #Column.
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:39'!
example3
	"
	self example3
	"
	| pane row innerRow |
pane := (LinearLayoutMorph newColumn separation: 5)	name: #example3.
	pane color: Color red.
	row := (LinearLayoutMorph newRow separation: 5) name: #Row.
	innerRow := (LinearLayoutMorph newRow separation: 5) name: #InnerRow;
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:39'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane := LinearLayoutMorph newRow separation: 5. "1"
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

pane := LinearLayoutMorph newRow separation: 5. "2"
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


pane := LinearLayoutMorph newRow separation: 5. "3"
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:39'!
example7
	"
	Useful example contributed by Ken Dickey
	self example7
	"
	"============================================"
	| c colorHexValue colorName r w |
	w := SystemWindow new.
	r := LinearLayoutMorph newRow separation: 30 @ 10.
	c := LinearLayoutMorph newColumn separation: 20 @ 10.
	colorHexValue := LabelMorph contents:  'F97306'.
	colorName := LabelMorph contents: 'cornSilk'.

	r addMorph: colorHexValue.
	r addMorph: colorName.
	c addMorph: ColoredBoxMorph new.
	c addMorph: r.
	w addMorph: c .
	w openInWorld.
	"============================================"! !

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:39'!
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:39'!
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

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:39'!
iconsExample
	"
	LayoutMorph iconsExample
	"
	| lm ilm counter |
	lm := LinearLayoutMorph newColumn.
	ilm := LinearLayoutMorph newRow.
	counter := 1.
	Theme methodsDo: [ :method | | sel | 
		(sel := method selector) numArgs = 0 ifTrue: [
			(sel endsWith: 'Icon') ifTrue: [ | mayBeForm |
				mayBeForm := Theme current perform: sel.
				(mayBeForm isKindOf: Form) ifTrue: [ | imageMorph |
					counter \\ 10 = 0 ifTrue: [
						lm addMorph: ilm.
						ilm := LinearLayoutMorph newRow ].
					counter := counter + 1.
					ilm addMorph: (imageMorph := ImageMorph newWith: (mayBeForm magnifyBy: 2)).
					imageMorph 
						setBalloonText: 'Copy ''', sel storeString bold, ''' to the Clipboard';
						setProperty: #'handlesMouseDown:' toValue: true;
						setProperty: #mouseButton1Down:localPosition:  toValue: [ :event :pos |
							Clipboard storeObject: sel storeString ] ] ] ] ].
	lm 
		setProperty: #taskbarButtonIcon toValue: Theme current imageIcon;
		setBalloonText: 'Drag me here!!'.
	lm openInWorld
! !

!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:58:39'!
launcherExample
	"
	self launcherExample
	"
	| b1 b2 b3 row b4 random buttons |
	random := Random new.
	b1 := PluggableButtonMorph model: [ Date today print ] action: #value label: 'Date'.
	b2 := PluggableButtonMorph model: [ Time now print ] action: #value label: 'Time'.
	b3 := PluggableButtonMorph model: [ SystemVersion current print ] action: #value label: 'Version'.
	b4 := PluggableButtonMorph model: [ random next print ] action: #value label: 'Random'.
	buttons := {b1. b2. b3. b4}.
	buttons do: [ :button |
		button color: `Color lightRed` ].
	row := LinearLayoutMorph newRow
		 color: `Color red`;
		 addMorphs: buttons;
		 morphExtent: 300 @ 40.
	^ row openInWorld! !

!LayoutMorph class methodsFor: 'FormLayout examples' stamp: 'jmv 7/25/2025 09:59:06'!
example1Form
"
	self example1Form
"
	| pane |
	pane := (FormLayoutMorph newForm separation: 5) name: #example1.
	pane 
		color: Color red;
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #A)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@20 brEdgesWeight: 0@0 offset: 100@40);
		addMorph: (ColoredBoxMorph new color: (Color blue); name: #B)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@0 offset: 30@20 brEdgesWeight: 1@0 offset: -30@40);
		addMorph: (ColoredBoxMorph new color: (Color lightGreen); name: #C)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@60 brEdgesWeight: 0.5@0 offset: -10@140);
		addMorph: (ColoredBoxMorph new color: (Color green); name: #D)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@0 offset: 10@60 brEdgesWeight: 1@0 offset: -30@140);
		addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@160 brEdgesWeight: 1@1 offset: -30@ -30).
	pane position: 150@130 extent: 400@300.
	pane openInWorld! !


!FormLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:55:25'!
initializeForm

	layoutObject := FormLayout new.
	self initialize.! !

!methodRemoval: LinearLayoutMorph class #example13 stamp: 'jmv 7/25/2025 09:58:38'!
LinearLayoutMorph class removeSelector: #example13!
!methodRemoval: LinearLayoutMorph class #newRow stamp: 'jmv 7/25/2025 09:57:36'!
LinearLayoutMorph class removeSelector: #newRow!
!methodRemoval: LinearLayoutMorph class #newForm stamp: 'jmv 7/25/2025 09:56:36'!
LinearLayoutMorph class removeSelector: #newForm!
!methodRemoval: LinearLayoutMorph class #example1b stamp: 'jmv 7/25/2025 09:58:38'!
LinearLayoutMorph class removeSelector: #example1b!
!methodRemoval: LinearLayoutMorph class #exampleEdgesRow stamp: 'jmv 7/25/2025 09:58:39'!
LinearLayoutMorph class removeSelector: #exampleEdgesRow!
!methodRemoval: LinearLayoutMorph class #example11 stamp: 'jmv 7/25/2025 09:58:38'!
LinearLayoutMorph class removeSelector: #example11!
!methodRemoval: LinearLayoutMorph class #exampleEdgesColumn stamp: 'jmv 7/25/2025 09:58:39'!
LinearLayoutMorph class removeSelector: #exampleEdgesColumn!
!methodRemoval: LinearLayoutMorph class #example1 stamp: 'jmv 7/25/2025 09:58:38'!
LinearLayoutMorph class removeSelector: #example1!
!methodRemoval: LinearLayoutMorph class #example1Form stamp: 'jmv 7/25/2025 09:58:44'!
LinearLayoutMorph class removeSelector: #example1Form!
!methodRemoval: LinearLayoutMorph class #launcherExample stamp: 'jmv 7/25/2025 09:58:39'!
LinearLayoutMorph class removeSelector: #launcherExample!
!methodRemoval: LinearLayoutMorph class #example101VG stamp: 'jmv 7/25/2025 09:58:38'!
LinearLayoutMorph class removeSelector: #example101VG!
!methodRemoval: LinearLayoutMorph class #example20 stamp: 'jmv 7/25/2025 09:58:39'!
LinearLayoutMorph class removeSelector: #example20!
!methodRemoval: LinearLayoutMorph class #iconsExample stamp: 'jmv 7/25/2025 09:58:39'!
LinearLayoutMorph class removeSelector: #iconsExample!
!methodRemoval: LinearLayoutMorph class #new stamp: 'jmv 7/25/2025 09:57:03'!
LinearLayoutMorph class removeSelector: #new!
!methodRemoval: LinearLayoutMorph class #example6 stamp: 'jmv 7/25/2025 09:58:39'!
LinearLayoutMorph class removeSelector: #example6!
!methodRemoval: LinearLayoutMorph class #categoryInNewMorphMenu stamp: 'jmv 7/25/2025 09:58:18'!
LinearLayoutMorph class removeSelector: #categoryInNewMorphMenu!
!methodRemoval: LinearLayoutMorph class #example10 stamp: 'jmv 7/25/2025 09:58:38'!
LinearLayoutMorph class removeSelector: #example10!
!methodRemoval: LinearLayoutMorph class #example7 stamp: 'jmv 7/25/2025 09:58:39'!
LinearLayoutMorph class removeSelector: #example7!
!methodRemoval: LinearLayoutMorph class #initializedInstance stamp: 'jmv 7/25/2025 09:56:16'!
LinearLayoutMorph class removeSelector: #initializedInstance!
!methodRemoval: LinearLayoutMorph class #newColumn stamp: 'jmv 7/25/2025 09:57:31'!
LinearLayoutMorph class removeSelector: #newColumn!
!methodRemoval: LinearLayoutMorph class #example3 stamp: 'jmv 7/25/2025 09:58:39'!
LinearLayoutMorph class removeSelector: #example3!
!methodRemoval: LinearLayoutMorph class #example2 stamp: 'jmv 7/25/2025 09:58:38'!
LinearLayoutMorph class removeSelector: #example2!
!methodRemoval: LinearLayoutMorph #initializeForm stamp: 'jmv 7/25/2025 09:55:28'!
LinearLayoutMorph removeSelector: #initializeForm!

!LayoutMorph class reorganize!
('instance creation' initializedInstance new newColumn newForm newRow)
('new-morph participation' categoryInNewMorphMenu)
('LinearLayout examples' example1 example10 example101VG example11 example13 example1b example2 example20 example3 example6 example7 exampleEdgesColumn exampleEdgesRow iconsExample launcherExample)
('FormLayout examples' example1Form)
!


!FormLayoutMorph reorganize!
('initialization' initializeForm)
!


!FormLayoutMorph class reorganize!
('instance creation' new)
!


!LinearLayoutMorph class reorganize!
!

