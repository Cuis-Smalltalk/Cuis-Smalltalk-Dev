Class {
	#name : 'LayoutMorph',
	#superclass : 'BorderedBoxMorph',
	#instVars : [
		'adoptsWidgetsColor',
		'layoutObject'
	],
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'new-morph participation' }
LayoutMorph class >> categoryInNewMorphMenu [
	^ 'Layouts'
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> example1 [
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
pane openInWorld
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> example10 [
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
^ pane
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> example101VG [
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
pane position: 150@130 extent: 800@600.
pane openInWorld
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> example11 [
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
pane openInWorld
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> example13 [
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
	pane openInWorld
]

{ #category : 'FormLayout examples' }
LayoutMorph class >> example1Form [
"
	self example1Form
"
	| pane |
	pane := (FormLayoutMorph newForm padding: 5) name: #example1.
	pane 
		color: Color red;
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #A)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@20 brEdgesWeight: 0@0 offset: 100@40);
		addMorph: (ColoredBoxMorph new color: (Color blue); name: #B)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@0 offset: 30@20 brEdgesWeight: 1@0 offset: -30@40);
		addMorph: (ColoredBoxMorph new color: (Color lightGreen); name: #C)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@60 brEdgesWeight: 0.5@0 offset: -10@120);
		addMorph: (ColoredBoxMorph new color: (Color green); name: #D)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@0 offset: 10@60 brEdgesWeight: 1@0 offset: -30@120);
		addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@140 brEdgesWeight: 1@1 offset: -30@ -70).
	pane
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #F)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@1 offset: 30@ -60 brEdgesWeight: 0@1 offset: 100@ -40);
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #G)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@1 offset: 110@ -60 brEdgesWeight: 0.5@1 offset: -10@ -40);
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #H)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@1 offset: 10@ -60 brEdgesWeight: 1@1 offset: -30@ -40).
	pane
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #I)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@1 offset: 30@ -30 brEdgesWeight: 0@1 offset: 100@ -10);
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #J)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@1 offset: 110@ -30 brEdgesWeight: 0.5@1 offset: -10@ -10);
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #K)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@1 offset: 10@ -30 brEdgesWeight: 1@1 offset: -30@ -10).
	pane position: 150@130 extent: 400@300.
	pane openInWorld
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> example1b [
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
pane openInWorld
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> example2 [
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
	pane openInWorld
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> example20 [
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
column openInWorld
]

{ #category : 'FormLayout examples' }
LayoutMorph class >> example2Form [
"
	self example2Form
"
	| pane |
	pane := FormLayoutMorph newForm padding: 0 :: name: #example2.
	pane 
		color: Color red;
		addMorph: (ColoredBoxMorph new ::  color: Color white; name: #canvas;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 0@0 offset: 0@0 
				brEdgesWeight: 1@1 offset: -50@-50) );
		addMorph: (ColoredBoxMorph new ::  color: Color blue; name: #zoomWheel;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 1@0 offset: -50@0 
				brEdgesWeight: 1@1 offset: 0@-50) );
		addMorph: (ColoredBoxMorph new :: color: Color lightGreen; name: #horizontalWheel;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 0@1 offset: 0@-50 
				brEdgesWeight: 1@1 offset: -50@0) ).
	pane position: 150@130 extent: 400@300.
	pane openInWorld
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> example3 [
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
	pane openInWorld
]

{ #category : 'FormLayout examples' }
LayoutMorph class >> example3Form [
"
	self example3Form
"
	| pane |
	pane := FormLayoutMorph newForm padding: 0 :: name: #example3.
	pane 
		color: Color red;
		addMorph: (ColoredBoxMorph new :: color: Color white; name: #canvas;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 0@0 offset: 0@0 
				brEdgesWeight: 1@1 offset: -50@-50) );
		addMorph: (ColoredBoxMorph new :: color: Color blue; name: #zoomWheelCentered150px;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 1@0.5 offset: -50@-75 
				brEdgesWeight: 1@0.5 offset: 0@75) );
		addMorph: (ColoredBoxMorph new :: color: Color lightGreen; name: #horizontalWheelCentered150px;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 0.5@1 offset: -75@-50 
				brEdgesWeight: 0.5@1 offset: 75@0) ).
	pane position: 150@130 extent: 400@300.
	pane openInWorld
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> example6 [
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
	openInWorld
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> example7 [
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
	"============================================"
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> exampleEdgesColumn [
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
	noEdge openInWorld.
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> exampleEdgesRow [
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
	noEdge openInWorld.
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> iconsExample [
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
		setBalloonText: 'Drag me here!'.
	lm openInWorld

]

{ #category : 'instance creation' }
LayoutMorph class >> initializedInstance [
	^LinearLayoutMorph newRow
]

{ #category : 'LinearLayout examples' }
LayoutMorph class >> launcherExample [
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
	^ row openInWorld
]

{ #category : 'instance creation' }
LayoutMorph class >> newColumn [
	^LinearLayoutMorph newColumn
]

{ #category : 'instance creation' }
LayoutMorph class >> newForm [
	^FormLayoutMorph new
]

{ #category : 'instance creation' }
LayoutMorph class >> newRow [
	^LinearLayoutMorph newRow
]

{ #category : 'submorphs-add/remove' }
LayoutMorph >> addMorph: aMorph layoutSpec: aLayoutSpec [
	"Add a submorph, with aLayoutSpec.
	Subclasses validate that aLayoutSpec is appropriate for them before calling super."

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec: aLayoutSpec.
	self addMorphFront: aMorph.
]

{ #category : 'submorphs-add/remove' }
LayoutMorph >> addMorphBack: aMorph [
	"Add a submorph, at the top or left, with a default LayoutSpec if none was provided."

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph assureLayoutSpecForOwner: self.
	super addMorphBack: aMorph.
]

{ #category : 'submorphs-add/remove' }
LayoutMorph >> addMorphFront: aMorph [
	"Add a submorph, at the bottom or right, with a default LayoutSpec if none was provided."

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph assureLayoutSpecForOwner: self.
	super addMorphFront: aMorph
]

{ #category : 'submorphs-add/remove' }
LayoutMorph >> addMorphFrontFromWorldPosition: aMorph [

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph assureLayoutSpecForOwner: self.
	self addMorphFront: aMorph.
	self layoutSubmorphs.

]

{ #category : 'accessing' }
LayoutMorph >> adoptWidgetsColor: paneColor [
	super adoptWidgetsColor: paneColor.
	adoptsWidgetsColor
		ifTrue: [ self color: (Theme current buttonColorFrom: color) ]
		ifFalse: [ self color: `Color transparent` ]
]

{ #category : 'accessing' }
LayoutMorph >> borderWidth: anInteger [

	super borderWidth: anInteger.
	self layoutNeeded: true.
]

{ #category : 'geometry testing' }
LayoutMorph >> clipsSubmorphs [
	"LayoutMorphs usually not do not clip submorphs.
	A special case is Transcript. It doesn't include a ScrollPane (that usually does clipping).
	Note: for this to work, we can't be transparent, as transparent morphs can't clip submorphs.
		Senders may also call #doAdoptWidgetsColor.
	There may be other similar cases in the future."

	^self hasProperty: #doClipSubmorphs.
]

{ #category : 'initialization' }
LayoutMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 0
]

{ #category : 'initialization' }
LayoutMorph >> defaultColor [
	^ Theme current defaultWindowColor
]

{ #category : 'initialization' }
LayoutMorph >> defaultGap [
	"Answer the default gap between submorphs for the receiver.
	Redefine this method for a different initial gap in your morphs.
	Only for Linear Layouts"

	^ 0
]

{ #category : 'layout in owner' }
LayoutMorph >> defaultLayoutSizeSpec [
	"Layout specific. Return a default layout spec describing where the
	receiver should appear in a LinearLayout."

	^LayoutSizeSpec useAll
]

{ #category : 'aux' }
LayoutMorph >> defaultLayoutSpecFor: aMorph [
	^self subclassResponsibility
]

{ #category : 'initialization' }
LayoutMorph >> doAdoptWidgetsColor [
	adoptsWidgetsColor := true.
]

{ #category : 'initialization' }
LayoutMorph >> doClipSubmorphs [
	self setProperty: #doClipSubmorphs toValue: true.
]

{ #category : 'drawing' }
LayoutMorph >> drawOn: aCanvas [
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.
]

{ #category : 'geometry' }
LayoutMorph >> extentChanged: oldExtent [
	"Our extent changed. Must layout submorphs again."

	self layoutNeeded: true.
]

{ #category : 'event handling testing' }
LayoutMorph >> handlesKeyboardFocus [
	"Some subclasses may want to answer true, though."

	^false
]

{ #category : 'initialization' }
LayoutMorph >> initialize [
	super initialize.
	adoptsWidgetsColor := false.
]

{ #category : 'testing' }
LayoutMorph >> is: aSymbol [
	^ aSymbol == #LayoutMorph or: [ super is: aSymbol ]
]

{ #category : 'testing' }
LayoutMorph >> isLayoutSpecOk: aLayoutSpecOrNil [
	"Answer whether the argument can work with us."
	
	^self subclassResponsibility
]

{ #category : 'layout' }
LayoutMorph >> layoutBounds [
	"Return the bounds for laying out children of the receiver"

	^ self interiorOrigin extent: self interiorExtent 
]

{ #category : 'layout' }
LayoutMorph >> layoutSubmorphs [
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		self layoutNeeded: false.
		^self ].

	"Invariant: morphExtent >=  minimumLayoutExtent"
	self adjustExtentFor: nil.

	layoutObject layoutMorphs: submorphs in: self layoutBounds.
	self layoutNeeded: false.
]

{ #category : 'layout' }
LayoutMorph >> layoutSubmorphsIfNeeded [
	"Recompute the layout if necessary."

	self isLayoutNeeded ifTrue: [
		self layoutSubmorphs ].
]

{ #category : 'accessing' }
LayoutMorph >> padding: numberOrPoint [
	"Honored by all kinds of Layouts"

	super padding: numberOrPoint.
	self layoutNeeded: true.
]

{ #category : 'private' }
LayoutMorph >> privateAddAllMorphs: aCollection atIndex: index [

	aCollection do: [ :m |
		m isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
		m assureLayoutSpecForOwner: self ].
	^ super privateAddAllMorphs: aCollection atIndex: index.
]

{ #category : 'private' }
LayoutMorph >> privateAddMorph: aMorph atIndex: index position: aPoint [

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph assureLayoutSpecForOwner: self.
	super privateAddMorph: aMorph atIndex: index position: aPoint.
]

{ #category : 'submorphs-add/remove' }
LayoutMorph >> removedMorph: aMorph [
	"One of my submorphs has been removed."

	super removedMorph: aMorph.
	self layoutSubmorphs
]

{ #category : 'geometry' }
LayoutMorph >> someSubmorphPositionOrExtentChanged [
	"Some submorph changed position or extent.
	Must layout submorphs again."

	self layoutNeeded: true.
]
