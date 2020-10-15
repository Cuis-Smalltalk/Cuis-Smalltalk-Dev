'From Cuis 5.0 [latest update: #4407] on 14 October 2020 at 9:50:53 pm'!

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 3/6/2020 16:06:23'!
example1
"
	self example1
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (WidgetMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (WidgetMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (WidgetMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 200 fixedHeight: 200).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (WidgetMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (WidgetMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (WidgetMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (WidgetMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/12/2015 16:42'!
example10
"
	self example10 openInWorld
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example10.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (WidgetMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addAdjusterMorph; 
	addMorph: (WidgetMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (WidgetMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (WidgetMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (WidgetMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (WidgetMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (WidgetMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
^ pane! !

!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/5/2015 10:55'!
example11
"
	self example11
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example11.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (WidgetMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addAdjusterMorph; 
	addMorph: (WidgetMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (WidgetMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (WidgetMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (WidgetMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (WidgetMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (WidgetMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'KenD 5/22/2020 13:57:31'!
example13
	"
	self example13
	"
	| pane row innerRow |
pane _ (LayoutMorph newColumn separation: 5)	name: #example13.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	innerRow _ (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (WidgetMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (WidgetMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (WidgetMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (WidgetMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (WidgetMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 2/16/2016 13:27'!
example1b
"
Based on #example1, but using some Morph instead of RectangleLikeMorph -> extent is not adjustable
	self example1b
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (WidgetMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (Morph new name: #B);
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (WidgetMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (WidgetMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (Morph new name: #G);
	addMorph: (WidgetMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (Morph new name: #J);
	addMorph: (WidgetMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (WidgetMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'KenD 5/22/2020 13:55:23'!
example2
	"
	self example2
	"
	| pane row |
	pane _ (LayoutMorph newColumn separation: 5)	name: #example2.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (WidgetMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #A)
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 offAxisEdgeWeight: #rightOrBottom);
		addMorph: (WidgetMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #B)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: (WidgetMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #C)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 offAxisEdgeWeight: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 200@180 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 11/18/2015 09:45'!
example20
"
	self example20
"
| column |

column _ (LayoutMorph newColumn separation: 5) name: #Column.
column
	color: Color red;
	addMorph: (WidgetMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 20);
	addMorph: (WidgetMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec fixedWidth: 40 proportionalHeight: 0.5);
	addMorph: (WidgetMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 30).
column morphPosition: 150@130 extent: 400@300.
column openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'KenD 5/22/2020 13:58:02'!
example3
	"
	self example3
	"
	| pane row innerRow |
pane _ (LayoutMorph newColumn separation: 5)	name: #example3.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	innerRow _ (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (WidgetMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (WidgetMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (WidgetMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: (WidgetMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: (WidgetMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 10/12/2020 21:02:19'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane _ LayoutMorph newRow separation: 5. "1"
pane addMorph: (LabelMorph contents: '1').

rect1 := WidgetMorph new color: (Color lightOrange); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect1.
rect2 := WidgetMorph new color: (Color cyan); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect2.
pane
	color: Color lightGreen;
	morphPosition: 120 @ 50 extent: 180 @ 100;
	openInWorld.

pane _ LayoutMorph newRow separation: 5. "2"
pane addMorph: (LabelMorph contents: '2').

rect1 := WidgetMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect1.
rect2 := WidgetMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane
	color: Color lightGreen;
	morphPosition: 320 @ 50 extent: 180 @ 100;
	openInWorld.


pane _ LayoutMorph newRow separation: 5. "3"
pane addMorph: (LabelMorph contents: '3').

rect1 := WidgetMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
rect2 := WidgetMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect2.
pane
	color: Color lightGreen;
	morphPosition: 520 @ 50 extent: 180 @ 100;
	openInWorld! !


!ResizeMorph methodsFor: 'events' stamp: 'jmv 3/10/2018 21:31:55'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	from _ self toGridPoint: localEventPosition.
	outlineMorph _ WidgetMorph new
		borderColor: `Color black`;
		color: `Color transparent`;
		openInWorld;
		hide.
	self selectTo: localEventPosition! !

