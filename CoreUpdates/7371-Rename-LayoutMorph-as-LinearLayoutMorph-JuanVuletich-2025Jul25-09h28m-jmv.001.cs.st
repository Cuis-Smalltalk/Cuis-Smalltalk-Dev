'From Cuis7.5 [latest update: #7370] on 25 July 2025 at 9:28:35 am'!
Smalltalk renameClassNamed: #LayoutMorph as: #LinearLayoutMorph!

!Dialog methodsFor: 'layout' stamp: 'jmv 7/25/2025 09:28:21'!
beColumn
	layoutMorph
		ifNotNil: [ layoutMorph beColumn ]
		ifNil: [
			layoutMorph := LinearLayoutMorph newColumn.
			self addMorphFront: layoutMorph ]! !


!FindInSourceDialog methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:28:20'!
buildWidgets

	| buttonRow titleMorph |
	titleMorph := (LabelMorph contents: 'Find in source code' fontSize: 16 emphasis: 1).
	titleMorph layoutSpec offAxisEdgeWeight: 0.
	searchField := EntryFieldMorph new.
	searchField
		crAction: [ self ok ];
		escAction: [ self cancel ].
	searchField layoutSpec offAxisEdgeWeight: 0.
	initialSearchString ifNotNil: [ searchField contents: initialSearchString ].
	scopeList := PluggableListMorph
		withModel: self
		listGetter: #scopesList
		indexGetter: #scopeIndex
		indexSetter: #scopeIndex:.
	scopeList 
		doubleClickSelector: #ok.
	buttonRow := LinearLayoutMorph newRow.
	buttonRow
		addMorph: (PluggableButtonMorph model: self action: #ok label: 'Ok');
		addMorph: (PluggableButtonMorph model: self action: #cancel label: 'Cancel').
	layoutMorph
		gap: 14;
		addMorphKeepMorphHeight: titleMorph;
		addMorphKeepMorphHeight: searchField;
		addMorphUseAll: scopeList;
		addMorphKeepMorphHeight: buttonRow.

	self morphExtent: self minimumExtent * (2@1.5).! !


!SampleModalDialog methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:28:21'!
buildWidgets
	"
	This is a sample implementation. Actual subclasses should reimplement #buildWidgets (without calling super!!).
	self openModal
	"

	| typeRow buttonRow |
	typeRow := LinearLayoutMorph newRow.
	typeRow
		addMorph: (LabelMorph contents: 'Type');
		addMorph: (LabelMorph contents: 'So what');
		addMorph: (LabelMorph contents: 'And then').
	buttonRow := LinearLayoutMorph newRow.
	buttonRow
		padding: 10;
		gap: 20;
		addMorph: (PluggableButtonMorph model: self action: #ok label: 'Ok');
		addMorph: (PluggableButtonMorph model: self action: #cancel label: 'Cancel').
	layoutMorph
		addMorph: 
			((LabelMorph contents: 'This is a title for this Dialog' fontSize: 25)
				borderWidth: 2; borderColor: color lighter;
				padding: 20);
		addMorph: Sample01Star new;
		addMorph: typeRow;
		addMorph: buttonRow.

	"self ensureMinimumExtent."
	"To make the star somehow larger"
	self morphExtent: self minimumExtent + (0@200).! !


!LinearLayoutMorph methodsFor: 'testing' stamp: 'jmv 7/25/2025 09:28:22'!
is: aSymbol
	^ aSymbol == #LinearLayoutMorph or: [ super is: aSymbol ]! !


!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:20'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:20'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:21'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:21'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:20'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:22'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:21'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:22'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:21'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:22'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:20'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:21'!
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

!LinearLayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 09:28:21'!
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

!LinearLayoutMorph class methodsFor: 'FormLayout examples' stamp: 'jmv 7/25/2025 09:28:20'!
example1Form
"
	self example1Form
"
	| pane |
	pane := (LinearLayoutMorph newForm separation: 5) name: #example1.
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


!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:28:22'!
addTextPane
	"Add a text pane and two buttons."
	
	| controlsContainer |
	textMorph := (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			textMorph emptyTextDisplayMessage: msg ].
	textMorph
		hasUnacceptedEdits: true;
		acceptOnCR: true;
		escAction: [ self cancel ].
	(controlsContainer := LinearLayoutMorph newRow)
		addMorphUseAll: textMorph;
		addMorph: 
			((PluggableButtonMorph model: textMorph scroller action: #acceptContents)
				setBalloonText: 'OK' bold, ' (accept input)';
				icon: Theme current acceptIcon);
		addMorph:
			((PluggableButtonMorph model: self action: #cancel)
				setBalloonText: 'Cancel' bold, '  (discard input)';
				icon: Theme current cancelIcon).
	self addMorphUseAll: controlsContainer! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:28:21'!
initialize
	super initialize.
	viewBox := LinearLayoutMorph newRow color: self defaultColor.
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


!MenuMorph methodsFor: 'construction' stamp: 'jmv 7/25/2025 09:28:21'!
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
		(LinearLayoutMorph newRow
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

!MenuMorph methodsFor: 'menu' stamp: 'jmv 7/25/2025 09:28:21'!
removeStayUpBox
	| box |
	submorphs isEmpty ifTrue: [^self].
	(submorphs first is: #LinearLayoutMorph) ifFalse: [^self].
	box := submorphs first submorphs second.
	(box is: #PluggableButtonMorph) 
		ifTrue: [ box hide ]! !


!PluggableButtonMorph class methodsFor: 'example' stamp: 'jmv 7/25/2025 09:28:20'!
example
	"
	PluggableButtonMorph example openInWorld
	"

	| s1 s2 s3 b1 b2 b3 row |
	s1 := Switch new.
	s2 := Switch new turnOn.
	s3 := Switch new.
	s1 onAction: [s3 turnOn].
	s2 onAction: [s3 turnOff].
	s3 onAction: [s2 turnOff].
	b1 := (PluggableButtonMorph model: s1 stateGetter: #isOn action: #switch) label: 'S1'.
	b2 := (PluggableButtonMorph model: s2 stateGetter: #isOn action: #turnOn) label: 'S2'.
	b3 := (PluggableButtonMorph model: s3 stateGetter: nil action: #switch) 
		enableSelector: #isOn;
		label: 'S3'.
	b1 color: `Color lightRed`.
	b2 color: `Color lightRed`.
	b3 color: `Color lightRed`.
	row := LinearLayoutMorph newRow
		addMorphs: (Array with: b1 with: b2 with: b3);
		morphExtent: `120@35`.
	^ row! !


!SystemWindow methodsFor: 'layout' stamp: 'jmv 7/25/2025 09:28:21'!
beColumn
	layoutMorph
		ifNotNil: [ layoutMorph beColumn ]
		ifNil: [
			layoutMorph := LinearLayoutMorph newColumn.
			self addMorphFront: layoutMorph ]! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 7/25/2025 09:28:21'!
beRow
	layoutMorph
		ifNotNil: [ layoutMorph beRow ]
		ifNil: [
			layoutMorph := LinearLayoutMorph newRow.
			self addMorphFront: layoutMorph ]! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buildButtonPane

	| saveButton createButton deleteButton browseChangesButton browseButton addReqButton buttonRow |
	saveButton := PluggableButtonMorph 
							model: model 
							action: #save 
							label: 'save'.
	createButton := PluggableButtonMorph 
							model: self 
							action: #createPackage 
							label: 'new'.
	deleteButton := PluggableButtonMorph 
							model: self 
							action: #deletePackage 
							label: 'delete/merge'.
	browseChangesButton := PluggableButtonMorph 
							model: self 
							action: #browseChanges 
							label: 'changes'.
	browseButton := PluggableButtonMorph 
							model: self 
							action: #browse 
							label: 'browse'.
	addReqButton := PluggableButtonMorph 
							model: self 
							action: #addRequirement 
							label: 'add requirement'.
	buttonRow := LinearLayoutMorph newRow.
	buttonRow
		doAdoptWidgetsColor;
		color: self widgetsColor quiteWhiter;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: createButton proportionalWidth: 0.6;
		addMorph: deleteButton proportionalWidth: 0.6;
		addMorph: browseChangesButton proportionalWidth: 0.6;
		addMorph: browseButton proportionalWidth: 0.6;
		addMorph: addReqButton proportionalWidth: 0.6.
	buttonRow submorphsDo: [ :button | button  color: self widgetsColor ].
	^ buttonRow ! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:21'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| names fileNames namesPane fileNamesPane 
	upperRow  description summary labelBackground |
	labelBackground := Theme current background.
	
	names := PluggableListMorph
		withModel: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	namesPane := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Package Name');
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		withModel: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames alwaysHideVerticalScrollbar.
	fileNamesPane := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' File Name');
		addMorphUseAll: fileNames.

	upperRow := LinearLayoutMorph newRow.
	upperRow
		addMorph: namesPane proportionalWidth: 0.4;
		addAdjusterAndMorph: fileNamesPane proportionalWidth: 0.6.
		
	description := (TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:) emptyTextDisplayMessage: 'Please enter a description for this package'.

	summary := (TextModelMorph
		textProvider: model
		textGetter: #summary) emptyTextDisplayMessage: 'Package summary (No package selected?)'.
	
	names scrollSiblings: {fileNames}.
	fileNames scrollSiblings: {names}.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: self buildButtonPane fixedHeight: Theme current buttonPaneHeight;
		addAdjusterAndMorph: summary fixedHeight: 60;
		addAdjusterAndMorph: description proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.15.
	self setLabel: 'Installed Packages'! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:21'!
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
							
	buttonLayout := LinearLayoutMorph newRow.
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
	
	reqLayout := LinearLayoutMorph newRow.
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


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:21'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons := LinearLayoutMorph newColumn.
	(Preferences at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations := LinearLayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
optionalButtonRow
	"Answer a row of control buttons"

	| row buttons widths buttonColor |
	buttons := OrderedCollection new.
	widths := OrderedCollection new.
	buttonColor := self buttonColor.
	self optionalButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button := PluggableButtonMorph
			model: self
			action: tuple third.
		button color: buttonColor.
		button label: tuple second.
		tuple size > 3 ifTrue: [ button setBalloonText: tuple fourth ].
		tuple size > 4 ifTrue: [ button secondaryActionSelector: tuple fifth ].
		buttons add: button ].
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row
		addMorphs: buttons
		widthProportionalTo: widths.
	^ row.! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations code comment separator |
	code := self buildMorphicCodePane.
	comment := self buildMorphicCommentPane.
	separator := LayoutAdjustingMorph new.
	comment separator: separator code: code.
	codeAndButtons := LinearLayoutMorph newColumn.
	(Preferences at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: code proportionalHeight: 0.5;
		addMorph: separator fixedHeight: Theme current layoutAdjusterThickness;
		addMorph: comment proportionalHeight: 0.5.
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations := LinearLayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:21'!
buildMorphicClassColumnWith: classList
	| column |
	column := LinearLayoutMorph newColumn.
	column
		addMorphUseAll: classList;
		addAdjusterAndMorph: self buildMorphicSwitches fixedHeight: Theme current buttonPaneHeight.
	^column! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:21'!
buildMorphicSwitches

	| instanceSwitch commentSwitch classSwitch row buttonColor |
	
	instanceSwitch := self createInstanceButton.
	commentSwitch := self createCommentButton.
	classSwitch := self createClassButton.
	
	row := LinearLayoutMorph newRow.
	row
		doAdoptWidgetsColor;
		addMorph: instanceSwitch proportionalWidth: 0.45;
		addMorph: commentSwitch proportionalWidth: 0.22;
		addMorph: classSwitch proportionalWidth: 0.33.
	buttonColor := self buttonColor.
	row color: buttonColor.

	{ 
		instanceSwitch.
		commentSwitch.
		classSwitch} do: [:m |  m color: buttonColor ].
		
	^row! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes messageCatList messageList systemCatList classList |

	systemCatList := self buildMorphicSystemCategoryList.
	classList := self buildMorphicClassList.
	messageCatList := self buildMorphicMessageCatList.
	messageList := self buildMorphicMessageList.
	
	classList allowItemDrag: true.
	systemCatList 
		acceptDropsFrom: classList 
		performing: #categorizeUnderCategory:class:.
		
	messageList allowItemDrag: true.
	messageCatList
		acceptDropsFrom: messageList
		performing: #categorizeUnderCategoryAt:selector:.
	
	upperPanes := LinearLayoutMorph newRow.
	upperPanes
		addMorph: systemCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: (self buildMorphicClassColumnWith: classList) proportionalWidth: 0.2;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:21'!
buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes messageCatList messageList classColumn classList |
	mySingletonList := (PluggableListMorph
				withModel: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:)
					menuNshortcutsKey: #systemCatSingletonMenuSpec.
	mySingletonList hideScrollBarsIndefinitely.

	classList := self buildMorphicClassList.
	classColumn := self buildMorphicClassColumnWith: classList.
	messageCatList := self buildMorphicMessageCatList.
	messageList := self buildMorphicMessageList.
	
	upperPanes := LinearLayoutMorph newRow.
	upperPanes
		addMorph: classColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	messageList allowItemDrag: true.
	messageCatList
		acceptDropsFrom: messageList
		performing: #categorizeUnderCategoryAt:selector:.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: (Preferences at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| sysCatList msgCatList upperPanes clsLayout clsList msgList |
	model systemCategoryList ifNotEmpty: [ 
		model setSelectedSystemCategory: model systemCategoryList first ].
	sysCatList := (PluggableListMorph
			withModel: model 
			listGetter: #systemCategorySingleton
			indexGetter: #indexIsOne 
			indexSetter: #indexIsOne:)
				menuNshortcutsKey: #codeFileListMenuSpec.
	sysCatList hideScrollBarsIndefinitely.
	
	msgCatList := (PluggableListMorph
			withModel: model 
			listGetter: #messageCategoryList
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:)
				menuNshortcutsKey: #messageCatListMenuSpec.

	clsList := self buildMorphicClassList.
	clsLayout := self buildMorphicClassColumnWith: clsList.
	msgList := self buildMorphicMessageList.

	upperPanes := LinearLayoutMorph newRow.
	upperPanes
		addMorph: clsLayout proportionalWidth: 0.3;
		addAdjusterAndMorph: msgCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: msgList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: sysCatList fixedHeight: (Preferences at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
optionalButtonRow
	"Answer a row of buttons to occur in a tool pane"

	| row buttons widths buttonColor |
	buttons := OrderedCollection new.
	widths := OrderedCollection new.
	buttonColor := self buttonColor.
	self optionalModelButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button := PluggableButtonMorph 
					model: model
					action: tuple third.
		button color: buttonColor.
		button label: tuple second.
		buttons add: button.
		button setBalloonText: tuple fourth].
	buttons add: self lineDiffButton.
	widths add: 14.
	buttons add: self wordDiffButton.
	widths add: 16.
	model wantsPrettyDiffOption ifTrue: [
		buttons add:  self prettyLineDiffButton.
		widths add: 21.
		buttons add:  self prettyWordDiffButton.
		widths add: 23 ].
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:21'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| changeSetList classList changeSetListPane classListPane
	  messageList upperPanes labelBackground |
	labelBackground := Theme current background.
	model myChangeSet ifNil: [
		"Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	changeSetList := (PluggableListMorphByItem
				withModel: model
				listGetter: #changeSetList
				indexGetter: #currentChangeSetName
				indexSetter: #showChangeSetNamed:)
					menuNshortcutsKey: #changeSetListMenuSpec;
					autoDeselect: false.
	changeSetListPane := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Change Set name');
		addMorphUseAll: changeSetList.

	classList := (PluggableListMorphByItem
				withModel: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:)
					menuNshortcutsKey: #classListMenuSpec.
	classListPane := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes := LinearLayoutMorph newRow.
	upperPanes
		addMorph: changeSetListPane proportionalWidth: 0.6;
		addAdjusterAndMorph: classListPane proportionalWidth: 0.4.
	
	messageList := (PluggableListMorphByItem
				withModel: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:)
					menuNshortcutsKey: #messageListMenuSpec.
	messageList := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Methods');
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:21'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations twoRowsOfButtons h |
	twoRowsOfButtons := LinearLayoutMorph newColumn.
	h := self defaultButtonPaneHeight.
	(Preferences at: #optionalButtons) ifTrue: [
		h := self defaultButtonPaneHeight * 2.
		twoRowsOfButtons
			addMorph: self optionalButtonRow proportionalHeight: 1.0;
			addAdjusterMorph ].
	twoRowsOfButtons
		addMorph: self customButtonRow proportionalHeight: 1.0.
	codeAndButtons := LinearLayoutMorph newColumn.
	codeAndButtons
		addMorph: twoRowsOfButtons fixedHeight: h;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations := LinearLayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:21'!
buildMorphicWindow
	"Open a full morphic debugger with the given label"

	| bottomMorph |
	bottomMorph := LinearLayoutMorph newRow.
	bottomMorph
		addMorph: self receiverInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: self receiverInspectorText proportionalWidth: 0.3;
		addAdjusterAndMorph: self contextVariableInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: self contextVariableInspectorText proportionalWidth: 0.3.

	self layoutMorph
		addMorph: self stackList proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55;
		addAdjusterAndMorph: bottomMorph proportionalHeight: 0.2.! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
customButtonRow
	"Answer a button pane affording the user one-touch access to certain functions; the pane is given the formal name 'customButtonPane' by which it can be retrieved by code wishing to send messages to widgets residing on the pane"

	| button buttons row buttonColor |
	
	buttons := OrderedCollection new.
	buttonColor := self buttonColor.
	"button with target = self"
	button := PluggableButtonMorph 
		model: self
		action: #proceed.
	button color: buttonColor.
	button label: 'Proceed'.
	button setBalloonText: 'close the debugger and proceed.'.
	buttons add: button.
	"buttons with model target"
	self customButtonSpecs do: [ :tuple |
		button := PluggableButtonMorph 
					model: self
					action: tuple second.
		button color: buttonColor.
		button label: tuple first.
		tuple size > 2 ifTrue: [button setBalloonText: tuple third].
		buttons add: button].

	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons.
	^row! !


!MethodSetWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:19'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons := LinearLayoutMorph newColumn.
	(Preferences at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations := LinearLayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"
	"MessageNames open"
	| selectorListView firstRow searchButton secondRow |
	textMorph := (TextModelMorph
		textProvider: model
		textGetter: #searchString
		textSetter: #searchString:
		selectionGetter: #contentsSelection)
		setBalloonText: 'See MessageNames class comment for search string options';
		emptyTextDisplayMessage: 'Type here, then hit Search'.
	textMorph askBeforeDiscardingEdits: false.
	textMorph acceptOnCR: true.
	textMorph hideScrollBarsIndefinitely.
	searchButton := PluggableButtonMorph new
		model: textMorph innerTextMorph;
		label: 'Search';
		action: #acceptContents.
	searchButton setBalloonText: 'Type some letters into the pane at right, and then press this Search button (or hit RETURN) and all method selectors that match what you typed will appear in the list pane below.  Click on any one of them, and all the implementors of that selector will be shown in the right-hand pane, and you can view and edit their code without leaving this tool.'.
	firstRow := LinearLayoutMorph newRow.
	firstRow color: self windowColor.
	firstRow
		doAdoptWidgetsColor;
		
			addMorph: searchButton
			proportionalWidth: 0.25;
		
			addMorph: textMorph
			proportionalWidth: 0.75.
	selectorListView := (PluggableListMorph
		withModel: model
		listGetter: #selectorList
		indexGetter: #selectorListIndex
		indexSetter: #selectorListIndex:)
			menuNshortcutsKey: #selectorListMenuSpec.
	secondRow := LinearLayoutMorph newRow.
	secondRow
		
			addMorph: selectorListView
			proportionalWidth: 0.5;
		
			addAdjusterAndMorph: self buildMorphicMessageList
			proportionalWidth: 0.5.
	self layoutMorph
		
			addMorph: firstRow
			fixedHeight: self defaultButtonPaneHeight + 4;
		
			addAdjusterAndMorph: secondRow
			proportionalHeight: 0.5;
		
			addAdjusterAndMorph: self buildLowerPanes
			proportionalHeight: 0.5.
	model changed: #editSelection! !


!ProtocolBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buildMorphicWindow
	"Answer a morphic window that can display the receiver with a class hierarchy"
	| topRow |

	topRow := LinearLayoutMorph newRow.
	topRow 
		addMorph: self buildSelectiveClassList proportionalWidth: 0.25;
		addAdjusterMorph;
		addMorph: self buildCategoriesList proportionalWidth: 0.25;
		addAdjusterMorph;
		addMorph: self buildMorphicMessageList proportionalWidth: 0.5.
	self layoutMorph
		addMorph: topRow proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.8.
	model changed: #editSelection! !


!ChangeSelectorWizardStepWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:22'!
buildLowerPanes

	| codeAndButtons  |
	
	codeAndButtons := LinearLayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	
	^codeAndButtons ! !

!ChangeSelectorWizardStepWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:22'!
buttonsRow

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^row
	
	! !


!ExtractMethodReplacementsWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:22'!
buildLowerPanes

	| codeAndButtons  |
	
	codeAndButtons := LinearLayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	
	^codeAndButtons ! !

!ExtractMethodReplacementsWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:21'!
buttonsRow

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^row
	
	! !


!InlineMethodWizardStepWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buildLowerPanes

	| codeAndButtons  |

	codeAndButtons := LinearLayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.

	^codeAndButtons ! !

!InlineMethodWizardStepWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buttonsRow

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.

	^row

	! !


!TestResultWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:21'!
buildLowerPanes

	| codeAndButtons  |

	codeAndButtons := LinearLayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	
	^codeAndButtons ! !

!TestResultWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buttonsRow

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^row
	
	! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buttonRowForPreDebugWindow: aDebugger
	| row aButton triads buttons |
	buttons := OrderedCollection new.
	triads := OrderedCollection withAll: self preDebugButtonSpec.
	aDebugger shouldBeAbleToCreateMethod ifTrue: [ triads add: { 'Create'. #createMethod. 'create the missing method' }].
	
	triads do: [ :triad |
		aButton := PluggableButtonMorph new model: self.
		aButton label: triad first.
		aButton action: triad second.
		aButton setBalloonText: triad third.
		buttons add: aButton].
	
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: `Color transparent`.
	row separation: 1.
	row addMorphs: buttons.
	^row! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buildMorphicWindow
	| middleRow upperRow |
	upperRow := LinearLayoutMorph newRow.
	upperRow
		addMorph: self morphicPatternPane proportionalWidth: 0.3;
		addAdjusterAndMorph: self optionalButtonRow proportionalWidth: 0.7.
	middleRow := LinearLayoutMorph newRow.
	middleRow
		addMorph: self morphicDirectoryTreePane proportionalWidth: 0.3;
		addAdjusterAndMorph: self morphicFileListPane proportionalWidth: 0.7.
	self layoutMorph
		addMorph: upperRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterAndMorph: middleRow proportionalHeight: 0.4;
		addAdjusterAndMorph: self morphicFileContentsPane proportionalHeight: 0.6.
	self setLabel: model directory pathName.
	model postOpen! !

!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
optionalButtonRow
	"Answer the button row associated with a file list"

	| row buttonColor |
	row := LinearLayoutMorph newRow.
	buttonColor := self buttonColor.
	row setProperty: #buttonRow toValue: true.  "Used for dynamic retrieval later on"
	row doAdoptWidgetsColor.
	row color: buttonColor.
	self updateButtonRow: row.
	^row! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:22'!
buildMorphicWindow
	" Inspector openOn: SystemOrganization "
	| contentsText list upperRow evaluatorText |

	"Build widgets. We'll assemble them below."
	list := self buildList.
	contentsText := self buildContentsText.
	evaluatorText := self buildEvaluatorText.
	
	"Upper row has two widgets, side by side: the list of variables at the left side, and the variable contents pane at the right side."
	upperRow := LinearLayoutMorph newRow.
	upperRow
		addMorph: list proportionalWidth: 0.3;
		addAdjusterAndMorph: contentsText proportionalWidth: 0.7.
	"Inspector Window has two rows: At the top, the one we just bult. Below it, the evaluation pane."
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.7;
		addAdjusterAndMorph: evaluatorText proportionalHeight: 0.3.
	"Set label"
	self setLabel: model displayLabel! !


!ProcessBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:22'!
buildMorphicWindow
	"Create a pluggable version of me, answer a window"
	| aTextMorph list1 list2 upperRow |
	list1 := (PluggableListMorph
				withModel: model
				listGetter: #processNameList
				indexGetter: #processListIndex
				indexSetter: #processListIndex:)
					menuNshortcutsKey: #processListMenuSpec.
	list2 := (PluggableListMorph
				withModel: model
				listGetter: #stackNameList
				indexGetter: #stackListIndex
				indexSetter: #stackListIndex:)
					menuNshortcutsKey: #stackMenuSpec.
	upperRow := LinearLayoutMorph newRow.
	upperRow
		addMorph: list1 proportionalWidth: 0.5;
		addAdjusterAndMorph: list2 proportionalWidth: 0.5.
	aTextMorph := (TextModelMorph
				textProvider: model
				textGetter: #selectedMethod) emptyTextDisplayMessage: 'Method source (not selected?)'.
	aTextMorph askBeforeDiscardingEdits: false.
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: aTextMorph proportionalHeight: 0.5.
	self setLabel: 'Process Browser'! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buildLowerPanes
	| failuresList errorsList column |
	progress := ProgressMorph label: '' subLabel: ''.
	self buildPassFailText.
	self buildDetailsText.
	failuresList := self buildFailuresList.
	errorsList := self buildErrorsList.
	column := LinearLayoutMorph newColumn.
	column
		addMorph: progress fixedHeight: 50;
		addMorph: passFailText fixedHeight: 26;
		addMorph: detailsText fixedHeight: 52;
		addMorph: failuresList proportionalHeight: 0.6;
		addAdjusterAndMorph: errorsList proportionalHeight: 0.6.
	^ column! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 7/25/2025 09:28:20'!
buildUpperControls
	| refreshButton filterButton stopButton runOneButton runButton runProfiledButton buttonsRow theTestsList column |

	runOneButton := self buildRunOneButton.
	runButton := self buildRunButton.
	runProfiledButton := self buildRunProfiledButton.	

	refreshButton := self buildRefreshButton.
	filterButton := self buildFilterButton.
	stopButton := self buildStopButton.

	theTestsList := (PluggableListMorphOfMany
				withModel: model
				listGetter: #tests
				primarySelectionGetter: #selectedSuite
				primarySelectionSetter: #selectedSuite:
				listSelectionGetter: #listSelectionAt:
				listSelectionSetter: #listSelectionAt:put:)
					menuNshortcutsKey: #listMenuSpec.
	theTestsList autoDeselect: false.
	theTestsList color: self textBackgroundColor.

	buttonsRow := LinearLayoutMorph newRow.
	buttonsRow doAdoptWidgetsColor.
	buttonsRow color: self buttonColor.
	buttonsRow
		addMorphs: {runButton. runOneButton. runProfiledButton};
		addMorphs: {refreshButton. filterButton. stopButton}.

	column := LinearLayoutMorph newColumn.
	column
		addMorph: theTestsList proportionalHeight: 1;
		addMorph: buttonsRow fixedHeight: self defaultButtonPaneHeight.

	^ column! !


!LayoutSpec methodsFor: 'private' stamp: 'jmv 7/25/2025 09:28:21'!
updateContainingLayout
	| owningLayout |
	morph ifNotNil: [
		owningLayout := morph owner.
		(owningLayout is: #LinearLayoutMorph) ifTrue:
			[ owningLayout layoutSubmorphs ]].! !


!Dialog reorganize!
('initialization' buildWidgets)
('defaults' defaultColor defaultExtent defaultPadding)
('layout' beColumn)
('geometry' extentChanged: minimumExtent)
!


!FindInSourceDialog reorganize!
('initialization' buildWidgets initialize intoWorld: smalltalkEditor:)
('defaults' defaultBorderWidth defaultPadding)
('events' cancel keyStroke: ok)
('event handling testing' handlesKeyboard)
('scopes list' scopeIndex scopeIndex: scopesList)
!


!SampleModalDialog reorganize!
('initialization' buildWidgets)
!


!LinearLayoutMorph reorganize!
('accessing' addMorphFrontFromWorldPosition: adoptWidgetsColor: axisEdgeWeight axisEdgeWeight: borderWidth: gap gap: padding: separation:)
('adjust' adjustBy:at:)
('convenience methods' addAdjusterAndMorph:fixedHeight: addAdjusterAndMorph:layoutSpec: addAdjusterAndMorph:proportionalHeight: addAdjusterAndMorph:proportionalWidth: addMorph:fixedHeight: addMorph:fixedWidth: addMorph:proportionalHeight: addMorph:proportionalWidth: addMorphKeepMorphHeight: addMorphUseAll: addMorphs: addMorphs:widthProportionalTo:)
('geometry' extentChanged: minimumExtent someSubmorphPositionOrExtentChanged)
('initialization' beColumn beRow defaultBorderWidth defaultColor defaultGap doAdoptWidgetsColor doClipSubmorphs initializeColumn initializeForm initializeRow)
('layout' layoutBounds layoutSubmorphs layoutSubmorphsIfNeeded)
('submorphs-add/remove' addAdjusterMorph addMorph:layoutSpec: addMorphBack: addMorphFront: removedMorph:)
('testing' is: isColumn isRow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex:position:)
('layout in owner' layoutSpec)
('geometry testing' clipsSubmorphs)
('drawing' drawOn:)
!


!LinearLayoutMorph class reorganize!
('instance creation' initializedInstance new newColumn newForm newRow)
('LinearLayout examples' example1 example10 example101VG example11 example13 example1b example2 example20 example3 example6 example7 exampleEdgesColumn exampleEdgesRow iconsExample launcherExample)
('FormLayout examples' example1Form)
('new-morph participation' categoryInNewMorphMenu)
!


!StringRequestMorph reorganize!
('accessing' defaultLayerNumber emptyTextDisplayMessage: response response:)
('drawing' drawOn:)
('initialization' acceptBlock: addTextPane addTitle: cancelBlock: defaultBorderWidth defaultColor initialize setQuery:initialAnswer: validationBlock:)
('private' cancel getUserResponseOrCancel: selectionInterval textBox)
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


!PluggableButtonMorph class reorganize!
('new-morph participation' categoryInNewMorphMenu includeInNewMorphMenu initializedInstance)
('example' example)
('instance creation' model:action: model:action:actionArgument: model:action:actionArgument:label: model:action:label: model:stateGetter:action: model:stateGetter:action:label: model:stateGetter:action:label:balloonText: model:stateGetter:stateArgument:action:actionArgument:label:)
!


!SystemWindow reorganize!
('GUI building' buttonColor defaultButtonPaneHeight textBackgroundColor windowColor)
('change reporting' invalidateTitleArea)
('drawing' drawClassicFrameOn:color: drawLabelOn: drawOn: drawRoundedFrameOn:color: isHighlighted makeMeFullyVisible makeMeVisible visible:)
('events' wantsToBeDroppedInto:)
('geometry' extentChanged: fontPreferenceChanged interiorExtent interiorOrigin justDroppedInto:event: labelXPosition minimumExtent rescaleButtons)
('geometry services' fullyCoveredRectangle)
('initialization' createCloseButton createCollapseButton createExpandButton createMenuButton defaultBorderWidth defaultColor initialize initializeAdjusters initializeLabelArea intoWorld: openInWorld windowEdgeAdjustingMorphClass)
('label' label labelHeight relabel setLabel: update:)
('layout' beColumn beRow layoutMorph)
('menu' addCustomMenuItems:hand: addTileResizerMenuTo: addWindowControlTo: buildWindowMenu changeColor makeSecondTopmost offerWindowMenu sendToBack setWindowColor setWindowColor:)
('open/close' closeBoxHit closeOtherWindows delete initialExtent initialFrameIn: openInWorld:)
('panes' widgetsColor widgetsColor: windowFrameColor)
('printing' printOn:)
('moving' moveBottom moveBottomLeft moveBottomRight moveLeft moveRight moveTop moveTopLeft moveTopRight)
('resize/collapse' allowedArea expandBoxHit resize resizeBottom resizeBottomLeft resizeBottomRight resizeFull resizeLeft resizeRight resizeTop resizeTopLeft resizeTopRight)
('resizing' windowBottom: windowBottomLeft: windowBottomRight: windowLeft: windowRight: windowTop: windowTopLeft: windowTopRight:)
('stepping' stepAt: stepTime wantsSteps wantsStepsWhenCollapsed)
('structure' owningWindow)
('submorphs-add/remove' addMorph: addMorph:layoutSpec:)
('submorphs-menus' allRegisteredMenuAndShortcutsSpecProviders:do:widget: menuAndShortcutRegistryRoot: menuNshortcutsSpecsFor:do:)
('testing' is:)
('top window' activate activateAndForceLabelToShow activateAndSendTopToBack: isTopWindow submorphToFocusKeyboard)
('user interface' canDiscardEditsOf: forceSaveContents forceSaveContentsTo: hasSaveAs isItOkToLooseChanges notifyUserWith: okToChange okToChangeDueTo: saveContents saveContentsTo:)
!


!CodePackageListWindow reorganize!
('GUI building' buildButtonPane buildMorphicWindow buildRequirementsPane initialExtent windowColor)
('commands' addRequirement browse browseChanges createPackage deletePackage)
('geometry' fontPreferenceChanged)
!


!CodeWindow reorganize!
('GUI building' addShiftClickEventHandlerFor: buildLowerPanes buildMorphicAnnotationsPane buildMorphicCodePane createCodePaneMorph defaultAnnotationPaneHeight lineDiffButton lineDiffButtonHelp optionalButtonRow optionalButtonTuples prettyLineDiffButton prettyLineDiffButtonHelp prettyWordDiffButton prettyWordDiffButtonHelp wordDiffButton wordDiffButtonHelp)
('accessing' buttonWithSelector: inheritanceButton)
('updating' classAdded: classRenamed:from:to:inCategory: decorateButtons decorateForInheritance updateListsAndCode updateListsAndCodeNow)
('top window' activateAndSendTopToBack:)
('menu commands' browseClassRefs browseClassVarRefs browseFullProtocol browseHierarchy browseHierarchyAndReplaceMe browseImplementors browseImplementorsOfMessages browseInstVarRefs browseLocalImplementorsOfMessages browseLocalSenders browseLocalSendersOfMessages browseMethodFull browseMethodFullAndReplaceMe browseSenders browseSendersOfMessages browseSubImplementors browseSuperImplementors browseUnusedMethods browseVersions findMethodInChangeSets inspectInstances inspectSubInstances makeSampleInstance methodInheritance methodInheritanceAndReplaceMe openSingleMessageBrowser shiftClickPressedOn:at: showUnreferencedClassVars showUnreferencedInstVars)
('menu building')
('misc' getSelectorAndSendQuery:to: getSelectorAndSendQuery:to:with: selectMessageAndEvaluate: sendQuery:to: sendQuery:to:with: useSelector:orGetSelectorAndSendQuery:to:)
('initialization' model:)
!


!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCategoryList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCodePaneMorph createCommentButton createInstanceButton windowColor)
('menu commands' browseAllClasses browsePackage findClass openSystemCategoryBrowser)
('updating' classAdded: classRenamed:from:to:inCategory: disableCodePaneEditing editSelectionChanged enableCodePaneEditing update:)
('menu & kbd shortcuts' classListMenuSpec messageCatListMenuSpec messageListMenuSpec systemCatListMenuSpec systemCatSingletonMenuSpec)
!


!CodeFileBrowserWindow reorganize!
('GUI building' buildMorphicWindow optionalButtonTuples windowColor)
('menu commands' browseMethodFull browseVersions findClass methodInheritance removeUnmodifiedClasses)
('menu & kbd shortcuts' classListMenuSpec codeFileListMenuSpec menuAndShortcutRegistryRoot: messageCatListMenuSpec messageListMenuSpec)
!


!ChangeListWindow reorganize!
('GUI building' buildMorphicWindow createCodePaneMorph optionalButtonRow optionalModelButtonTuples windowColor)
('menu commands' browseCurrentVersionsOfSelections browseVersions)
('menu & kbd shortcuts' listMenuSpec)
!


!ChangeSorterWindow reorganize!
('GUI building' buildMorphicWindow initialExtent windowColor)
('menu commands' browseMethodConflicts browseVersions trimHistory trimHistoryConfirmationMessage)
('menu & kbd shortcuts' changeSetListMenuSpec classListMenuSpec messageListMenuSpec)
!


!DebuggerWindow reorganize!
('GUI building' buildLowerPanes buildMorphicWindow contextVariableInspector contextVariableInspectorText createCodePaneMorph customButtonRow customButtonSpecs initialExtent receiverInspector receiverInspectorText stackList windowColor)
('menu commands' contextClassBrowse contextClassHierarchy contextClassProtocol exploreContextSelection exploreReceiverSelection inspectContextSelection inspectReceiverSelection receiverClassBrowse receiverClassHierarchy receiverClassProtocol returnValue shiftClickPressedOn:at:)
('open/close' closeView delete)
('user interface' ifOkToChangeCodePaneDo: okToChangeCodePane okToChangeContextVariableInspectorText okToChangeDueTo: okToChangeReceiverInspectorText)
('context stack menu' createMethod doStep fullStack peelToFirst proceed restart runToCursor send stepIntoBlock where)
('menu & kbd shortcuts' contextStackMenuSpec contextVariablesListMenuSpec receiverVariablesListMenuSpec)
('initialization' model:)
!


!MethodSetWindow reorganize!
('GUI building' buildLowerPanes buildMorphicMessageList buildMorphicWindow createCodePaneMorph windowColor)
('menu commands' browsePackage filterMessageList)
('updating' disableCodePaneEditing editSelectionChanged enableCodePaneEditing update:)
('menu & kbd shortcuts' messageListMenuSpec)
!


!MessageNamesWindow reorganize!
('GUI building' buildMorphicWindow submorphToFocusKeyboard windowColor)
('menu & kbd shortcuts' selectorListMenuSpec)
!


!ProtocolBrowserWindow reorganize!
('GUI building' buildCategoriesList buildMorphicWindow buildSelectiveClassList)
('menu & kbd shortcuts' classListMenuSpec messageListMenuSpec)
!


!ChangeSelectorWizardStepWindow reorganize!
('actions' add addToList: do:withEnteredClassLabeled: inform:doesNotImplement: justRefactor refactor remove withClassNamed:do:)
('GUI building' addButton:to:color: addButtonsTo:color: buildLowerPanes buttonsRow)
('compile methods' compiledMethodsFrom:)
('button creation' createAddButton createCancelButton createJustRefactorButton createRefactorButton createRemoveButton)
('initialization' initializeFrom:)
('accessing' oldSelector)
!


!ExtractMethodReplacementsWindow reorganize!
('actions' closeAfter: extractAllInClass extractInMethodOnly extractSelectionOnly refactor remove)
('GUI building' addButton:to:color: addButtonsTo:color: buildLowerPanes buttonsRow)
('button creation' createCancelButton createExtractAllInClassButton createExtractInMethodOnlyButton createExtractSelectionOnlyButton createRefactorButton createRemoveButton)
('initialization' initializeFrom:with:)
('as yet unclassified' createCodePaneMorph)
!


!InlineMethodWizardStepWindow reorganize!
('actions' do:withEnteredClassLabeled: inform:doesNotImplement: justRefactor refactor withClassNamed:do:)
('GUI building' addButton:to:color: addButtonsTo:color: buildLowerPanes buttonsRow)
('compile methods' compiledMethodsFrom:)
('button creation' createCancelButton createJustRefactorButton createRefactorButton createRemoveButton)
('initialization' initializeFrom:)
('testing' isMessageSelected)
('accessing' selectorToInline)
!


!TestResultWindow reorganize!
('actions' debug runSuite)
('button creation' createDebugButton createReRunButton)
('GUI building' addButton:to:color: addButtonsTo:color: buildLowerPanes buttonsRow)
('initialization' initializeFor:)
('testing' isMessageSelected)
!


!PreDebugWindow reorganize!
('GUI building' buildMorphicWindowMessage: buttonRowForPreDebugWindow: initialExtent preDebugButtonSpec windowColor)
('button actions' abandon createMethod debug delete proceed)
('open/close' closeView)
('initialization' model:)
!


!FileListWindow reorganize!
('GUI building' buildMorphicWindow buttonToTriggerIn:for: morphicDirectoryTreePane morphicFileContentsPane morphicFileListPane morphicPatternPane optionalButtonRow updateButtonRow: windowColor)
('updating' updateButtonRow)
('menu & kbd shortcuts' directoryTreeMenuSpec fileListMenuSpec fileSelectedMenuSpec noFileSelectedMenuSpec)
('menu commands' useCuisTopAsTreeRoot useOSrootsAsTreeRoot)
('stepping' stepTime)
('file list formatting')
!


!InspectorWindow reorganize!
('GUI building' buildContentsText buildEvaluatorText buildList buildMorphicWindow initialExtent)
('menu commands' addEntry browseFullProtocol browseHierarchy browseMethodFull copySelectionToClipboard exploreSelection inspectBasic inspectSelectedKey inspectSelection openReferencesFinder openWeightExplorer renameEntry sendersOfSelectedKey)
('menu & kbd shortcuts' fieldListMenuSpec menuSpecForDictionary menuSpecForSet)
!


!ProcessBrowserWindow reorganize!
('initialization' initialize)
('GUI building' buildMorphicWindow)
('menu building')
('menu commands' browseContext changePriority debugProcess exploreContext exploreProcess exploreReceiver findContext inspectContext inspectProcess inspectReceiver openReferencesFinder startAutoUpdate stopAutoUpdate toggleAutoUpdate)
('keyboard shortcuts')
('stepping' stepTime)
('open/close' delete openInWorld)
('menu & kbd shortcuts' processListMenuSpec stackMenuSpec)
!


!TestRunnerWindow reorganize!
('GUI building' buildDetailsText buildErrorsList buildFailuresList buildFilterButton buildLowerPanes buildMorphicWindow buildPassFailText buildRefreshButton buildRunButton buildRunOneButton buildRunProfiledButton buildStopButton buildUpperControls windowColor)
('constants' runButtonColor)
('updating' isNotRunning refreshTests refreshWindow resetProgress update: updateColors updatePartColors:)
('menu commands' browseSelected)
('menu & kbd shortcuts' listMenuSpec)
!


!LayoutSpec reorganize!
('accessing' morph morph:)
('private' updateContainingLayout)
!

