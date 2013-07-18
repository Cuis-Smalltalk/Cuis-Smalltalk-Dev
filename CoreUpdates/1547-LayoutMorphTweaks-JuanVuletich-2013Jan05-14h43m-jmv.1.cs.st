'From Cuis 4.1 of 12 December 2012 [latest update: #1544] on 5 January 2013 at 2:51:18 pm'!

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 1/5/2013 14:49'!
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
pane openInWorld.
pane morphPosition: 150@130! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 1/5/2013 14:49'!
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
pane openInWorld.
pane morphPosition: 150@230! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 1/5/2013 14:49'!
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
	pane openInWorld.
	pane morphPosition: 250@130! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 1/5/2013 14:50'!
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
	pane openInWorld.
	pane morphPosition: 200@180! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 1/5/2013 14:50'!
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
	pane openInWorld.
	pane morphPosition: 250@230! !

