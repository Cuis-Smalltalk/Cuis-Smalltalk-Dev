'From Cuis 4.2 of 25 July 2013 [latest update: #2218] on 5 April 2015 at 10:02:48.355704 pm'!

!Morph methodsFor: 'accessing - properties' stamp: 'dhn 4/5/2015 10:14'!
name
	"Answer the value of morphName"
	^ self valueOfProperty: #morphName! !

!Morph methodsFor: 'accessing - properties' stamp: 'dhn 4/5/2015 10:12'!
name: anObject
	"Set the morphName property"
	self setProperty: #morphName toValue: anObject! !


!Morph methodsFor: 'printing' stamp: 'dhn 4/5/2015 10:34'!
printOn: aStream 
	"Add the identity of the receiver to a stream"
	super printOn: aStream. "a(n) className"
	aStream 
		nextPut: $(;
		print: self identityHash;
		nextPut: $).
	self valueOfProperty: #morphName ifPresentDo: [:x | aStream nextPutAll: x asString]
! !


!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/5/2015 10:57'!
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
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (BorderedRectMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !

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
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/5/2015 11:08'!
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
		addMorph: (BorderedRectMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 				minorDirectionPadding: #center);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 				minorDirectionPadding: #top);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/5/2015 11:25'!
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
		addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #A)
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 minorDirectionPadding: #bottom);
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #B)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #C)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 minorDirectionPadding: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 200@180 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/5/2015 11:13'!
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
		addMorph: (BorderedRectMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 				minorDirectionPadding: #center);
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 				minorDirectionPadding: #top);
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@130 extent: 400@300.
	pane openInWorld! !

