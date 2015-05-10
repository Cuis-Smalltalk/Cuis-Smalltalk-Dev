'From Cuis 4.2 of 25 July 2013 [latest update: #2315] on 10 May 2015 at 2:48:46.71928 pm'!

!FunctionGraphMorph class methodsFor: 'as yet unclassified' stamp: 'dhn 4/12/2015 15:31'!
example2
"
	self  example2
"
| g |
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x cos ] color: Color green.
g addFunction: [ :x | (x + 1) squared - 3 ] color: Color red. "parabola with vertex at (-1,-3)"
g openInWorld
! !


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
^ pane! !


!FunctionGraphMorph methodsFor: 'drawing' stamp: 'jmv 5/10/2015 14:48'!
drawOn: aCanvas
	"
	| g |
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x mod: 1 ] color: Color green.
	g addFunction: [ :x | x div: 1 ] color: Color red.
	g openInWorld
	"
	| r y |
	r _ self morphLocalBounds.
	aCanvas
		fillRectangle: r
		color: (Color gray: 0.9).
	aCanvas
		frameAndFillRectangle: r fillColor: (Color gray: 0.9)
		borderWidth: 0.05 borderColor: Color white.
	aCanvas line: 0@yMin to: 0 @ yMax width: 0.05 color: Color lightGray.
	aCanvas line:xMin@0 to: xMax@0 width: 0.05 color: Color lightGray.

	xMin*1.0 to: xMax count: 600 do: [ :x |
		functions with: colors do: [ :f :c |
			y _ f value: x.
			(y between: yMin and: yMax) ifTrue: [
				aCanvas
					fillRectangle: (x@y extent: 0.1)
					color: c ]]]! !

