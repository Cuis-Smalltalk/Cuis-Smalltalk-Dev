'From Cuis 4.0 of 21 April 2012 [latest update: #1453] on 22 September 2012 at 2:54:35 pm'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/22/2012 14:47'!
endInteraction
	"Clean up after a user interaction with the a halo control"

	(target isInWorld not or: [owner isNil]) ifTrue: [^self].
	self isInWorld 
		ifTrue: [
			"make sure handles show in front"
			self comeToFront.
			self addHandles]! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 9/22/2012 14:52'!
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
pane addMorph: rect1.
rect2 := BorderedRectMorph new color: (Color cyan); 
	morphWidth: 20; morphHeight: 30.
pane addMorph: rect2.
pane
	color: Color lightGreen;
	openInWorld;
	morphPositionInOwner: 120 @ 50;
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
	morphPositionInOwner: 320 @ 50;
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
	morphPositionInOwner: 520 @ 50;
	morphExtent: 180 @ 100! !

