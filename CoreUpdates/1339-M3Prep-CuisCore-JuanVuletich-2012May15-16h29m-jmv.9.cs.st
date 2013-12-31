'From Cuis 4.0 of 21 April 2012 [latest update: #4220] on 15 May 2012 at 6:04:54 pm'!

!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 5/15/2012 16:50'!
placeEmbeddedObject: anchoredFormOrMorph
	"Place the anchoredMorph or return false if it cannot be placed.
	In any event, advance destX by its width."

	| w |
	w _ (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph morphWidth ]
		ifFalse: [ anchoredFormOrMorph width ].
	destX _ destX + w.
	(destX > rightMargin and: [ lastIndex ~= line first ])
		"Won't fit, but  not at start of a line. Start a new line with it"
		ifTrue: [ ^ false].
	lastIndex _ lastIndex + 1.
	^ true! !


!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 5/15/2012 16:52'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	specialWidth _ (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph morphWidth ]
		ifFalse: [ anchoredFormOrMorph width ].
	^ true! !


!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 5/15/2012 16:51'!
placeEmbeddedObject: anchoredFormOrMorph
	| descent h |

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [
		line stop: lastIndex-1.
		^ false].
	descent _ lineHeight - baseline.
	h _ (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph morphHeight ]
		ifFalse: [ anchoredFormOrMorph height ].
	baseline _ baseline max: h.
	lineHeight _ baseline + descent.
	line stop: lastIndex.
	^ true! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 5/15/2012 16:39'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane _ LayoutMorph newRow separation: 5. "1"
pane color: Color lightGreen; bounds: (120 @ 50 extent: 180 @ 100).
pane addMorph: (StringMorph contents: '1').

rect1 := RectangleMorph new color: (Color lightOrange); 
	morphWidth: 20; morphHeight: 30.
pane addMorph: rect1.
rect2 := RectangleMorph new color: (Color cyan); 
	morphWidth: 20; morphHeight: 30.
pane addMorph: rect2.
pane openInWorld.


pane _ LayoutMorph newRow separation: 5. "2"
pane color: Color lightGreen; bounds: (320 @ 50 extent: 180 @ 100).
pane addMorph: (StringMorph contents: '2').

rect1 := RectangleMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect1.
rect2 := RectangleMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane openInWorld.


pane _ LayoutMorph newRow separation: 5. "3"
pane color: Color lightGreen; bounds: (520 @ 50 extent: 180 @ 100).
pane addMorph: (StringMorph contents: '3').

rect1 := RectangleMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
rect2 := RectangleMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect2.
pane openInWorld.! !


!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 5/15/2012 17:49'!
testLayout1
	"
	self new testLayout1
	"
	| pane row1 row2 row3 r1c1 r1c2 r1c3 r1c4 r1c5 r2c1 r2c2 r2c3 r3c1 r3c2 r3c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row1 _ LayoutMorph newRow separation: 5.
	row1 color: Color red;
		addMorph: (r1c1 _ RectangleMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 10);
		addMorph: (r1c2 _ RectangleMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r1c3 _ RectangleMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r1c4 _ RectangleMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.15);
		addMorph: (r1c5 _ RectangleMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
	pane addMorph: row1 layoutSpec: LayoutSpec useAll.
	row2 _ LayoutMorph newRow separation: 5.
	row2 color: Color red;
		addMorph: (r2c1 _ RectangleMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r2c2 _ RectangleMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r2c3 _ RectangleMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.2).
	pane addMorph: row2 layoutSpec: LayoutSpec useAll.
	row3 _ LayoutMorph newRow separation: 5.
	row3 color: Color red;
		addMorph: (r3c1 _ RectangleMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
		addMorph: (r3c2 _ RectangleMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40);
		addMorph: (r3c3 _ RectangleMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row3 layoutSpec: (LayoutSpec fixedHeight: 60).
	pane openInWorld; morphExtent: 408@300.
	World doOneCycleNow.

	self assert: row1 morphWidth = (pane morphWidth - 10).
	self assert: r1c1 morphWidth class == SmallInteger.
	self assert: r1c1 morphHeight class == SmallInteger.
	self assert: r1c1 morphWidth = 10.
	self assert: r1c1 morphHeight = (row1 morphHeight - 10).
	self assert: r1c2 morphWidth = 200.
	self assert: r1c2 morphHeight = (row1 morphHeight - 10).
	self assert: r1c3 morphWidth = (r1c2 morphWidth / 0.8 * 0.4) rounded.
	self assert: r1c3 morphHeight = (row1 morphHeight - 10).
	self assert: r1c4 morphWidth = (r1c2 morphWidth / 0.8 * 0.15) rounded.
	self assert: r1c4 morphHeight = (row1 morphHeight - 10).
	self assert: r1c5 morphWidth = 20.
	self assert: r1c5 morphHeight = 20.

	self assert: row2 morphWidth = (pane morphWidth - 10).
	self assert: r2c1 morphWidth = 216.
	self assert: r2c1 morphHeight = (row2 morphHeight - 10).
	self assert: r2c2 morphWidth = (r2c1 morphWidth / 0.8 * 0.4) rounded.
	self assert: r2c2 morphHeight = (row2 morphHeight - 10).
	self assert: r2c3 morphWidth = (r2c1 morphWidth / 0.8 * 0.2) rounded.
	self assert: r2c3 morphHeight = (row2 morphHeight - 10).

	self assert: row3 morphWidth = (pane morphWidth - 10).
	self assert: row3 morphHeight = 60.
	self assert: r3c1 morphWidth = 20.
	self assert: r3c1 morphHeight = (row3 morphHeight - 10 * 0.8) rounded.
	self assert: r3c2 morphWidth = (row3 morphWidth - 10 - 20 - 10 - 30 * 0.5) rounded.
	self assert: r3c2 morphHeight = 40.
	self assert: r3c3 morphWidth = 30.
	self assert: r3c3 morphHeight = (row3 morphHeight - 10).

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 5/15/2012 17:47'!
testLayout2
	"
	self new testLayout2
	"
	| pane row c1 c2 c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (c1 _ RectangleMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 minorDirectionPadding: #bottom);
		addMorph: (c2 _ RectangleMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (c3 _ RectangleMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 minorDirectionPadding: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane openInWorld; morphExtent: 400@300.
	World doOneCycleNow.

	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = (pane morphHeight - 10 * 0.9) rounded.
	self assert: c1 bounds bottom = (row bounds bottom - 5) description: 'Should be at bottom'.
	self assert: c1 morphWidth = 20.
	self assert: c1 morphHeight = (row morphHeight - 10 * 0.8) rounded.
	self assert: c2 bounds top = (row bounds top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = 256.
	self assert: c2 morphHeight = 40.
	self assert: ((c3 bounds top - row bounds top) - (row bounds bottom - c3 bounds bottom)) abs < 2 description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (c1 morphHeight / 0.8 * 0.7) rounded.

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 5/15/2012 17:45'!
testLayout3
	"
	self new testLayout3
	"
	| pane row innerRow i1 i2 i3 c2 c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	innerRow _ LayoutMorph newRow color: Color red;  separation: 5.
	innerRow
		addMorph: (i1 _ RectangleMorph new  borderWidth: 0)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i2 _ RectangleMorph new  borderWidth: 0)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i3 _ RectangleMorph new  borderWidth: 0)
			layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 minorDirectionPadding: #center);
		addMorph: (c2 _ RectangleMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (c3 _ RectangleMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 200).
	pane openInWorld; morphExtent: 400@300.
	World doOneCycleNow.

	self assert: row bounds left = (pane bounds left + 5).
	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 200.
	self assert: innerRow bounds left = (row bounds left + 5).
	self assert: (innerRow bounds top - row bounds top) = (row bounds bottom - innerRow bounds bottom) description: 'Should be centered'.
	self assert: innerRow morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: innerRow morphHeight = 30.

	self assert: i1 bounds left = (innerRow bounds left + 5).
	self assert: (i1 bounds top - innerRow bounds top) = (innerRow bounds bottom - i1 bounds bottom) description: 'Should be centered'.
	self assert: i1 morphWidth = 10.
	self assert: i1 morphHeight = 10.
	self assert: i2 bounds left = (innerRow bounds left + 20).
	self assert: (i2 bounds top - innerRow bounds top) = (innerRow bounds bottom - i2 bounds bottom) description: 'Should be centered'.
	self assert: i2 morphWidth = 10.
	self assert: i2 morphHeight = 10.
	self assert: i3 bounds left = (innerRow bounds left + 35).
	self assert: (i3 bounds top - innerRow bounds top) = (innerRow bounds bottom - i3 bounds bottom) description: 'Should be centered'.
	self assert: i3 morphWidth = (innerRow morphWidth - 40).
	self assert: i3 morphHeight = 10.

	self assert: c2 bounds top = (row bounds top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: c2 morphHeight = 40.
	self assert: (c3 bounds top - row bounds top) = (row bounds bottom - c3 bounds bottom) description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (row morphHeight - 10).

	pane delete! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 5/15/2012 16:54'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			anchoredFormOrMorph morphPosition:
				((destX - anchoredFormOrMorph morphWidth)@
				(lineY+ line baseline - anchoredFormOrMorph morphHeight)) -
					paraTopLeft ]
		ifFalse: [
			destY _ lineY.
			runX _ destX.
			anchoredFormOrMorph 
				displayOn: canvas grafPort destForm 
				at: destX - anchoredFormOrMorph width @ (destY + line baseline - anchoredFormOrMorph height)
				clippingBox: canvas grafPort clipRect
				rule: Form blend
				fillColor: nil ].
	^ true! !


!ScrollBar methodsFor: 'initialize' stamp: 'jmv 5/15/2012 17:09'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	self addMorph: downButton.
	downButton
		morphPositionInOwner: self morphExtent - borderWidth - e;
		morphExtent: e@e.
	self isHorizontal
		ifTrue: [ downButton updateRightButtonImage ]
		ifFalse: [ downButton updateDownButtonImage ]! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 5/15/2012 17:09'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ self buttonClass new.
	upButton model: self.
	self addMorph: upButton.
	upButton
		morphPositionInOwner: borderWidth@borderWidth;
		morphExtent: e@e.
	self isHorizontal
		ifTrue: [ upButton updateLeftButtonImage ]
		ifFalse: [ upButton updateUpButtonImage ].! !


!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 5/15/2012 17:41'!
testHeight
	
	self should: [ taskbar morphHeight = 18 ]! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 5/15/2012 17:41'!
testWidth
	
	self should: [ taskbar morphWidth = World morphWidth ]! !