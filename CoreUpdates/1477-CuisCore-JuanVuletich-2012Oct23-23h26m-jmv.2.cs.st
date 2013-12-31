'From Cuis 4.0 of 21 April 2012 [latest update: #1475] on 23 October 2012 at 11:30:38 pm'!

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 10/23/2012 23:28'!
testLayout1
	"
	self new testLayout1
	"
	| pane row1 row2 row3 r1c1 r1c2 r1c3 r1c4 r1c5 r2c1 r2c2 r2c3 r3c1 r3c2 r3c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row1 _ LayoutMorph newRow separation: 5.
	row1 color: Color red;
		addMorph: (r1c1 _ BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 10);
		addMorph: (r1c2 _ BorderedRectMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r1c3 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r1c4 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.15);
		addMorph: (r1c5 _ BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
	pane addMorph: row1 layoutSpec: LayoutSpec useAll.
	row2 _ LayoutMorph newRow separation: 5.
	row2 color: Color red;
		addMorph: (r2c1 _ BorderedRectMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r2c2 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r2c3 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.2).
	pane addMorph: row2 layoutSpec: LayoutSpec useAll.
	row3 _ LayoutMorph newRow separation: 5.
	row3 color: Color red;
		addMorph: (r3c1 _ BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
		addMorph: (r3c2 _ BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40);
		addMorph: (r3c3 _ BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row3 layoutSpec: (LayoutSpec fixedHeight: 60).
	pane openInWorld; morphExtent: 408@300.
	ProjectX ui doOneCycleNow.

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

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 10/23/2012 23:28'!
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
		addMorph: (c1 _ BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 minorDirectionPadding: #bottom);
		addMorph: (c2 _ BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (c3 _ BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 minorDirectionPadding: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane openInWorld; morphExtent: 400@300.
	ProjectX ui doOneCycleNow.

	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = (pane morphHeight - 10 * 0.9) rounded.
	self assert: c1 morphBoundsInWorld bottom = (row morphBoundsInWorld bottom - 5) description: 'Should be at bottom'.
	self assert: c1 morphWidth = 20.
	self assert: c1 morphHeight = (row morphHeight - 10 * 0.8) rounded.
	self assert: c2 morphBoundsInWorld top = (row morphBoundsInWorld top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = 256.
	self assert: c2 morphHeight = 40.
	self assert: ((c3 morphBoundsInWorld top - row morphBoundsInWorld top) - (row morphBoundsInWorld bottom - c3 morphBoundsInWorld bottom)) abs < 2 description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (c1 morphHeight / 0.8 * 0.7) rounded.

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 10/23/2012 23:28'!
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
		addMorph: (i1 _ RectangleLikeMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i2 _ RectangleLikeMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i3 _ RectangleLikeMorph new)
			layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 minorDirectionPadding: #center);
		addMorph: (c2 _ BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (c3 _ BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 200).
	pane openInWorld; morphExtent: 400@300.
	ProjectX ui doOneCycleNow.

	self assert: row morphBoundsInWorld left = (pane morphBoundsInWorld left + 5).
	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 200.
	self assert: innerRow morphBoundsInWorld left = (row morphBoundsInWorld left + 5).
	self assert: (innerRow morphBoundsInWorld top - row morphBoundsInWorld top) = (row morphBoundsInWorld bottom - innerRow morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: innerRow morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: innerRow morphHeight = 30.

	self assert: i1 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 5).
	self assert: (i1 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i1 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i1 morphWidth = 10.
	self assert: i1 morphHeight = 10.
	self assert: i2 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 20).
	self assert: (i2 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i2 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i2 morphWidth = 10.
	self assert: i2 morphHeight = 10.
	self assert: i3 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 35).
	self assert: (i3 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i3 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i3 morphWidth = (innerRow morphWidth - 40).
	self assert: i3 morphHeight = 10.

	self assert: c2 morphBoundsInWorld top = (row morphBoundsInWorld top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: c2 morphHeight = 40.
	self assert: (c3 morphBoundsInWorld top - row morphBoundsInWorld top) = (row morphBoundsInWorld bottom - c3 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (row morphHeight - 10).

	pane delete! !


!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 10/23/2012 23:28'!
testClassShow
	taskbar class show.
	self should: [ taskbar isInWorld ].
	ProjectX ui removeMorph: taskbar! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 10/23/2012 23:28'!
testVerticalAlignment
	
	self should: [ taskbar morphBoundsInWorld bottom = ProjectX ui morphBoundsInWorld bottom ]! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 10/23/2012 23:28'!
testWidth
	
	self should: [ taskbar morphWidth = ProjectX ui morphWidth ]! !


!TextEditorTest methodsFor: 'as yet unclassified' stamp: 'jmv 10/23/2012 23:29'!
testSimpleEditor
	"
	TextEditorTest new testSimpleEditor
	"
	| m |
	self shouldnt: [
		m _ OneLineEditorMorph new.
		m editor offerMenuFromEsc:
			(KeyboardEvent new
				setType: #keystroke
				buttons: 0
				position: 0@0
				keyValue: 65
				hand: ProjectX ui activeHand
				stamp: 0)
	] raise: Exception! !


!WorldTest methodsFor: 'as yet unclassified' stamp: 'jmv 10/23/2012 23:30'!
testDoOneCycleWorksWithDeferredQueue
        "Ensure that nested doOneCycles don't break deferred UI messages"
        | finished |
        [
                WorldState addDeferredUIMessage: [ ProjectX ui doOneCycleNow ].
                WorldState addDeferredUIMessage: nil "whatever".
                ProjectX ui doOneCycleNow.
                finished _ true.
        ] valueWithin: 1 seconds onTimeout: [finished _ false ].
        self assert: finished! !

