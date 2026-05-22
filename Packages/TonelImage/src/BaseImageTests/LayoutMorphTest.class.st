Class {
	#name : 'LayoutMorphTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Morphic-Kernel',
	#package : 'BaseImageTests',
	#tag : 'Morphic-Kernel'
}

{ #category : 'tests' }
LayoutMorphTest >> testLayout1 [
	"
	self new testLayout1
	"
	| pane row1 row2 row3 r1c1 r1c2 r1c3 r1c4 r1c5 r2c1 r2c2 r2c3 r3c1 r3c2 r3c3 |
	pane := LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row1 := LayoutMorph newRow separation: 5.
	row1 color: Color red;
		addMorph: (r1c1 := ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 10);
		addMorph: (r1c2 := ColoredBoxMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r1c3 := ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r1c4 := ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.15);
		addMorph: (r1c5 := ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
	pane addMorph: row1 layoutSpec: LayoutSpec useAll.
	row2 := LayoutMorph newRow separation: 5.
	row2 color: Color red;
		addMorph: (r2c1 := ColoredBoxMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r2c2 := ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r2c3 := ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.2).
	pane addMorph: row2 layoutSpec: LayoutSpec useAll.
	row3 := LayoutMorph newRow separation: 5.
	row3 color: Color red;
		addMorph: (r3c1 := ColoredBoxMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
		addMorph: (r3c2 := ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40);
		addMorph: (r3c3 := ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row3 layoutSpec: (LayoutSpec fixedHeight: 60).
	pane openInWorld: UISupervisor ui; morphExtent: 408@300.
	(UISupervisor animatedUIOf: Processor activeProcess)
		ifNotNil: [ :ui | ui doOneCycleNowOn: ui mainCanvas ]
		ifNil: [ | updated d |
			updated := false.
			d := Delay forMilliseconds: 10.
			UISupervisor whenUIinSafeState: [ UISupervisor whenUIinSafeState: [updated := true ]].
			[updated] whileFalse: [
				d wait ]].

	self assert: row1 morphWidth = (pane morphWidth - 10).
	self assert: r1c1 morphWidth = 10.
	self assert: r1c2 morphWidth rounded = 200.
	self assert: r1c3 morphWidth rounded = (r1c2 morphWidth / 0.8 * 0.4) rounded.
	self assert: r1c4 morphWidth rounded = (r1c2 morphWidth / 0.8 * 0.15) rounded.
	self assert: r1c5 morphWidth = 20.
	self assert: r1c1 morphHeight = (row1 morphHeight - 10).
	self assert: r1c2 morphHeight = (row1 morphHeight - 10).
	self assert: r1c3 morphHeight = (row1 morphHeight - 10).
	self assert: r1c4 morphHeight = (row1 morphHeight - 10).
	self assert: r1c5 morphHeight = 20.

	self assert: row2 morphWidth = (pane morphWidth - 10).
	self assert: r2c1 morphWidth rounded = 216.
	self assert: r2c2 morphWidth rounded = 108.
	self assert: r2c3 morphWidth rounded = 54.
	self assert: r2c1 morphHeight = (row2 morphHeight - 10).
	self assert: r2c2 morphHeight = (row2 morphHeight - 10).
	self assert: r2c3 morphHeight = (row2 morphHeight - 10).

	self assert: row3 morphWidth = (pane morphWidth - 10).
	self assert: r3c1 morphWidth = 20.
	self assert: r3c2 morphWidth = (row3 morphWidth - 10 - 20 - 10 - 30 * 0.5) rounded.
	self assert: r3c3 morphWidth = 30.
	self assert: row3 morphHeight = 60.
	self assert: r3c1 morphHeight = 40.
	self assert: r3c2 morphHeight = 40.
	self assert: r3c3 morphHeight = 50.

	pane delete
]

{ #category : 'tests' }
LayoutMorphTest >> testLayout2 [
	"
	self new testLayout2
	"
	| pane row c1 c2 c3 |
	pane := LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row := LayoutMorph newRow separation: 5.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (c1 := ColoredBoxMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 offAxisEdgeWeight: #rowBottom);
		addMorph: (c2 := ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 offAxisEdgeWeight: #rowTop);
		addMorph: (c3 := ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 offAxisEdgeWeight: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 1.0).
	pane openInWorld: UISupervisor ui; morphExtent: 400@271.
	(UISupervisor animatedUIOf: Processor activeProcess)
		ifNotNil: [ :ui | ui doOneCycleNowOn: ui mainCanvas ]
		ifNil: [ | updated d |
			updated := false.
			d := Delay forMilliseconds: 10.
			UISupervisor whenUIinSafeState: [ UISupervisor whenUIinSafeState: [updated := true ]].
			[updated] whileFalse: [
				d wait ]].

	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 261.
	self assert: c1 displayBounds bottom = (row displayBounds bottom - 5) description: 'Should be at bottom'.
	self assert: c1 morphWidth = 20.
	self assert: c1 morphHeight = 200.8.
	self assert: c2 displayBounds top = (row displayBounds top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = 256.
	self assert: c2 morphHeight = 40.
	self assert: ((c3 displayBounds top - row displayBounds top) - (row displayBounds bottom - c3 displayBounds bottom)) abs < 2 description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight rounded = 176.

	pane delete
]

{ #category : 'tests' }
LayoutMorphTest >> testLayout3 [
	"
	self new testLayout3
	"
	| pane row innerRow i1 i2 i3 c2 c3 |
	pane := LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row := LayoutMorph newRow separation: 5.
	innerRow := LayoutMorph newRow color: Color red;  separation: 5.
	innerRow
		addMorph: (i1 := ColoredBoxMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i2 := ColoredBoxMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i3 := ColoredBoxMorph new)
			layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: (c2 := ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #rowTop);
		addMorph: (c3 := ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 200).
	pane openInWorld: UISupervisor ui; morphExtent: 400@300.
	(UISupervisor animatedUIOf: Processor activeProcess)
		ifNotNil: [ :ui | ui doOneCycleNowOn: ui mainCanvas ]
		ifNil: [ | updated d |
			updated := false.
			d := Delay forMilliseconds: 10.
			UISupervisor whenUIinSafeState: [ UISupervisor whenUIinSafeState: [updated := true ]].
			[updated] whileFalse: [
				d wait ]].

	self assert: row displayBounds left = (pane displayBounds left + 5).
	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 200.
	self assert: innerRow displayBounds left = (row displayBounds left + 5).
	self assert: (innerRow displayBounds top - row displayBounds top) = (row displayBounds bottom - innerRow displayBounds bottom) description: 'Should be centered'.
	self assert: innerRow morphWidth = 170.
	self assert: innerRow morphHeight = 30.

	self assert: i1 displayBounds left = (innerRow displayBounds left + 5).
	self assert: (i1 displayBounds top - innerRow displayBounds top) = (innerRow displayBounds bottom - i1 displayBounds bottom) description: 'Should be centered'.
	self assert: i1 morphWidth = 10.
	self assert: i1 morphHeight = 10.
	self assert: i2 displayBounds left = (innerRow displayBounds left + 20).
	self assert: (i2 displayBounds top - innerRow displayBounds top) = (innerRow displayBounds bottom - i2 displayBounds bottom) description: 'Should be centered'.
	self assert: i2 morphWidth = 10.
	self assert: i2 morphHeight = 10.
	self assert: i3 displayBounds left = (innerRow displayBounds left + 35).
	self assert: (i3 displayBounds top - innerRow displayBounds top) = (innerRow displayBounds bottom - i3 displayBounds bottom) description: 'Should be centered'.
	self assert: i3 morphWidth = (innerRow morphWidth - 40).
	self assert: i3 morphHeight = 10.

	self assert: c2 displayBounds top = (row displayBounds top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = 170.
	self assert: c2 morphHeight = 40.
	self assert: (c3 displayBounds top - row displayBounds top) = (row displayBounds bottom - c3 displayBounds bottom) description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (row morphHeight - 10).

	pane delete
]
