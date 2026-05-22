Class {
	#name : 'TileResizeMorph',
	#superclass : 'BorderedBoxMorph',
	#instVars : [
		'selectedResize',
		'selectionColor',
		'action'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'as yet unclassified' }
TileResizeMorph class >> allowedArea [
	^(RealEstateAgent maximumUsableAreaInWorld: self runningWorld) insetBy: Theme current fullScreenDeskMargin
]

{ #category : 'as yet unclassified' }
TileResizeMorph class >> calculateTileRegions: aNumber [
	
	^ self calculateTileRegionsIn: self allowedArea by: aNumber
]

{ #category : 'as yet unclassified' }
TileResizeMorph class >> calculateTileRegionsHorizontallyIn: aRectangle by: aNumber [

	| rects rects1 rects2 |
	aNumber <= 2 ifTrue: [^ self divideRectHorizontally: aRectangle by: aNumber].
	
	rects := self divideRectHorizontally: aRectangle by: 2.

	rects1 := self calculateTileRegionsVerticallyIn: rects first by: aNumber // 2.
	rects2 :=  self calculateTileRegionsVerticallyIn: rects second by: (aNumber - (aNumber // 2)). 
	
	^ rects1, rects2
]

{ #category : 'as yet unclassified' }
TileResizeMorph class >> calculateTileRegionsIn: aRectangle by: aNumber [
	
	^ self calculateTileRegionsHorizontallyIn: aRectangle by: aNumber
]

{ #category : 'as yet unclassified' }
TileResizeMorph class >> calculateTileRegionsVerticallyIn: aRectangle by: aNumber [

	| rects rects1 rects2 |
	aNumber <= 3 ifTrue: [^ self divideRectVertically: aRectangle by: aNumber].
	
	rects := self divideRectVertically: aRectangle by: 2.

	rects1 := self calculateTileRegionsHorizontallyIn: rects first by: aNumber // 2.
	rects2 :=  self calculateTileRegionsHorizontallyIn: rects second by: (aNumber - (aNumber // 2)). 
	
	^ rects1, rects2
]

{ #category : 'new-morph participation' }
TileResizeMorph class >> categoryInNewMorphMenu [

	^ 'User Interface'
]

{ #category : 'as yet unclassified' }
TileResizeMorph class >> divideRectHorizontally: aRectangle by: aNumber [

	| w x rects |
	
	x := aRectangle origin x.
	w := aRectangle width // aNumber.
	rects := OrderedCollection new.
	
	aNumber timesRepeat: [ |rect|
		rect := Rectangle origin: x@aRectangle origin y extent: w@aRectangle height.
		x := x + w.
		rects add: rect].
	
	 ^ rects
	
	
]

{ #category : 'as yet unclassified' }
TileResizeMorph class >> divideRectVertically: aRectangle by: aNumber [

	| h y rects |
	
	y := aRectangle origin y.
	h := aRectangle height // aNumber.
	rects := OrderedCollection new.
	
	aNumber timesRepeat: [ |rect|
		rect := Rectangle origin: aRectangle origin x@y extent: aRectangle width@h.
		y := y + h.
		rects add: rect].
	
	 ^ rects
	
	
]

{ #category : 'as yet unclassified' }
TileResizeMorph class >> tileOpenWindows [
	
	|windows regions i |
	
	windows := (SystemWindow
		windowsIn: self runningWorld
		satisfying: [ :w | w visible]).
		
	windows ifEmpty: [^ self].
		
	regions := self calculateTileRegions: windows size.
	
	i := 1.
	windows do: [:w | |r|
		r := regions at: i.
		w resize: r.
		i := i + 1]
]

{ #category : 'as yet unclassified' }
TileResizeMorph >> action: aBlock [
	action := aBlock
]

{ #category : 'drawing' }
TileResizeMorph >> drawOn: aCanvas [

	super drawOn: aCanvas.
	
	selectedResize ifNil: [^ self].
	
	aCanvas fillRectangle: (self selectionRectangle: selectedResize) 
			color: selectionColor
			borderWidth: borderWidth
			borderStyleSymbol: #simple
			baseColorForBorder: borderColor.
]

{ #category : 'event handling testing' }
TileResizeMorph >> handlesMouseDown: aMouseButtonEvent [

	^ true
]

{ #category : 'event handling testing' }
TileResizeMorph >> handlesMouseHover [
	^ true
]

{ #category : 'initialization' }
TileResizeMorph >> initialize [

	super initialize.
	extent := 120@60.
	color := Color white.
	selectionColor := Color lightYellow .
	self borderColor: Color black.
	self borderWidth: 1.
]

{ #category : 'events' }
TileResizeMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	self selectResize: localEventPosition
]

{ #category : 'events' }
TileResizeMorph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [
	self selectResize: localEventPosition.
	action ifNotNil: [
		action value: selectedResize.
		self delete]
]

{ #category : 'events' }
TileResizeMorph >> mouseHover: aMouseOverEvent localPosition: localEventPosition [
	self selectResize: localEventPosition
]

{ #category : 'as yet unclassified' }
TileResizeMorph >> resizeAtPoint: aPoint [
	
	|region|
		
	region := (aPoint min: extent - 1) // (extent // 3).
	
	^ region caseOf: {
		[`0@0`] -> [#topLeft].
		[`1@0`] -> [#top].
		[`2@0`] -> [#topRight].
		[`0@1`] -> [#left].
		[`1@1`] -> [#full].
		[`2@1`] -> [#right].
		[`0@2`] -> [#bottomLeft].
		[`1@2`] -> [#bottom].
		[`2@2`] -> [#bottomRight].
	} otherwise: [nil]
]

{ #category : 'as yet unclassified' }
TileResizeMorph >> selectResize: localEventPosition [

	| newResize |
	
	newResize := self resizeAtPoint: localEventPosition.
	newResize ~= selectedResize ifTrue: [
		selectedResize := newResize.
		self redrawNeeded]
]

{ #category : 'as yet unclassified' }
TileResizeMorph >> selectionColor: aColor [

	selectionColor := aColor
]

{ #category : 'as yet unclassified' }
TileResizeMorph >> selectionRectangle: region [

	^ region caseOf: {
		[#topLeft] -> [`0@0` corner: (extent // 2)].
		[#top] -> [`0@0` corner: (extent x@(extent y // 2))].
		[#topRight] -> [(extent x // 2)@0 corner: (extent x@(extent y // 2))].
		[#left] -> [`0@0` corner: (extent x // 2)@extent y].
		[#full] -> [`0@0` corner: extent].
		[#right] -> [(extent x // 2)@0 corner: extent].
		[#bottomLeft] -> [0@(extent y // 2) corner: (extent x // 2)@extent y].
		[#bottomRight] -> [(extent x // 2)@(extent y // 2) corner: extent].
		[#bottom] -> [0@(extent y // 2) corner: extent].
	}
]
