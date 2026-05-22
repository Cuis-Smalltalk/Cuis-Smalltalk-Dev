Class {
	#name : 'ResizeMorph',
	#superclass : 'ColoredBoxMorph',
	#instVars : [
		'gridLineWidth',
		'gridColor',
		'selectionColor',
		'outlineMorph',
		'grid',
		'from',
		'to',
		'action'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
ResizeMorph class >> categoryInNewMorphMenu [
	^ 'Views'
]

{ #category : 'accessing' }
ResizeMorph >> action: aBlock [
	action := aBlock
]

{ #category : 'drawing' }
ResizeMorph >> drawGridOn: aCanvas [
	0 to: grid x do: [:i |
		| x |
		x := i * (extent x - gridLineWidth) / grid x.
		aCanvas line: x @ 0 to: x @ (extent y - 2) width: gridLineWidth color: gridColor].
	0 to: grid y do: [:i |
		| y |
		y := i * (extent y - gridLineWidth) / grid y.
		aCanvas line: 0 @ y to: (extent x - 2) @ y width: gridLineWidth color: gridColor]
]

{ #category : 'drawing' }
ResizeMorph >> drawOn: aCanvas [
	super drawOn: aCanvas.
	from ifNotNil: [aCanvas fillRectangle: (self selectionRectangle: extent) color: selectionColor].
	self drawGridOn: aCanvas
]

{ #category : 'accessing' }
ResizeMorph >> grid: aPoint [
	grid := aPoint
]

{ #category : 'event handling testing' }
ResizeMorph >> handlesMouseDown: aMouseButtonEvent [
	^true
]

{ #category : 'initialization' }
ResizeMorph >> initialize [
	super initialize.
	extent := `400@300`.
	color := `Color white`.
	grid := `8@6`.
	gridLineWidth := 2.
	gridColor := `Color black`.
	selectionColor := `Color red`
]

{ #category : 'events' }
ResizeMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	from := self toGridPoint: localEventPosition.
	outlineMorph := BorderedBoxMorph new
		borderColor: `Color black`;
		color: `Color transparent`;
		openInWorld;
		hide.
	self selectTo: localEventPosition
]

{ #category : 'events' }
ResizeMorph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [
	self selectTo: localEventPosition.
	outlineMorph delete.
	action ifNotNil: [
		action value.
		self delete]
]

{ #category : 'events' }
ResizeMorph >> mouseMove: aMouseMoveEvent localPosition: localEventPosition [

	aMouseMoveEvent anyButtonPressed ifTrue: [
		self selectTo: localEventPosition ]
]

{ #category : 'printing' }
ResizeMorph >> printOn: aStream [
	super printOn: aStream.
	aStream space; print: from; space; print: to
]

{ #category : 'as yet unclassified' }
ResizeMorph >> selectTo: localEventPosition [
	| newTo |
	newTo := self toGridPoint: localEventPosition.
	newTo ~= to ifTrue: [
		to := newTo.
		self redrawNeeded.
		self updateOutlineMorph]
]

{ #category : 'as yet unclassified' }
ResizeMorph >> selectionRectangle: aRectangle [
	^(from corner: to + 1) scaledBy: aRectangle // grid
]

{ #category : 'as yet unclassified' }
ResizeMorph >> toGridPoint: aPoint [
	^(aPoint min: extent - 1) // (extent // grid)
]

{ #category : 'as yet unclassified' }
ResizeMorph >> updateOutlineMorph [
	| rectangle |
	rectangle := self selectionRectangle: Display extent.
	outlineMorph
		position: rectangle origin extent: rectangle extent;
		show
]
