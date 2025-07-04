'From Cuis7.5 [latest update: #7303] on 30 June 2025 at 12:07:01 pm'!

!Layout methodsFor: 'testing' stamp: 'jmv 6/30/2025 12:00:32'!
isColumn
	^false! !

!Layout methodsFor: 'testing' stamp: 'jmv 6/30/2025 12:00:27'!
isRow
	^false! !


!ColumnLayout methodsFor: 'testing' stamp: 'jmv 6/30/2025 12:00:43'!
isColumn
	^true! !


!RowLayout methodsFor: 'testing' stamp: 'jmv 6/30/2025 12:00:55'!
isRow
	^true! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 6/30/2025 12:03:02'!
axisEdgeWeight: aSymbolOrNumber
	"Along the Major or Layout Axis, subMorphs may be attracted toward one side (0.0) or the other (1.0).  This is the 	 	 LayoutMorph's axisEdgeWeight.  A numeric value is between 0.0 and 1.0

	If a Row, one can specify this weight symbolically as
	  { #rowLeft (0.0), #center (0.5), #rowRight (1.0)}

	If a Column, one can specify the weight symbolically as:
	  { #columnTop (0.0), #center (0.5), #columnBottom (1.0) }"
"	
	self axisEdgeWeight: #rowLeft.	-- axis must be horizontal 
	self axisEdgeWeight: #columnTop.	-- axis must be vertical
"
	| edgeWeight |
	(aSymbolOrNumber is: #Number)
		ifTrue: [ (aSymbolOrNumber between: 0.0 and: 1.0)
			ifTrue: [edgeWeight := aSymbolOrNumber ] 
			ifFalse: [self error: 'axisEdgeWeight ', aSymbolOrNumber printString, ' is out of range 0.0 to 1.0']
		]
		ifFalse: [
			layout isRow 
				ifTrue: [ "Row"
					edgeWeight := aSymbolOrNumber
						caseOf: {
							[ #rowLeft ] -> [ 0.0 ].
							[ #center ] -> [ 0.5 ].
							[ #rowRight ] -> [ 1.0 ]
						}
						otherwise: [ self error: 'bad axisEdgeWeight for Row: ', aSymbolOrNumber printString ].
				]
				ifFalse: [ "vertical => Column"
					edgeWeight := aSymbolOrNumber
						caseOf: {
							[ #columnTop ] -> [ 0.0 ].
							[ #center ] -> [ 0.5 ].
							[ #columnBottom ] -> [ 1.0 ]
						}
						otherwise: [ self error: 'bad axisEdgeWeight for Column: ', aSymbolOrNumber printString ].
				]
		].
	layout axisEdgeWeight: edgeWeight.
	self layoutSubmorphs! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 6/30/2025 12:03:46'!
separation: aNumberOrPoint

	| g |
	self padding: aNumberOrPoint.
	g := aNumberOrPoint.
	g isPoint ifTrue: [
		g := layout isRow ifTrue: [ g x ] ifFalse: [ g y ]].
	self gap: g.! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 6/30/2025 12:02:43'!
adjustBy: aLayoutAdjustMorph at: aGlobalPoint
	"See Class Comment of LayoutAdjustingMorph"

	| localPoint |
	localPoint := self internalizeFromWorld: aGlobalPoint.

	layout isRow ifTrue: [
		self adjustHorizontallyBy: aLayoutAdjustMorph at: localPoint ].

	layout isColumn ifTrue: [
		self adjustVerticallyBy: aLayoutAdjustMorph at: localPoint ].! !

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 6/30/2025 12:03:21'!
minimumExtent: cachedValues
	"Answer size sufficient to frame my submorphs."
	
	| width height mle |
	width  := 0.
	height := 0.
	layout isColumn
		ifTrue: [ "Column"
			self submorphsToLayout do: [ :sm |
				mle := sm minimumLayoutExtent: cachedValues.
				"use maximum width across submorphs"
				width := width max: mle x.
				"sum up submorph heights, including separation between them"
				height := height + mle y + layout gap.
			].
			height := height - layout gap. "An extra gap was just added"
		]
		ifFalse: [ "Row"
			self submorphsToLayout do: [ :sm |
				mle := sm minimumLayoutExtent: cachedValues.
				"sum up submorphs width, include gap"
				width := width + mle x + layout gap.
				"use maximum height across submorph"
				height := height max: mle y.
			].
			width := width - layout gap. "An extra gap was just added"
		].
	^ (width @ height) + (2 * self interiorOrigin)! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2025 12:04:10'!
beColumn
	"Establish the major layout axis, with default edge weight"
	layout := ColumnLayout new.
	layout gap: self defaultGap.
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2025 12:04:07'!
beRow
	"Establish the major layout axis, with default edge weight"
	layout := RowLayout new.
	layout gap: self defaultGap.
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 6/30/2025 12:01:58'!
addAdjusterMorph
	"So the user can adjust layout"

	| thickness |
	thickness := Theme current layoutAdjusterThickness.

	layout isRow ifTrue: [
		self
			addMorph: LayoutAdjustingMorph new
			layoutSpec: (LayoutSizeSpec fixedWidth: thickness) ].

	layout isColumn ifTrue: [
		self
			addMorph: LayoutAdjustingMorph new
			layoutSpec: (LayoutSizeSpec fixedHeight: thickness)]! !

!LayoutMorph methodsFor: 'testing' stamp: 'jmv 6/30/2025 12:01:30'!
isColumn

	^ layout isColumn! !

!LayoutMorph methodsFor: 'testing' stamp: 'jmv 6/30/2025 12:01:22'!
isRow

	^ layout isRow! !


!LayoutAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 6/30/2025 11:54:30'!
cursor
	^ layoutSpec fixedWidth = Theme current layoutAdjusterThickness
		ifTrue: [ Cursor cursorAt: #resizeLeftCursor ]
		ifFalse: [ Cursor cursorAt: #resizeTopCursor ].! !

!methodRemoval: LayoutMorph #direction stamp: 'jmv 6/30/2025 11:54:59'!
LayoutMorph removeSelector: #direction!
!methodRemoval: LayoutMorph #desiredLayoutHeight stamp: 'jmv 6/30/2025 11:44:50'!
LayoutMorph removeSelector: #desiredLayoutHeight!

!Layout reorganize!
('testing' isColumn isRow)
!


!ColumnLayout reorganize!
('initialization' initialize)
('layout' heightsFor:within:minLayoutExtentCache: layoutMorphs:in: offAxisWidthFor:within:minLayoutExtentCache: offAxisXOffsetFor:within:)
('testing' isColumn)
!


!RowLayout reorganize!
('initialization' initialize)
('layout' layoutMorphs:in: offAxisHeightFor:within:minLayoutExtentCache: offAxisYOffsetFor:within: widthsFor:within:minLayoutExtentCache:)
('testing' isRow)
!

