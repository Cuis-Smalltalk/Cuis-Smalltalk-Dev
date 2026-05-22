"
A row or column of Morphs, does layout by placing them along either a horizontal axis or a vertical axis.

Submorphs might specify a LayoutSpec. If some don't, then, for a column, the column width is taken as the width, and any morph height is kept. Same for rows: submorph width would be maintained, and submorph height would be made equal to row height.

Notes:
  - Contained submorphs may have fixed or minimum sizes.
  - We can specify fixed separation.
  - We can offset to left or right.
  - Contained submorphs can, via LayoutSizeSpecs, ask for a proportion of
    the space which is left over after the minimum extent.

  One subtlety with the proportional layouts is that the sum of
  proportions may add up to more than 100%.

  We want to allocate fairly.

  Say we have 2 submorphs which ask for 80% and 50%.

  They should be normalized to get 80/130 and 50/130 of the excess space.

Instance Variables:
  direction - either #horizontal (row) or #vertical (column).
  gap - Empty space between Morphs:
	gap for both x and y axes or a point: (xGap @ yGap)
  axisEdgeWeight  - Along the Major or Layout Axis, subMorphs may be attracted 
	toward one side (0.0) or the other (1.0).  
	See comment in method #axisEdgeWeight:
  doAdoptWidgetsColor - see method #adoptWidgetsColor:
  useEdgeSpace - When true, x and y separation are kept between submorphs and the
	edge of our extent, othersize no space is kept between submorphs 
	and our outer edge.


"
Class {
	#name : 'LinearLayoutMorph',
	#superclass : 'LayoutMorph',
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'instance creation' }
LinearLayoutMorph class >> newColumn [
	^self basicNew initializeColumn
]

{ #category : 'instance creation' }
LinearLayoutMorph class >> newRow [
	^self basicNew initializeRow
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addAdjusterAndMorph: aMorph fixedHeight: aNumber [
	"Convenience method.
	Add others as necessary."
	self addAdjusterAndMorph: aMorph layoutSpec: (LayoutSizeSpec fixedHeight: aNumber)
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addAdjusterAndMorph: aMorph layoutSpec: aLayoutSpec [
	"Add a submorph, at the bottom or right, with aLayoutSpec"
	self
		addAdjusterMorph;
		addMorph: aMorph layoutSpec: aLayoutSpec
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addAdjusterAndMorph: aMorph proportionalHeight: aNumber [
	"Convenience method.
	Add others as necessary."
	self addAdjusterAndMorph: aMorph layoutSpec: (LayoutSizeSpec proportionalHeight: aNumber)
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addAdjusterAndMorph: aMorph proportionalWidth: aNumber [
	"Convenience method.
	Add others as necessary."
	self addAdjusterAndMorph: aMorph layoutSpec: (LayoutSizeSpec proportionalWidth: aNumber)
]

{ #category : 'adjust' }
LinearLayoutMorph >> addAdjusterMorph [
	"So the user can adjust layout"

	| thickness |
	thickness := Theme current layoutAdjusterThickness.

	layoutObject isRow ifTrue: [
		self
			addMorph: LayoutAdjustingMorph new
			layoutSpec: (LayoutSizeSpec fixedWidth: thickness) ].

	layoutObject isColumn ifTrue: [
		self
			addMorph: LayoutAdjustingMorph new
			layoutSpec: (LayoutSizeSpec fixedHeight: thickness)]
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addMorph: aMorph fixedHeight: aNumber [
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSizeSpec fixedHeight: aNumber)
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addMorph: aMorph fixedWidth: aNumber [
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSizeSpec fixedWidth: aNumber)
]

{ #category : 'submorphs-add/remove' }
LinearLayoutMorph >> addMorph: aMorph layoutSpec: aLayoutSpec [
	"Add a submorph, at the bottom or right, with aLayoutSpec"
	
	aLayoutSpec isLayoutSizeSpec ifFalse: [ self error: 'Only LayoutSizeSpec can be used in LinearLayoutMorphs' ].
	super addMorph: aMorph layoutSpec: aLayoutSpec
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addMorph: aMorph proportionalHeight: aNumber [
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSizeSpec proportionalHeight: aNumber)
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addMorph: aMorph proportionalWidth: aNumber [
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSizeSpec proportionalWidth: aNumber)
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addMorphKeepMorphHeight: aMorph [
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSizeSpec new useMorphHeight)
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addMorphUseAll: aMorph [
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: LayoutSizeSpec useAll
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addMorphs: morphs [
	"All morphs are made equal width"
	morphs do: [ :m |
		self addMorph: m proportionalWidth: 1 ]
]

{ #category : 'convenience methods' }
LinearLayoutMorph >> addMorphs: morphs widthProportionalTo: widths [
	"Widths can be in any arbitrary unit. The actual widths will be proportional to them."
	morphs with: widths do: [ :m :w |
		self addMorph: m proportionalWidth: w ]
]

{ #category : 'adjust' }
LinearLayoutMorph >> adjustBy: aLayoutAdjustMorph at: aGlobalPoint [
	"See Class Comment of LayoutAdjustingMorph"

	| localPoint  i before after adjusterRefPos |
	localPoint := self internalizeFromWorld: aGlobalPoint.
	i := submorphs indexOf: aLayoutAdjustMorph.
	((i = 1) or: [i = self submorphs size]) ifTrue: [
		self inform: 'LayoutAdjustingMorphs require morphs before and after'.
		^self ].
	before := self submorphs at: i +1.
	after := self submorphs at: i - 1.
	adjusterRefPos := aLayoutAdjustMorph referencePositionInOwner.
	(layoutObject adjustMorphBefore: before morphAfter: after adjusterRefPos: adjusterRefPos at: localPoint)
		ifTrue: [
			self layoutSubmorphs ].
]

{ #category : 'accessing' }
LinearLayoutMorph >> axisEdgeWeight [
	"Only for Linear Layouts"
	^layoutObject axisEdgeWeight
]

{ #category : 'accessing' }
LinearLayoutMorph >> axisEdgeWeight: aSymbolOrNumber [
	"Along the Major or Layout Axis, subMorphs may be attracted toward one side (0.0) or the other (1.0).  This is the LayoutMorph's axisEdgeWeight.  A numeric value is between 0.0 and 1.0

	If a Row, one can specify this weight symbolically as
	  { #rowLeft (0.0), #center (0.5), #rowRight (1.0)}

	If a Column, one can specify the weight symbolically as:
	  { #columnTop (0.0), #center (0.5), #columnBottom (1.0) }
	
	Ignored for other kinds of Layouts"
"	
	self axisEdgeWeight: #rowLeft.	-- axis must be horizontal 
	self axisEdgeWeight: #columnTop.	-- axis must be vertical
"
	layoutObject axisEdgeWeight: aSymbolOrNumber.
	self layoutNeeded: true.
]

{ #category : 'initialization' }
LinearLayoutMorph >> beColumn [
	"Establish the major layout axis, with default edge weight"
	layoutObject := ColumnLayout new.
	layoutObject gap: self defaultGap.
	self layoutNeeded: true.
]

{ #category : 'initialization' }
LinearLayoutMorph >> beRow [
	"Establish the major layout axis, with default edge weight"
	layoutObject := RowLayout new.
	layoutObject gap: self defaultGap.
	self layoutNeeded: true.
]

{ #category : 'aux' }
LinearLayoutMorph >> defaultLayoutSpecFor: aMorph [
	^aMorph defaultLayoutSizeSpec
]

{ #category : 'accessing' }
LinearLayoutMorph >> gap [
	"Only for Linear Layouts"
	^layoutObject gap
]

{ #category : 'accessing' }
LinearLayoutMorph >> gap: aNumber [
	"Only for Linear Layouts"
	layoutObject gap = aNumber ifFalse: [
		self adjustExtentFor: [
			layoutObject gap: aNumber ]].
	self layoutNeeded: true.
]

{ #category : 'initialization' }
LinearLayoutMorph >> initializeColumn [

	layoutObject := ColumnLayout new.
	layoutObject gap: self defaultGap.
	self initialize.
]

{ #category : 'initialization' }
LinearLayoutMorph >> initializeRow [

	layoutObject := RowLayout new.
	layoutObject gap: self defaultGap.
	self initialize.
]

{ #category : 'testing' }
LinearLayoutMorph >> is: aSymbol [
	^ aSymbol == #LinearLayoutMorph or: [ super is: aSymbol ]
]

{ #category : 'testing' }
LinearLayoutMorph >> isColumn [
	"Only makes sense for Linear Layouts"
	^layoutObject isColumn
]

{ #category : 'testing' }
LinearLayoutMorph >> isLayoutSpecOk: aLayoutSpecOrNil [
	"Answer whether the argument can work with us."
	
	^aLayoutSpecOrNil notNil and: [aLayoutSpecOrNil isLayoutSizeSpec]
]

{ #category : 'testing' }
LinearLayoutMorph >> isRow [
	"Only makes sense for Linear Layouts"
	^layoutObject isRow
]

{ #category : 'geometry' }
LinearLayoutMorph >> minimumExtent [
	"Answer size sufficient to frame my submorphs."
	
	^ (layoutObject minimumExtentFor: submorphs cachedValues: IdentityDictionary new) + (2 * self interiorOrigin)
]

{ #category : 'accessing' }
LinearLayoutMorph >> separation: aNumberOrPoint [
	"Only for Linear Layouts"

	| g |
	self padding: aNumberOrPoint.
	g := aNumberOrPoint.
	g isPoint ifTrue: [
		g := layoutObject isRow ifTrue: [ g x ] ifFalse: [ g y ]].
	self gap: g.
]
