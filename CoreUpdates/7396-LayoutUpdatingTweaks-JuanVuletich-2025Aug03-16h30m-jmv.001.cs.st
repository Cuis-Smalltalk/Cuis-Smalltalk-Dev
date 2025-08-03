'From Cuis7.5 [latest update: #7395] on 3 August 2025 at 4:40:02 pm'!

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 8/3/2025 16:38:51'!
borderWidth: anInteger

	super borderWidth: anInteger.
	self layoutNeeded: true.! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 8/3/2025 16:38:47'!
padding: numberOrPoint
	"Honored by all kinds of Layouts"

	super padding: numberOrPoint.
	self layoutNeeded: true.! !


!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 8/3/2025 16:39:02'!
axisEdgeWeight: aSymbolOrNumber
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
	self layoutNeeded: true.! !

!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 8/3/2025 16:38:57'!
gap: aNumber
	"Only for Linear Layouts"
	layoutObject gap = aNumber ifFalse: [
		self adjustExtentFor: [
			layoutObject gap: aNumber ]].
	self layoutNeeded: true.! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 8/3/2025 16:39:17'!
beColumn
	"Establish the major layout axis, with default edge weight"
	layoutObject := ColumnLayout new.
	layoutObject gap: self defaultGap.
	self layoutNeeded: true.! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 8/3/2025 16:39:20'!
beRow
	"Establish the major layout axis, with default edge weight"
	layoutObject := RowLayout new.
	layoutObject gap: self defaultGap.
	self layoutNeeded: true.! !


!LayoutSpec methodsFor: 'private' stamp: 'jmv 8/3/2025 16:38:28'!
updateContainingLayout
	| owningLayoutMorph |
	morph ifNotNil: [
		owningLayoutMorph := morph owner.
		(owningLayoutMorph is: #LayoutMorph) ifTrue:
			[ owningLayoutMorph layoutNeeded: true ]].! !


!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 8/3/2025 16:32:50'!
bottomRightEdgesWeight: aPoint
	"Argument x and y must be in [0.0 .. 1.0]"
	bottomRightEdgesWeight := aPoint.
	self updateContainingLayout.! !

!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 8/3/2025 16:33:00'!
bottomRightOffset: aPoint
	bottomRightOffset := aPoint.
	self updateContainingLayout.! !

!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 8/3/2025 16:32:33'!
topLeftEdgesWeight: aPoint
	"Argument x and y must be in [0.0 .. 1.0]"
	topLeftEdgesWeight := aPoint.
	self updateContainingLayout.! !

!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 8/3/2025 16:32:42'!
topLeftOffset: aPoint
	topLeftOffset := aPoint.
	self updateContainingLayout.! !

