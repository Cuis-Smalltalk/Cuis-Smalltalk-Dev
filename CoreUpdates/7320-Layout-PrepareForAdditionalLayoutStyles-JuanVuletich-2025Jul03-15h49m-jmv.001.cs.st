'From Cuis7.5 [latest update: #7313] on 3 July 2025 at 3:57:45 pm'!

!Layout methodsFor: 'testing' stamp: 'jmv 7/3/2025 15:51:04'!
isLinearLayout
	^false! !


!LinearLayout methodsFor: 'testing' stamp: 'jmv 7/3/2025 15:50:56'!
isLinearLayout
	^true! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/3/2025 15:51:39'!
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
	layout isLinearLayout ifTrue: [
		layout axisEdgeWeight: aSymbolOrNumber.
		self layoutSubmorphs ].! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/3/2025 15:52:16'!
gap: aNumber
	"Only for Linear Layouts"
	layout isLinearLayout ifTrue: [
		layout gap = aNumber ifFalse: [
			self adjustExtentFor: [
				layout gap: aNumber ]].
		self layoutSubmorphs ].! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/3/2025 15:56:52'!
padding: numberOrPoint
	"Honored by all kinds of Layouts"

	super padding: numberOrPoint.
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/3/2025 15:52:50'!
separation: aNumberOrPoint
	"Only for Linear Layouts"

	| g |
	layout isLinearLayout ifTrue: [
		self padding: aNumberOrPoint.
		g := aNumberOrPoint.
		g isPoint ifTrue: [
			g := layout isRow ifTrue: [ g x ] ifFalse: [ g y ]].
		self gap: g ].! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/3/2025 15:54:54'!
defaultGap
	"Answer the default gap between submorphs for the receiver.
	Redefine this method for a different initial gap in your morphs.
	Only for Linear Layouts"

	^ 0! !


!LinearLayout reorganize!
('accessing' axisEdgeWeight axisEdgeWeight: gap gap: gap:axisEdgeWeight:)
('testing' isLinearLayout)
!

