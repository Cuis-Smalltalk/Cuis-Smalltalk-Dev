'From Cuis7.5 [latest update: #7372] on 25 July 2025 at 9:47:28 am'!

!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 09:46:17'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	adoptsWidgetsColor
		ifTrue: [ self color: (Theme current buttonColorFrom: color) ]
		ifFalse: [ self color: `Color transparent` ]! !

!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 09:45:18'!
axisEdgeWeight
	"Only for Linear Layouts"
	^layoutObject isLinearLayout ifTrue: [
		layoutObject axisEdgeWeight ]! !

!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 09:45:23'!
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
	layoutObject isLinearLayout ifTrue: [
		layoutObject axisEdgeWeight: aSymbolOrNumber.
		self layoutSubmorphs ].! !

!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 09:45:28'!
gap
	"Only for Linear Layouts"
	^layoutObject isLinearLayout ifTrue: [
		layoutObject gap ]! !

!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 09:45:36'!
gap: aNumber
	"Only for Linear Layouts"
	layoutObject isLinearLayout ifTrue: [
		layoutObject gap = aNumber ifFalse: [
			self adjustExtentFor: [
				layoutObject gap: aNumber ]].
		self layoutSubmorphs ].! !

!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 09:43:31'!
separation: aNumberOrPoint
	"Only for Linear Layouts"

	| g |
	layoutObject isLinearLayout ifTrue: [
		self padding: aNumberOrPoint.
		g := aNumberOrPoint.
		g isPoint ifTrue: [
			g := layoutObject isRow ifTrue: [ g x ] ifFalse: [ g y ]].
		self gap: g ].! !

!LinearLayoutMorph methodsFor: 'adjust' stamp: 'jmv 7/25/2025 09:45:12'!
adjustBy: aLayoutAdjustMorph at: aGlobalPoint
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
			self layoutSubmorphs ].! !

!LinearLayoutMorph methodsFor: 'geometry' stamp: 'jmv 7/25/2025 09:43:35'!
minimumExtent
	"Answer size sufficient to frame my submorphs."
	
	^ (layoutObject minimumExtentFor: submorphs cachedValues: IdentityDictionary new) + (2 * self interiorOrigin)! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:43:24'!
beColumn
	"Establish the major layout axis, with default edge weight"
	layoutObject := ColumnLayout new.
	layoutObject gap: self defaultGap.
	self layoutSubmorphs.! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:43:18'!
beRow
	"Establish the major layout axis, with default edge weight"
	layoutObject := RowLayout new.
	layoutObject gap: self defaultGap.
	self layoutSubmorphs.! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:46:38'!
initializeColumn

	layoutObject := ColumnLayout new.
	layoutObject gap: self defaultGap.
	self initialize.! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:46:47'!
initializeForm

	layoutObject := FormLayout new.
	self initialize.! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:46:50'!
initializeRow

	layoutObject := RowLayout new.
	layoutObject gap: self defaultGap.
	self initialize.! !

!LinearLayoutMorph methodsFor: 'layout' stamp: 'jmv 7/25/2025 09:43:44'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		self layoutNeeded: false.
		^self ].

	"Invariant: morphExtent >=  minimumLayoutExtent"
	self adjustExtentFor: nil.

	layoutObject layoutMorphs: submorphs in: self layoutBounds.
	self layoutNeeded: false.! !

!LinearLayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/25/2025 09:45:06'!
addAdjusterMorph
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
			layoutSpec: (LayoutSizeSpec fixedHeight: thickness)]! !

!LinearLayoutMorph methodsFor: 'testing' stamp: 'jmv 7/25/2025 09:45:40'!
isColumn
	"Only makes sense for Linear Layouts"
	^layoutObject isColumn! !

!LinearLayoutMorph methodsFor: 'testing' stamp: 'jmv 7/25/2025 09:45:43'!
isRow
	"Only makes sense for Linear Layouts"
	^layoutObject isRow! !

!methodRemoval: LinearLayoutMorph #doAdoptWidgetsColor stamp: 'jmv 7/25/2025 09:46:34'!
LinearLayoutMorph removeSelector: #doAdoptWidgetsColor!
