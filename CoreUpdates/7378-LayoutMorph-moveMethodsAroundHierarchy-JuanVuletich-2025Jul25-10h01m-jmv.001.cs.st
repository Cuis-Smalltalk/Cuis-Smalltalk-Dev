'From Cuis7.5 [latest update: #7372] on 25 July 2025 at 10:09:54 am'!

!FormLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 10:03:33'!
initialize

	super initialize.
	layoutObject := FormLayout new.! !


!LinearLayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/25/2025 10:02:00'!
newColumn
	^self basicNew initializeColumn! !

!LinearLayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/25/2025 10:02:11'!
newRow
	^self basicNew initializeRow! !


!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/25/2025 10:02:53'!
initializedInstance
	^LinearLayoutMorph newRow! !

!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/25/2025 10:02:21'!
newColumn
	^LinearLayoutMorph newColumn! !

!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/25/2025 10:01:35'!
newForm
	^FormLayoutMorph new! !

!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/25/2025 10:02:27'!
newRow
	^LinearLayoutMorph newRow! !


!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 10:08:10'!
axisEdgeWeight
	"Only for Linear Layouts"
	^layoutObject axisEdgeWeight! !

!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 10:08:27'!
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
	self layoutSubmorphs.! !

!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 10:08:39'!
gap
	"Only for Linear Layouts"
	^layoutObject gap! !

!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 10:08:49'!
gap: aNumber
	"Only for Linear Layouts"
	layoutObject gap = aNumber ifFalse: [
		self adjustExtentFor: [
			layoutObject gap: aNumber ]].
	self layoutSubmorphs.! !

!LinearLayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 10:08:57'!
separation: aNumberOrPoint
	"Only for Linear Layouts"

	| g |
	self padding: aNumberOrPoint.
	g := aNumberOrPoint.
	g isPoint ifTrue: [
		g := layoutObject isRow ifTrue: [ g x ] ifFalse: [ g y ]].
	self gap: g.! !

!methodRemoval: LinearLayout #isLinearLayout stamp: 'jmv 7/25/2025 10:09:19'!
LinearLayout removeSelector: #isLinearLayout!
!methodRemoval: Layout #isLinearLayout stamp: 'jmv 7/25/2025 10:09:18'!
Layout removeSelector: #isLinearLayout!
!methodRemoval: FormLayoutMorph class #new stamp: 'jmv 7/25/2025 10:06:56'!
FormLayoutMorph class removeSelector: #new!
!methodRemoval: FormLayoutMorph #initializeForm stamp: 'jmv 7/25/2025 10:04:44'!
FormLayoutMorph removeSelector: #initializeForm!
!methodRemoval: LayoutMorph class #new stamp: 'jmv 7/25/2025 10:06:52'!
LayoutMorph class removeSelector: #new!

!FormLayoutMorph class reorganize!
!


!LinearLayoutMorph class reorganize!
('instance creation' newColumn newRow)
!

