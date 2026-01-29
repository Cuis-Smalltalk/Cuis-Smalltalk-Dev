'From Cuis7.7 [latest update: #7819] on 28 January 2026 at 2:45:43 pm'!

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'KenD 28/Jan/2026 14:38:37'!
beColumn
	"Reset the major layout axis.
	 Keep previously set gap and axisEdgeWeight"
	| oldLayoutObject |
	oldLayoutObject := layoutObject.
	layoutObject := ColumnLayout new ::
		gap: oldLayoutObject gap;
		axisEdgeWeight: oldLayoutObject axisEdgeWeight.
	self layoutNeeded: true.! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'KenD 28/Jan/2026 14:38:24'!
beRow
	"Reset the major layout axis.
	 Keep previously set gap and axisEdgeWeight"
	| oldLayoutObject |
	oldLayoutObject := layoutObject.
	layoutObject := RowLayout new ::
		gap: oldLayoutObject gap;
		axisEdgeWeight: oldLayoutObject axisEdgeWeight.
	self layoutNeeded: true.! !

