'From Cuis7.7 [latest update: #7823] on 1 February 2026 at 11:41:51 am'!

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'hlsf 1/Feb/2026 11:41:29'!
beColumn
	"Reset the major layout axis.
	 Keep previously set gap and axisEdgeWeight"
	layoutObject := ColumnLayout new ::
		gap: (layoutObject ifNil: [self defaultGap] ifNotNil: [layoutObject gap]);
		axisEdgeWeight: (layoutObject ifNil: [0] ifNotNil: [layoutObject axisEdgeWeight]).
	self layoutNeeded: true! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'hlsf 1/Feb/2026 11:41:39'!
beRow
	"Reset the major layout axis.
	 Keep previously set gap and axisEdgeWeight"
	layoutObject := RowLayout new ::
		gap: (layoutObject ifNil: [self defaultGap] ifNotNil: [layoutObject gap]);
		axisEdgeWeight: (layoutObject ifNil: [0] ifNotNil: [layoutObject axisEdgeWeight]).
	self layoutNeeded: true! !

