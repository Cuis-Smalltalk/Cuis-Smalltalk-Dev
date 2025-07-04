'From Cuis7.5 [latest update: #7300] on 30 June 2025 at 10:19:15 am'!

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2025 10:18:45'!
initializeColumn

	self initialize.

	gap := self defaultGap.
	axisEdgeWeight := 0.0.
	doAdoptWidgetsColor := false.
	self beColumn.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2025 10:18:34'!
initializeRow

	self initialize.

	gap := self defaultGap.
	axisEdgeWeight := 0.0.
	doAdoptWidgetsColor := false.
	self beRow.! !


!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 6/27/2025 15:16:07'!
newColumn
	^self basicNew initializeColumn! !

!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 6/27/2025 15:16:12'!
newRow
	^self basicNew initializeRow! !

!methodRemoval: LayoutMorph #initialize stamp: 'jmv 6/30/2025 10:17:58'!
LayoutMorph removeSelector: #initialize!
!methodRemoval: LayoutMorph #direction: stamp: 'jmv 6/30/2025 10:17:58'!
LayoutMorph removeSelector: #direction:!
