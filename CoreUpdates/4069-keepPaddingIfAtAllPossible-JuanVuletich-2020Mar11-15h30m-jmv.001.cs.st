'From Cuis 5.0 [latest update: #4068] on 11 March 2020 at 3:31:36 pm'!

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 3/11/2020 15:31:04'!
beColumn
	direction _ #vertical.
	padding ifNil: [self padding: #center].
	self refreshExtent.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 3/11/2020 15:31:13'!
beRow
	direction _ #horizontal.
	padding ifNil: [self padding: #left].
	self refreshExtent.! !

