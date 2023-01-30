'From Cuis 6.0 [latest update: #5658] on 30 January 2023 at 5:41:00 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 1/30/2023 17:11:01'!
drawHighlight: aMorph

	aMorph displayBounds ifNotNil: [ :r |
		self
			privateFrameAndFillGlobalRect: (r intersect: clipRect)
			fillColor: `Color pink alpha: 0.1`
			borderWidth: 2
			color: `Color black`
			].! !

