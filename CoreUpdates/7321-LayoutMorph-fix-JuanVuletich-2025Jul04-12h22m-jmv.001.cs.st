'From Cuis7.5 [latest update: #7320] on 4 July 2025 at 12:22:30 pm'!

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/4/2025 12:22:15'!
initializeColumn

	layout := ColumnLayout new.
	layout gap: self defaultGap.
	doAdoptWidgetsColor := false.
	self initialize.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/4/2025 12:22:13'!
initializeRow

	layout := RowLayout new.
	layout gap: self defaultGap.
	doAdoptWidgetsColor := false.
	self initialize.! !

