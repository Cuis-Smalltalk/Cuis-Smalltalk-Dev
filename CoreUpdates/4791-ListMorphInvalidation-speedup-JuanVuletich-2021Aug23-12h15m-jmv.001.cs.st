'From Cuis 5.0 [latest update: #4789] on 23 August 2021 at 12:15:48 pm'!

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 8/22/2021 19:02:22'!
highlightedRow: n
	highlightedRow = n ifFalse: [
		highlightedRow ifNotNil: [
			self invalidateLocalRect: (self drawBoundsForRow: highlightedRow) ].
		highlightedRow _ n.
		highlightedRow ifNotNil: [
			self invalidateLocalRect: (self drawBoundsForRow: highlightedRow) ].
		].! !

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 8/22/2021 19:03:57'!
selectedRow: index
	"select the index-th row.  if nil, remove the current selection"
	selectedRow = index ifFalse: [
		selectedRow ifNotNil: [
			self invalidateLocalRect: (self drawBoundsForRow: selectedRow) ].
		highlightedRow ifNotNil: [
			self invalidateLocalRect: (self drawBoundsForRow: highlightedRow) ].
		selectedRow _ index.
		highlightedRow _ nil.
		selectedRow ifNotNil: [
			self invalidateLocalRect: (self drawBoundsForRow: selectedRow) ].
		].! !

