'From Cuis7.5 [latest update: #7399] on 11 August 2025 at 2:58:38 pm'!

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/11/2025 14:56:49'!
visibleRowIntervalForCanvas: aCanvas
	"return the interval visible rows in aCanvas's clip rectangle"
	| tx clipRect rows |
	tx := aCanvas currentTransformation.
	clipRect := aCanvas clipRect.
	rows := clipRect corners collect: [ :corner |
		self rowAtLocation: (tx internalizePosition: corner) ].
	^rows min to: rows max.! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/11/2025 14:56:07'!
drawOn: aCanvas

	listItems isEmpty ifTrue: [ ^self ].
 
	self drawHighlightOn: aCanvas.
	self drawSelectionOn: aCanvas.

	(self visibleRowIntervalForCanvas: aCanvas)
		do: [ :row |
			(owner itemSelectedAmongMultiple:  row) ifTrue: [
				self drawBackgroundForMulti: row on: aCanvas. ].
			self draw: (self item: row) displayStringOrText atRow: row on: aCanvas ]! !

!methodRemoval: InnerListMorph #bottomVisibleRowForCanvas: stamp: 'jmv 8/11/2025 14:58:22'!
InnerListMorph removeSelector: #bottomVisibleRowForCanvas:!
!methodRemoval: InnerListMorph #topVisibleRowForCanvas: stamp: 'jmv 8/11/2025 14:58:22'!
InnerListMorph removeSelector: #topVisibleRowForCanvas:!
