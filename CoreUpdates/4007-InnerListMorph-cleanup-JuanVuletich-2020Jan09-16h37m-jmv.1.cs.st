'From Cuis 5.0 [latest update: #4006] on 9 January 2020 at 4:38:36 pm'!

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 1/9/2020 16:37:23'!
draw: item atRow: row on: canvas
	"display the given item at row row"
	| f |
	f _ (item is: #Text)
		ifTrue: [ font emphasized: (item emphasisAt: 1) ]
		ifFalse: [ font ].
	canvas
		drawString: item
		at: 0 @ (self drawYForRow: row)
		font: f
		color: Theme current text! !

!methodRemoval: Theme #listSelectedRowText stamp: 'jmv 1/9/2020 16:37:41'!
Theme removeSelector: #listSelectedRowText!
!methodRemoval: Theme #listUnselectedRowText stamp: 'jmv 1/9/2020 16:37:44'!
Theme removeSelector: #listUnselectedRowText!
!methodRemoval: InnerListMorph #colorForRow: stamp: 'jmv 1/9/2020 16:37:28'!
InnerListMorph removeSelector: #colorForRow:!
