'From Cuis 5.0 [latest update: #4032] on 28 January 2020 at 9:56:35 am'!

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 1/28/2020 09:55:26'!
draw: item atRow: row on: canvas
	"display the given item at row row"
	| f c |
	(item is: #Text)
		ifTrue: [
			f _ font emphasized: (item emphasisAt: 1).
			c _ (item colorAt: 1) ifNil: [Theme current text]]
		ifFalse: [ 
			f _ font.
			c _  Theme current text].
	canvas
		drawString: item
		at: 0 @ (self drawYForRow: row)
		font: f
		color: c! !

