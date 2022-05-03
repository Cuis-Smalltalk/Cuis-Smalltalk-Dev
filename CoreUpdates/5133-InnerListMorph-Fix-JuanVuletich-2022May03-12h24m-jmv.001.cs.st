'From Cuis 6.0 [latest update: #5132] on 3 May 2022 at 12:24:17 pm'!

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 5/3/2022 12:24:14'!
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
		drawString: item string
		at: 0 @ (self drawYForRow: row)
		font: f
		color: c! !

