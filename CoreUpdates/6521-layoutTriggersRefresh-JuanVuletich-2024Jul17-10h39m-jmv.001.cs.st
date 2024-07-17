'From Cuis7.1 [latest update: #6520] on 17 July 2024 at 10:39:47 am'!

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 7/17/2024 10:39:12'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		self layoutNeeded: false.
		^self].

	"Invariant: morphExtent >=  minimumLayoutExtent"
	self refreshExtent.
	
	direction == #horizontal ifTrue: [
		self layoutSubmorphsHorizontallyIn: self layoutBounds ].

	direction == #vertical ifTrue: [
		self layoutSubmorphsVerticallyIn: self layoutBounds ].

	self invalidateBounds.
	self layoutNeeded: false.! !

