'From Cuis7.1 [latest update: #6687] on 10 September 2024 at 12:20:34 pm'!

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/10/2024 12:20:21'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		self layoutNeeded: false.
		^self].

	"Invariant: morphExtent >=  minimumLayoutExtent"
	self adjustExtentFor: [].
	
	direction == #horizontal ifTrue: [
		self layoutSubmorphsHorizontallyIn: self layoutBounds ].

	direction == #vertical ifTrue: [
		self layoutSubmorphsVerticallyIn: self layoutBounds ].

	self invalidateBounds.
	self layoutNeeded: false.! !

!methodRemoval: LayoutMorph #refreshExtent stamp: 'jmv 9/10/2024 12:20:09'!
LayoutMorph removeSelector: #refreshExtent!
