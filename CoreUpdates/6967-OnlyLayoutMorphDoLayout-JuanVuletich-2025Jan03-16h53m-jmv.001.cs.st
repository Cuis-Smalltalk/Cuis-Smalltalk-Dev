'From Cuis7.3 [latest update: #6963] on 3 January 2025 at 5:01:41 pm'!

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 1/3/2025 16:55:32'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed at any nested level.
	Must layout submorphs again."

	super someSubmorphPositionOrExtentChanged.
	self layoutNeeded: true.! !


!Morph methodsFor: 'layout' stamp: 'jmv 1/3/2025 16:55:41'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed at any nested level."

	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 1/3/2025 17:00:00'!
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

	self layoutNeeded: false.! !


!HierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 1/3/2025 16:57:10'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self updateScrollBarsBounds! !


!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 1/3/2025 16:57:29'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self updateScrollBarsBounds! !

