'From Cuis 5.0 [latest update: #4965] on 4 November 2021 at 4:49:32 pm'!

!HierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 11/4/2021 15:52:51'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	self layoutNeeded: true.
	self updateScrollBarsBounds! !


!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 11/4/2021 15:52:17'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	self layoutNeeded: true.
	self updateScrollBarsBounds! !

