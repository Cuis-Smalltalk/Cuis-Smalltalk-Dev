'From Cuis7.3 [latest update: #6963] on 3 January 2025 at 5:11:13 pm'!

!Morph methodsFor: 'layout' stamp: 'jmv 1/3/2025 17:11:04'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed position or extent.
	Some Morph classes need to adjust for that."! !


!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 1/3/2025 17:09:19'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed position or extent.
	Must layout submorphs again."

	self layoutNeeded: true.! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 1/3/2025 17:09:24'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed position or extent.
	Must layout submorphs again."

	scroller ifNotNil: [ | scrollerLocation doIt |
		doIt := false.
		scroller hasVariableExtent ifTrue: [
			theScrollerExtent = scroller extentInOwner ifFalse: [
				theScrollerExtent := scroller extentInOwner.
				doIt := true ]].
		scrollerLocation := scroller location.
		scrollerScale = scrollerLocation scale ifFalse: [
			scrollerScale := scrollerLocation scale.
			doIt := true ].
		scrollerRadians = scrollerLocation radians ifFalse: [
			scrollerRadians := scrollerLocation radians.
			doIt := true ].
		doIt ifTrue: [ self setScrollDeltas ]].! !


!HierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 1/3/2025 17:09:28'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed position or extent.
	Scrollbars are likely affected."

	self updateScrollBarsBounds! !


!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 1/3/2025 17:09:33'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed position or extent.
	Scrollbars are likely affected."

	self updateScrollBarsBounds! !


!InnerTextMorph methodsFor: 'notifications' stamp: 'jmv 1/3/2025 17:09:38'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed position or extent.
	Must layout submorphs again."

	textComposition ifNotNil: [ 
		textComposition composeAll.
		self fit.
		self selectionChanged ]! !

