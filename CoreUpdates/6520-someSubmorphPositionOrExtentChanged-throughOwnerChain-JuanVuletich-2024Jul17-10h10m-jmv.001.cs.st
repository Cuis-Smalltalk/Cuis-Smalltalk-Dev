'From Cuis7.1 [latest update: #6519] on 17 July 2024 at 10:30:09 am'!

!Morph methodsFor: 'layout' stamp: 'jmv 7/17/2024 10:08:48'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed at any nested level.
	Must layout submorphs again."

	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self layoutNeeded: true.! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 7/17/2024 10:08:16'!
model: aTextModel wrappedTo: width
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	| newExtent oldExtent |
	wrapFlag := true.
	model := aTextModel.
	newExtent := width truncated@extent y.
	extent = newExtent ifFalse: [
		oldExtent := extent.
		extent := newExtent.
		self extentChanged: oldExtent.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].
	self model: aTextModel! !

!InnerTextMorph methodsFor: 'notifications' stamp: 'jmv 7/17/2024 10:09:01'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed at any nested level.
	Must layout submorphs again."

	super someSubmorphPositionOrExtentChanged.
	textComposition ifNotNil: [ 
		textComposition composeAll.
		self fit.
		self selectionChanged ]! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 7/17/2024 10:09:04'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed at any nested level.
	Must layout submorphs again."

	super someSubmorphPositionOrExtentChanged.
	scroller ifNotNil: [ | scrollerLocation doIt |
		doIt := false.
		scroller hasVariableExtent ifTrue: [
			theScrollerExtent = scroller morphExtentInOwner ifFalse: [
				theScrollerExtent := scroller morphExtentInOwner.
				doIt := true ]].
		scrollerLocation := scroller location.
		scrollerScale = scrollerLocation scale ifFalse: [
			scrollerScale := scrollerLocation scale.
			doIt := true ].
		scrollerRadians = scrollerLocation radians ifFalse: [
			scrollerRadians := scrollerLocation radians.
			doIt := true ].
		doIt ifTrue: [ self setScrollDeltas ]].! !


!HierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 7/17/2024 10:29:58'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self layoutNeeded: true.
	self updateScrollBarsBounds! !


!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 7/17/2024 10:30:01'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self layoutNeeded: true.
	self updateScrollBarsBounds! !

