'From Cuis7.5 [latest update: #7372] on 25 July 2025 at 9:54:58 am'!

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 09:48:16'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	adoptsWidgetsColor
		ifTrue: [ self color: (Theme current buttonColorFrom: color) ]
		ifFalse: [ self color: `Color transparent` ]! !

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 7/25/2025 09:52:40'!
minimumExtent
	"Ojo!!"
	^100@100! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 7/25/2025 09:50:41'!
layoutBounds
	"Return the bounds for laying out children of the receiver"

	^ self interiorOrigin extent: self interiorExtent ! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 7/25/2025 09:50:22'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		self layoutNeeded: false.
		^self ].

	"Invariant: morphExtent >=  minimumLayoutExtent"
	self adjustExtentFor: nil.

	layoutObject layoutMorphs: submorphs in: self layoutBounds.
	self layoutNeeded: false.! !


!LinearLayout methodsFor: 'services' stamp: 'jmv 7/25/2025 09:53:25'!
minimumExtentFor: morphs cachedValues: cachedValues
	self subclassResponsibility ! !

!methodRemoval: FormLayout #minimumExtentFor:cachedValues: stamp: 'jmv 7/25/2025 09:53:38'!
FormLayout removeSelector: #minimumExtentFor:cachedValues:!
!methodRemoval: Layout #minimumExtentFor:cachedValues: stamp: 'jmv 7/25/2025 09:53:40'!
Layout removeSelector: #minimumExtentFor:cachedValues:!
!methodRemoval: LinearLayoutMorph #adoptWidgetsColor: stamp: 'jmv 7/25/2025 09:48:16'!
LinearLayoutMorph removeSelector: #adoptWidgetsColor:!
!methodRemoval: LinearLayoutMorph #layoutSubmorphs stamp: 'jmv 7/25/2025 09:50:22'!
LinearLayoutMorph removeSelector: #layoutSubmorphs!
!methodRemoval: LinearLayoutMorph #layoutBounds stamp: 'jmv 7/25/2025 09:50:42'!
LinearLayoutMorph removeSelector: #layoutBounds!

!LayoutMorph reorganize!
('accessing' adoptWidgetsColor:)
('initialization' doAdoptWidgetsColor initialize)
('geometry' minimumExtent)
('layout' layoutBounds layoutSubmorphs)
!


!LinearLayout reorganize!
('accessing' axisEdgeWeight axisEdgeWeight: gap gap: gap:axisEdgeWeight:)
('testing' isLinearLayout)
('services' minimumExtentFor:cachedValues:)
!

