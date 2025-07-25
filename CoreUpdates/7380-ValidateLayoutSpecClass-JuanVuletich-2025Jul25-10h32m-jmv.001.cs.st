'From Cuis7.5 [latest update: #7372] on 25 July 2025 at 11:30:30 am'!

!FormLayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/25/2025 11:26:14'!
addMorph: aMorph layoutSpec: aLayoutSpec
	"Add a submorph, with aLayoutSpec"
	
	aLayoutSpec isLayoutEdgesSpec ifFalse: [ self error: 'Only LayoutEdgesSpec can be used in FormLayoutMorphs' ].
	super addMorph: aMorph layoutSpec: aLayoutSpec! !


!LinearLayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/25/2025 11:26:40'!
addMorph: aMorph layoutSpec: aLayoutSpec
	"Add a submorph, at the bottom or right, with aLayoutSpec"
	
	aLayoutSpec isLayoutSizeSpec ifFalse: [ self error: 'Only LayoutSizeSpec can be used in LinearLayoutMorphs' ].
	super addMorph: aMorph layoutSpec: aLayoutSpec! !


!Layout methodsFor: 'testing' stamp: 'jmv 7/25/2025 11:17:52'!
isFormLayout
	^false! !

!Layout methodsFor: 'testing' stamp: 'jmv 7/25/2025 11:17:47'!
isLinearLayout
	^false! !


!FormLayout methodsFor: 'testing' stamp: 'jmv 7/25/2025 11:18:55'!
isFormLayout
	^true! !


!LinearLayout methodsFor: 'testing' stamp: 'jmv 7/25/2025 11:19:07'!
isLinearLayout
	^true! !


!LayoutSpec methodsFor: 'testing' stamp: 'jmv 7/25/2025 11:25:43'!
isLayoutEdgesSpec
	^false! !

!LayoutSpec methodsFor: 'testing' stamp: 'jmv 7/25/2025 11:24:59'!
isLayoutSizeSpec
	^false! !


!LayoutEdgesSpec methodsFor: 'testing' stamp: 'jmv 7/25/2025 11:25:34'!
isLayoutEdgesSpec
	^true! !


!LayoutSizeSpec methodsFor: 'testing' stamp: 'jmv 7/25/2025 11:27:27'!
isLayoutSizeSpec
	^true! !


!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/25/2025 11:29:53'!
addMorph: aMorph layoutSpec: aLayoutSpec
	"Add a submorph, with aLayoutSpec.
	Subclasses validate that aLayoutSpec is appropriate for them before calling super."

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec: aLayoutSpec.
	self addMorphFront: aMorph.! !

!methodRemoval: LayoutSizeSpec #isLinearLayoutSpec stamp: 'jmv 7/25/2025 11:27:32'!
LayoutSizeSpec removeSelector: #isLinearLayoutSpec!
!methodRemoval: LayoutSpec #isLinearLayoutSpec stamp: 'jmv 7/25/2025 11:27:07'!
LayoutSpec removeSelector: #isLinearLayoutSpec!

!LayoutMorph reorganize!
('accessing' adoptWidgetsColor: borderWidth: padding:)
('initialization' defaultBorderWidth defaultColor defaultGap doAdoptWidgetsColor doClipSubmorphs initialize)
('drawing' drawOn:)
('layout' layoutBounds layoutSubmorphs layoutSubmorphsIfNeeded)
('geometry' extentChanged: someSubmorphPositionOrExtentChanged)
('submorphs-add/remove' addMorph:layoutSpec: addMorphBack: addMorphFront: addMorphFrontFromWorldPosition: removedMorph:)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex:position:)
('testing' is:)
('geometry testing' clipsSubmorphs)
('layout in owner' layoutSpec)
!


!FormLayoutMorph reorganize!
('initialization' initialize)
('geometry' minimumExtent)
('submorphs-add/remove' addMorph:layoutSpec:)
!


!LinearLayoutMorph reorganize!
('accessing' axisEdgeWeight axisEdgeWeight: gap gap: separation:)
('initialization' beColumn beRow initializeColumn initializeRow)
('adjust' addAdjusterMorph adjustBy:at:)
('convenience methods' addAdjusterAndMorph:fixedHeight: addAdjusterAndMorph:layoutSpec: addAdjusterAndMorph:proportionalHeight: addAdjusterAndMorph:proportionalWidth: addMorph:fixedHeight: addMorph:fixedWidth: addMorph:proportionalHeight: addMorph:proportionalWidth: addMorphKeepMorphHeight: addMorphUseAll: addMorphs: addMorphs:widthProportionalTo:)
('geometry' minimumExtent)
('submorphs-add/remove' addMorph:layoutSpec:)
('testing' isColumn isRow)
!


!Layout reorganize!
('testing' isColumn isFormLayout isLinearLayout isRow)
!


!FormLayout reorganize!
('layout' layoutMorphs:in:)
('testing' isFormLayout)
!


!LinearLayout reorganize!
('accessing' axisEdgeWeight axisEdgeWeight: gap gap: gap:axisEdgeWeight:)
('services' minimumExtentFor:cachedValues:)
('testing' isLinearLayout)
!


!LayoutSpec reorganize!
('accessing' morph morph:)
('testing' isLayoutEdgesSpec isLayoutSizeSpec)
('private' updateContainingLayout)
!


!LayoutEdgesSpec reorganize!
('accessing' bottomRightEdgesWeight bottomRightEdgesWeight: bottomRightOffset bottomRightOffset: topLeftEdgesWeight topLeftEdgesWeight: topLeftOffset topLeftOffset:)
('layout' minimumSpecExtent)
('testing' isLayoutEdgesSpec)
!

