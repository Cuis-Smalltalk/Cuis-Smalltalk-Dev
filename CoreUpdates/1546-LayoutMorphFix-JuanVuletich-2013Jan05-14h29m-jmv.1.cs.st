'From Cuis 4.1 of 12 December 2012 [latest update: #1544] on 5 January 2013 at 2:43:28 pm'!

!LayoutMorph methodsFor: 'testing' stamp: 'jmv 1/5/2013 14:43'!
clipsSubmorphs
	"Drawing specific. If this property is set, clip the receiver's  
	submorphs to the receiver's clipping bounds.
	Maybe move up to RectangleLikeMorph"
	^ true! !


!LayoutMorph reorganize!
('initialization' beColumn beRow defaultColor initialize)
('accessing' direction padding: separation: xSeparation ySeparation)
('layout' layoutSubmorphs layoutSubmorphsHorizontallyIn: layoutSubmorphsVerticallyIn:)
('submorphs-add/remove' addAdjusterMorph addMorph:layoutSpec:)
('private' minPaneHeightForReframe minPaneWidthForReframe proportionalHeightNormalizationFactor proportionalWidthNormalizationFactor)
('adjust' adjustBy:at: adjustHorizontallyBy:at: adjustVerticallyBy:at:)
('convenience methods' addAdjusterAndMorph:fixedHeight: addAdjusterAndMorph:layoutSpec: addAdjusterAndMorph:proportionalHeight: addAdjusterAndMorph:proportionalWidth: addMorph:fixedHeight: addMorph:fixedWidth: addMorph:proportionalHeight: addMorph:proportionalWidth: addMorphUseAll: addMorphs: addMorphs:widthProportionalTo:)
('testing' clipsSubmorphs)
!

