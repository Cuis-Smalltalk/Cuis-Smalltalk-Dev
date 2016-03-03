'From Cuis 4.2 of 25 July 2013 [latest update: #2700] on 3 March 2016 at 9:47:53.700704 am'!

!LayoutMorph methodsFor: 'layout in owner' stamp: 'jmv 3/3/2016 09:43'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec _ LayoutSpec useAll.
	layoutSpec morph: self.

	^ layoutSpec ! !


!LayoutMorph reorganize!
('accessing' adoptWidgetsColor: direction padding: separation: xSeparation ySeparation)
('adjust' adjustBy:at: adjustHorizontallyBy:at: adjustVerticallyBy:at:)
('convenience methods' addAdjusterAndMorph:fixedHeight: addAdjusterAndMorph:layoutSpec: addAdjusterAndMorph:proportionalHeight: addAdjusterAndMorph:proportionalWidth: addMorph:fixedHeight: addMorph:fixedWidth: addMorph:proportionalHeight: addMorph:proportionalWidth: addMorphKeepMorphHeight: addMorphUseAll: addMorphs: addMorphs:widthProportionalTo:)
('geometry' calculateMinimumExtent fontPreferenceChanged minimumExtent)
('initialization' beColumn beRow defaultColor doAdoptWidgetsColor initialize)
('layout' layoutBounds layoutSubmorphs layoutSubmorphsHorizontallyIn: layoutSubmorphsVerticallyIn: submorphsToLayout)
('submorphs-add/remove' addAdjusterMorph addMorph: addMorph:layoutSpec:)
('testing' is:)
('private' minPaneHeightForReframe minPaneWidthForReframe proportionalHeightNormalizationFactor proportionalWidthNormalizationFactor)
('layout in owner' layoutSpec)
!

