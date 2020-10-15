'From Cuis 5.0 [latest update: #4396] on 12 October 2020 at 9:09:20 pm'!

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 21:09:06'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	"If you redefine this method in a subclass, please take a look at the comment at #isOpaqueMorph"
	aCanvas
		fillRectangle: self morphLocalBounds
		color: color
		borderWidth: 0
		borderStyleSymbol: #simple
		baseColorForBorder: Color white! !


!LayoutMorph reorganize!
('accessing' addMorphFrontFromWorldPosition: adoptWidgetsColor: axisEdgeWeight axisEdgeWeight: direction direction: separation: xSeparation ySeparation)
('adjust' adjustBy:at: adjustHorizontallyBy:at: adjustVerticallyBy:at:)
('convenience methods' addAdjusterAndMorph:fixedHeight: addAdjusterAndMorph:layoutSpec: addAdjusterAndMorph:proportionalHeight: addAdjusterAndMorph:proportionalWidth: addMorph:fixedHeight: addMorph:fixedWidth: addMorph:proportionalHeight: addMorph:proportionalWidth: addMorphKeepMorphHeight: addMorphUseAll: addMorphs: addMorphs:widthProportionalTo:)
('geometry' desiredLayoutHeight minimumExtent)
('initialization' beColumn beRow defaultColor doAdoptWidgetsColor drawOn: initialize)
('layout' heightsFor:within: layoutBounds layoutSubmorphs layoutSubmorphsHorizontallyIn: layoutSubmorphsVerticallyIn: offHorizontalMetricFor:withinExtent: offVerticalMetricFor:withinExtent: refreshExtent submorphsToLayout widthsFor:within:)
('submorphs-add/remove' addAdjusterMorph addMorph: addMorph:layoutSpec: addMorphFront: removedMorph:)
('testing' is: isColumn isRow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position:)
('layout in owner' layoutSpec)
!

