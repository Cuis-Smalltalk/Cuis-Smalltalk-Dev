'From Cuis7.1 [latest update: #6693] on 11 September 2024 at 9:20:36 am'!
!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 09:20:26'!
separation: aNumberOrPoint

	gap = aNumberOrPoint ifFalse: [
		self adjustExtentFor: [ gap := aNumberOrPoint ]].
	self layoutSubmorphs ! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 09:20:26'!
xSeparation
	^gap isNumber
		ifTrue: [ gap ]
		ifFalse: [ gap x ]! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 09:20:26'!
ySeparation
	^gap isNumber
		ifTrue: [ gap ]
		ifFalse: [ gap y ]! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 9/11/2024 09:20:26'!
initialize
	super initialize.
	gap := 0.
	axisEdgeWeight := 0.0.
	doAdoptWidgetsColor := false.! !

!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
BorderedBoxMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction gap axisEdgeWeight doAdoptWidgetsColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutMorph reorganize!
('accessing' addMorphFrontFromWorldPosition: adoptWidgetsColor: axisEdgeWeight axisEdgeWeight: borderWidth: direction direction: padding: separation: xSeparation ySeparation)
('adjust' adjustBy:at: adjustHorizontallyBy:at: adjustVerticallyBy:at:)
('convenience methods' addAdjusterAndMorph:fixedHeight: addAdjusterAndMorph:layoutSpec: addAdjusterAndMorph:proportionalHeight: addAdjusterAndMorph:proportionalWidth: addMorph:fixedHeight: addMorph:fixedWidth: addMorph:proportionalHeight: addMorph:proportionalWidth: addMorphKeepMorphHeight: addMorphUseAll: addMorphs: addMorphs:widthProportionalTo:)
('geometry' desiredLayoutHeight minimumExtent minimumExtent:)
('initialization' beColumn beRow defaultBorderWidth defaultColor defaultPadding doAdoptWidgetsColor doClipSubmorphs initialize)
('layout' heightsFor:within:minLayoutExtentCache: layoutBounds layoutSubmorphs layoutSubmorphsHorizontallyIn: layoutSubmorphsVerticallyIn: offAxisHeightFor:within:minLayoutExtentCache: offAxisWidthFor:within:minLayoutExtentCache: offAxisXOffsetFor:within: offAxisYOffsetFor:within: submorphsToLayout widthsFor:within:minLayoutExtentCache:)
('submorphs-add/remove' addAdjusterMorph addMorph:layoutSpec: addMorphBack: addMorphFront: removedMorph:)
('testing' is: isColumn isRow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex:position:)
('layout in owner' layoutSpec)
('geometry testing' clipsSubmorphs)
('drawing' drawOn:)
!

