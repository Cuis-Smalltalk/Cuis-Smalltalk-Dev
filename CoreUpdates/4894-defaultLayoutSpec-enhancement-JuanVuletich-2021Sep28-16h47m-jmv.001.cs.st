'From Cuis 5.0 [latest update: #4893] on 28 September 2021 at 4:49:34 pm'!

!KernelMorph methodsFor: 'layout-properties' stamp: 'jmv 9/28/2021 16:47:56'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec := LayoutSpec keepMorphExtent.
	layoutSpec morph: self.

	^ layoutSpec! !


!WidgetMorph methodsFor: 'layout-properties' stamp: 'jmv 9/28/2021 16:48:09'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec := LayoutSpec keepMorphExtent.
	layoutSpec morph: self.

	^ layoutSpec! !


!MovableMorph methodsFor: 'layout-properties' stamp: 'jmv 9/28/2021 16:48:18'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec _ LayoutSpec useAll.
	layoutSpec morph: self.

	^ layoutSpec ! !

!methodRemoval: LayoutMorph #layoutSpec stamp: 'jmv 9/28/2021 16:48:21'!
LayoutMorph removeSelector: #layoutSpec!

!KernelMorph reorganize!
('accessing' color color:)
('initialization' defaultColor initialize)
('drawing' drawOn: imageForm:depth:)
('geometry' displayBoundsUpdateFrom: findFullBoundsInOwner fitInto: morphExtent morphExtent: morphExtentInOwner morphExtentInOwner: morphExtentInWorld morphExtentInWorld: morphHeight morphHeight: morphHeightInOwner: morphLocalBounds morphLocalBoundsForError morphPosition:extent: morphWidth morphWidth: morphWidthInOwner: privateExtent: rotationCenter)
('geometry services' coversLocalPoint: coversPixel: fullOwnsOrCoversPixel: ownsOrCoversPixel:)
('geometry testing' knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('testing' is:)
('layout-properties' layoutSpec)
!

