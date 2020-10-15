'From Cuis 5.0 [latest update: #4410] on 15 October 2020 at 9:10:32 am'!
Smalltalk renameClassNamed: #RectangleLikeMorph as: #KernelMorph!

!WidgetMorph commentStamp: '<historical>' prior: 0!
Hierarchy for morphs that are rectangle like, and that occupy an area that can be specified as a Rectangle. This means that, besides being of rectangular shape, sides are aligned with local coordinate axes. Including rectangles with rounded corners and such. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape. Instances may have a border, see instanceVariables borderWidth and borderColor. Subclasses can use a variety of border styles: simple, inset, raised
Subclasses can add things like 'roundedCornerRadious' or such.!


!KernelMorph methodsFor: 'geometry testing' stamp: 'jmv 10/15/2020 09:09:42'!
requiresVectorCanvas
	"Kernel morphs can run with the older BitBltCanvas"

	^false! !

!methodRemoval: KernelMorph #layoutSpec: stamp: 'jmv 10/15/2020 09:08:40'!
KernelMorph removeSelector: #layoutSpec:!

!KernelMorph reorganize!
('accessing' color color:)
('initialization' defaultColor initialize)
('drawing' drawOn:)
('geometry' morphBounds: morphExtent morphExtent: morphExtentInWorld: morphHeight morphHeight: morphPosition:extent: morphTopLeft morphWidth morphWidth: privateExtent:)
('geometry testing' isOrthoRectangularMorph morphContainsPoint: requiresVectorCanvas submorphsMightProtrude)
!

!classRemoval: #EllipseMorph stamp: 'jmv 10/15/2020 09:03:51'!
Smalltalk removeClassNamed: #EllipseMorph!

