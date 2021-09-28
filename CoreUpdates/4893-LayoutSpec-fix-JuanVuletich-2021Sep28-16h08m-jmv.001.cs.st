'From Cuis 5.0 [latest update: #4892] on 28 September 2021 at 4:26:44 pm'!

!MovableMorph methodsFor: 'layout-properties' stamp: 'jmv 9/28/2021 16:08:01'!
layoutSpec: aLayoutSpec
	"Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout"
	self layoutSpec == aLayoutSpec ifTrue: [ ^self ].
	aLayoutSpec morph: self.
	layoutSpec := aLayoutSpec.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!methodRemoval: WidgetMorph #layoutSpec: stamp: 'jmv 9/28/2021 16:08:05'!
WidgetMorph removeSelector: #layoutSpec:!
