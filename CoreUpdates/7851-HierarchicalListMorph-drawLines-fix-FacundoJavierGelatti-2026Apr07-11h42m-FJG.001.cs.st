'From Cuis7.7 [latest update: #7850] on 7 April 2026 at 11:43:48 am'!

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'FJG 2/Apr/2026 19:15:49'!
drawLinesOn: aCanvas
	| lColor bottomY topY tx clipRect |
	lColor := Theme current line.
	tx := aCanvas currentTransformation.
	clipRect := aCanvas clipRect.
	topY := (tx internalizePosition: clipRect topLeft) y min: (tx internalizePosition: clipRect topRight) y.
	bottomY := (tx internalizePosition: clipRect bottomLeft) y max: (tx internalizePosition: clipRect bottomRight) y.
	self submorphs do: [ :submorph | 
		(submorph isExpanded or: [
			(submorph morphPosition y between: topY - submorph morphHeight and: bottomY) or: [
				submorph nextSibling notNil and: [
					submorph nextSibling morphPosition y between: topY - submorph nextSibling morphHeight and: bottomY ]]
		]) ifTrue: [
			self 
				drawLinesFor: submorph
				on: aCanvas
				lineColor: lColor
		]
	]
	! !

