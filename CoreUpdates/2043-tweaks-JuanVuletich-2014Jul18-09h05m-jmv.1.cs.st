'From Cuis 4.2 of 25 July 2013 [latest update: #2042] on 18 July 2014 at 9:33:16.012342 am'!

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 7/18/2014 09:30'!
drawLinesOn: aCanvas 
	| lColor bottomY topY tx clipRect |
	lColor _ Theme current line.
	tx _ aCanvas currentTransformation.
	clipRect _ aCanvas clipRect.
	topY _ (tx internalizePosition: clipRect topLeft) y min: (tx internalizePosition: clipRect topRight) y.
	bottomY _ (tx internalizePosition: clipRect bottomLeft) y max: (tx internalizePosition: clipRect bottomRight) y.
	self submorphs do: [ :submorph | 
		(submorph isExpanded or: [
			(submorph morphPosition y between: topY and: bottomY) or: [
				submorph nextSibling notNil and: [
					submorph nextSibling morphPosition y between: topY and: bottomY ] ] ])
		ifTrue: [
			self
				drawLinesFor: submorph
				on: aCanvas
				lineColor: lColor ]]
	! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 7/18/2014 09:30'!
bottomVisibleRowForCanvas: aCanvas
	"return the bottom visible row in aCanvas's clip rectangle"
	| tx clipRect |
	tx _ aCanvas currentTransformation.
	clipRect _ aCanvas clipRect.
	^ (self rowAtLocation: (tx internalizePosition: clipRect bottomLeft))
		max: (self rowAtLocation: (tx internalizePosition: clipRect bottomRight))! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 7/18/2014 09:30'!
topVisibleRowForCanvas: aCanvas
	"return the top visible row in aCanvas's clip rectangle"
	| tx clipRect |
	tx _ aCanvas currentTransformation.
	clipRect _ aCanvas clipRect.
	^ (self rowAtLocation: (tx internalizePosition: clipRect topLeft))
		min: (self rowAtLocation: (tx internalizePosition: clipRect topRight))! !

