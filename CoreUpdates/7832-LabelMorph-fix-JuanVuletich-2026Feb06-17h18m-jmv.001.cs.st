'From Cuis7.7 [latest update: #7831] on 6 February 2026 at 5:19:01 pm'!

!LabelMorph methodsFor: 'drawing' stamp: 'jmv 6/Feb/2026 16:05:21'!
drawOn: aCanvas
	| f delta |
	contents ifNotNil: [
		f := self fontToUse.
		delta := contents isEmpty
			ifFalse: [ (f boundsLeftOf: contents first) negated@0 ]
			ifTrue: [ 0@0 ].
		aCanvas
			drawString: contents
			at: self interiorOrigin + delta
			font: f
			color: color ].
	(borderWidth > 0) ifTrue: [
		aCanvas
			frameRectangle: (`0@0` extent: extent)
			color: borderColor
			borderWidth: borderWidth
			borderStyleSymbol: #simple ].! !

