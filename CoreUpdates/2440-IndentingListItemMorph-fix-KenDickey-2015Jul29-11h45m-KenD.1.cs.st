'From Cuis 4.2 of 25 July 2013 [latest update: #2439] on 29 July 2015 at 11:45:29.15323 am'!

!IndentingListItemMorph methodsFor: 'drawing' stamp: 'KenD 7/13/2015 14:44'!
drawOn: aCanvas

	| tRect colorToUse sLeft aForm centeringOffset |
	isHighlighted ifTrue: [
		aCanvas
			fillRectangle: (0@0 extent: extent)
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) duller ].
	isSelected ifTrue: [
		aCanvas
			fillRectangle: (0@0 extent: extent)
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].
			
	tRect _ self toggleRectangle.
	aForm _ isExpanded 
		ifTrue: [ container expandedForm ]
		ifFalse: [ container notExpandedForm ].
	centeringOffset _ ((tRect height - aForm extent y) / 2.0) rounded.

	complexContents hasContents ifTrue: [
		aCanvas 
			image: aForm 
			at: (tRect topLeft translatedBy: 0 @ centeringOffset) ].

	icon isNil
	ifFalse: [
		 aCanvas 
			image: icon
			at:  (tRect topLeft translatedBy:  icon width  @ centeringOffset).
		sLeft _ indentLevel * 12 + 16 + (icon width + 2).
	]
	ifTrue: [
		sLeft _ indentLevel * 12 + 16.
	].
	colorToUse _ complexContents preferredColor ifNil: [ color ].

	aCanvas
		drawString: contents asString
		at: sLeft@0
		font: self fontToUse
		color: colorToUse! !

