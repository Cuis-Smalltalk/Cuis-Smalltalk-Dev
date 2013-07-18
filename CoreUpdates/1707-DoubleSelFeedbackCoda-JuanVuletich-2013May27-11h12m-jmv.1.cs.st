'From Cuis 4.1 of 12 December 2012 [latest update: #1706] on 27 May 2013 at 11:13:06.155166 am'!

!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 5/27/2013 11:13'!
drawOn: aCanvas

	| tRect sRect colorToUse sLeft aForm centeringOffset |
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
	
	complexContents hasContents ifTrue: [
		tRect _ self toggleRectangle.
		aForm _ isExpanded 
			ifTrue: [ container expandedForm ]
			ifFalse: [ container notExpandedForm ].
		centeringOffset _ ((tRect height - aForm extent y) / 2.0) rounded.
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
	sRect _ sLeft@0 extent: extent - (sLeft@0).
	colorToUse _ complexContents preferredColor ifNil: [ color ].

	aCanvas
		drawString: contents asString
		in: sRect
		font: self fontToUse
		color: colorToUse! !

