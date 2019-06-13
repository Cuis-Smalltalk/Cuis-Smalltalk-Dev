'From Cuis 5.0 of 7 November 2016 [latest update: #3771] on 26 May 2019 at 7:38:33 am'!

!IndentingListItemMorph methodsFor: 'events' stamp: 'len 5/26/2019 05:08:15'!
mouseEnter: event
	isHighlighted _ true.
	self redrawNeeded.
	^super mouseEnter: event! !

!IndentingListItemMorph methodsFor: 'events' stamp: 'len 5/26/2019 05:08:59'!
mouseLeave: event
	isHighlighted _ false.
	self redrawNeeded.
	^super mouseEnter: event! !

!IndentingListItemMorph methodsFor: 'event handling testing' stamp: 'len 5/26/2019 05:12:45'!
handlesMouseOver: event
	^ true! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'len 5/26/2019 07:35:54'!
drawOn: aCanvas

	| tRect colorToUse sLeft aForm centeringOffset |
	isHighlighted ifTrue: [
		aCanvas
			fillRectangle: (`0@0` extent: extent)
			color: ((Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) alpha: 0.3) ].
	isSelected ifTrue: [
		aCanvas
			fillRectangle: (`0@0` extent: extent)
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


!InnerListMorph methodsFor: 'drawing' stamp: 'len 5/26/2019 07:34:30'!
drawHighlightOn: aCanvas
"Ademas, poner el mouse pointer de relojito si tarda... Detectarlo automaticamente, etc. Aunque no estoy seguro de como hacerlo... quizas colgar un cachito de codigo en un proceso de mayor prioridad, onda 'si pasa 1 segundo, y todavia no te resetee este flag, entonces pone el relojito'"
	| selectionDrawBounds |
	highlightedRow  ifNil: [ ^self ].
	highlightedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: highlightedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: (`0@0` extent: extent).
	aCanvas
		fillRectangle: selectionDrawBounds
		color: ((Theme current listHighlightFocused: owner hasKeyboardFocus) alpha: 0.3)! !

