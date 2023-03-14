'From Cuis 6.0 [latest update: #5688] on 14 March 2023 at 2:55:28 pm'!

!IndentingListItemMorph methodsFor: 'drawing' stamp: 'HAW 3/14/2023 14:53:08'!
drawOn: aCanvas

	| x colorToUse centeringOffset |
	isHighlighted ifTrue: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: ((Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) alpha: 0.3) ].

	isSelected ifTrue: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].

	x := 12 * indentLevel.

	complexContents hasContents ifTrue: [
		isExpanded
			ifTrue: [ aCanvas drawExpandedAt: x@(extent y//2)]
			ifFalse: [ aCanvas drawNotExpandedAt: x@(extent y//2) ]].
	x := x + 1 + FontFamily defaultPointSize.

	icon isNil ifFalse: [
		centeringOffset := ((extent y - icon height) / 2.0) roundedHAFZ.
		 aCanvas 
			image: icon
			at:  (x @ centeringOffset).
		x := x + 20 ].

	colorToUse := complexContents preferredColor ifNil: [ color ].
	aCanvas
		drawString: contents asString
		at: x@0
		font: self fontToUse
		color: colorToUse! !

!IndentingListItemMorph methodsFor: 'private' stamp: 'HAW 3/14/2023 14:53:35'!
toggleRectangle

	| width |
	
	width := FontFamily defaultPointSize - 5.
	
	^(width*indentLevel @ 0) extent: width@extent y! !


!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'HAW 3/14/2023 14:53:51'!
drawExpandedAt: aPoint
	"For Tree View (IndentingListItemMorph)"

	| f |
	f := BitBltCanvas arrowOfDirection: #down size: FontFamily defaultPointSize.
	self 
		image: f 
		at: aPoint x-3 @ (aPoint y - (f height // 2)).! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'HAW 3/14/2023 14:54:04'!
drawNotExpandedAt: aPoint
	"For Tree View (IndentingListItemMorph)"

	| f |
	f := BitBltCanvas arrowOfDirection: #right size: FontFamily defaultPointSize.
	self 
		image: f 
		at: aPoint x-1 @ (aPoint y - (f height // 2)).
		! !

