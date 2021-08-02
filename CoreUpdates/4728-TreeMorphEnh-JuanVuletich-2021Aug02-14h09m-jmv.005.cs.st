'From Cuis 5.0 [latest update: #4727] on 2 August 2021 at 2:53:12 pm'!

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/2/2021 14:21:31'!
drawExpandedAt: aPoint
	"For Tree View (IndentingListItemMorph)"

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/2/2021 14:21:53'!
drawNotExpandedAt: aPoint
	"For Tree View (IndentingListItemMorph)"

	self subclassResponsibility.! !


!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/2/2021 14:50:23'!
drawExpandedAt: aPoint
	"For Tree View (IndentingListItemMorph)"

	| f |
	f _ BitBltCanvas arrowOfDirection: #down size: 17.
	self 
		image: f 
		at: aPoint x-3 @ (aPoint y - (f height // 2)).! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/2/2021 14:50:32'!
drawNotExpandedAt: aPoint
	"For Tree View (IndentingListItemMorph)"

	| f |
	f _ BitBltCanvas arrowOfDirection: #right size: 17.
	self 
		image: f 
		at: aPoint x-1 @ (aPoint y - (f height // 2)).! !


!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 8/2/2021 14:45:19'!
drawLineToggleToTextFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
	"If I am not the only item in my container, draw the line between:
		- my left edge
		- and my text left edge"

	| aMorphCenter hLineY hLineLeft rect right |
	anIndentingListItemMorph isSoleItem ifTrue: [ ^ self ].
	hasToggle ifFalse: [
		rect _ anIndentingListItemMorph toggleRectangle.
		aMorphCenter _ anIndentingListItemMorph externalize: rect center.
		right _ (anIndentingListItemMorph externalize: rect rightCenter) x.
		hLineY _ aMorphCenter y.
		hLineLeft _ aMorphCenter x - 1.
		aCanvas
			line: hLineLeft @ hLineY
			to: right @ hLineY
			width: 1
			color: lineColor ]! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 8/2/2021 14:18:46'!
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

	x _ 12 * indentLevel.

	complexContents hasContents ifTrue: [
		isExpanded
			ifTrue: [ aCanvas drawExpandedAt: x@(extent y//2)]
			ifFalse: [ aCanvas drawNotExpandedAt: x@(extent y//2) ]].
	x _ x + 18.

	icon isNil ifFalse: [
		centeringOffset _ ((extent y - icon height) / 2.0) rounded.
		 aCanvas 
			image: icon
			at:  (x @ centeringOffset).
		x _ x + 20 ].

	colorToUse _ complexContents preferredColor ifNil: [ color ].
	aCanvas
		drawString: contents asString
		at: x@0
		font: self fontToUse
		color: colorToUse! !

!methodRemoval: HierarchicalListMorph #notExpandedForm stamp: 'jmv 8/2/2021 14:19:50'!
HierarchicalListMorph removeSelector: #notExpandedForm!
!methodRemoval: HierarchicalListMorph #expandedForm stamp: 'jmv 8/2/2021 14:19:54'!
HierarchicalListMorph removeSelector: #expandedForm!
