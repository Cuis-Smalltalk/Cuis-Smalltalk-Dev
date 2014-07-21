'From Cuis 4.2 of 25 July 2013 [latest update: #2033] on 17 July 2014 at 5:30:29.974499 pm'!

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 7/10/2014 22:46'!
drawStringEmbossed: s at: aPoint font: fontOrNil color: c
	^self drawStringEmbossed: s from: 1 to: s size at: aPoint font: fontOrNil color: c! !


!InnerListMorph methodsFor: 'list management' stamp: 'jmv 7/10/2014 22:39'!
drawYForRow: row
	"calculate the vertical position that row should be drawn at.  This might be outside our bounds!!"
	^ row - 1 * font height! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:35'!
drawOn: aCanvas
	| rectangle w y0 h y1 y2 scrollbarThickness |
	aCanvas frameAndFillRectangle: (0@0 extent: extent) fillColor: self color borderWidth: borderWidth borderColor: borderColor.
	y0 _ 1.
	w _ extent x-2.
	scrollbarThickness _ ScrollBar scrollbarThickness.
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ w - scrollbarThickness.
		aCanvas
			frameRectangle: (extent x - scrollbarThickness@0
				extent: scrollbarThickness @ extent y)
			borderWidth: 1
			color: borderColor.
		aCanvas
			image: (FormCanvas arrowOfDirection: #up size: scrollbarThickness)
			at: extent x - scrollbarThickness@0.
		aCanvas
			image: (FormCanvas arrowOfDirection: #down size: scrollbarThickness)
			at: 0@0 + extent - scrollbarThickness.
		h _ extent y - (2 * scrollbarThickness).
		y1 _ (1.0 * self firstVisible-1 / completer entryCount * h) ceiling + y0 + scrollbarThickness-1.
		y2 _ (1.0 * self lastVisible / completer entryCount * h) floor + y0 + scrollbarThickness -1.
		aCanvas
			fillRectangle: (extent x - scrollbarThickness+2@y1 corner:  extent x-2 @ y2)
			color: Color veryLightGray ].
	self firstVisible
		to: self lastVisible
		do: [ :index |
			rectangle _ 1@y0 extent: w@self class itemHeight.
			index = self selected
				ifTrue: [
					aCanvas fillRectangle: rectangle color: (Theme current listHighlightFocused: true) ].
			aCanvas
				drawString: (completer entries at: index) asString
				at: rectangle topLeft
				font: self class listFont
				color: Theme current text.
			y0 _ y0 + self itemHeight ]! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:50'!
drawEmbossedLabelOn: aCanvas

	| availableW center colorForLabel f l labelMargin targetSize w x y |
	label ifNotNil: [
		colorForLabel _ Theme current buttonLabel.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel _ colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel _ colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f _ self fontToUse.
		center _ extent // 2.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin.
		availableW >= w
			ifTrue: [
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				targetSize _ label size * availableW // w.
				l _ label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize _ targetSize - 1.
					l _ label squeezedTo: targetSize ]].
		
		w _ f widthOfString: l.
		x _ center x - (w // 2).
		y _ center y - (f height // 2).
		aCanvas
			drawStringEmbossed: l
			at: x@y
			font: f
			color: colorForLabel ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:43'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin |

	f _ self fontToUse.
	center _ extent // 2.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin - 1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f height // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: Theme current buttonLabel ]! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 7/11/2014 10:36'!
drawLabelOn: aCanvas

	| e x0 y0 f w availableW l |
	e _ self boxExtent.
	x0 _  e x * 4 + 14.
	y0 _ 2.
	f _ Preferences windowTitleFont.
	w _ f widthOfString: labelString.
	availableW _ extent x - x0 - 10.
	availableW >= w
		ifTrue: [
			l _ labelString ]
		ifFalse: [
			l _ labelString squeezedTo: (labelString size * availableW / w) rounded ].
	Theme current embossedTitles
		ifFalse: [
			aCanvas
				drawString: l
				at: x0@y0
				font: f
				color: Theme current windowLabel ]
		ifTrue: [
			aCanvas
				drawStringEmbossed: l
				at: x0@y0
				font: f
				color: Theme current windowLabel ]! !


!InnerListMorph methodsFor: 'list management' stamp: 'jmv 7/10/2014 22:39'!
drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!!"

	self flag: #jmvVer2.
	"revisar senders"
	^ 0 @ (self drawYForRow: row) extent: extent x @ font height! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:40'!
draw: item atRow: row on: canvas
	"display the given item at row row"
	| f |
	f _ (item is: #Text)
		ifTrue: [ font emphasized: (item emphasisAt: 1) ]
		ifFalse: [ font ].
	canvas
		drawString: item
		at: 0 @ (self drawYForRow: row)
		font: f
		color: (self colorForRow: row)! !


!MinimalStringMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:41'!
drawOn: aCanvas

	aCanvas drawString: contents at: 0@0 font: self fontToUse color: color! !


!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:42'!
drawOn: aCanvas
	self hasSelection ifTrue: [ self drawSelectionOn: aCanvas ].
	self hasTextCursor ifTrue: [ self drawTextCursorOn: aCanvas ].
	aCanvas
		drawString: contents
		at: 0@0
		font: self fontToUse
		color: color! !


!StringMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:45'!
drawOn: aCanvas
	aCanvas
		drawString: contents
		at: 0@0
		font: self fontToUse
		color: color! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:36'!
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
	colorToUse _ complexContents preferredColor ifNil: [ color ].

	aCanvas
		drawString: contents asString
		at: sLeft@0
		font: self fontToUse
		color: colorToUse! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:41'!
drawOn: aCanvas 
	| stringColor leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: (0@0 extent: extent) color: Theme current menuHighlight].
	leftEdge _ 0.

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + submorphs first morphWidth + 8 ].

	self hasIcon
		ifTrue: [| iconForm | 
			iconForm _ isEnabled ifTrue: [ self icon ] ifFalse: [ self icon asGrayScale ].
			aCanvas image: iconForm at: leftEdge+1 @ (extent y - iconForm height // 2).
			leftEdge _ leftEdge + iconForm width + self iconSeparation].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: SubMenuMarker
			at: extent x - 8 @ (extent y - SubMenuMarker height // 2) ]! !

!methodRemoval: SystemWindow #labelRectangle!
SystemWindow removeSelector: #labelRectangle!
!methodRemoval: SystemWindow #labelRectangleForEmbossed!
SystemWindow removeSelector: #labelRectangleForEmbossed!
!methodRemoval: FormCanvas #drawString:from:to:in:font:color:!
FormCanvas removeSelector: #drawString:from:to:in:font:color:!
!methodRemoval: FormCanvas #drawString:in:font:color:!
FormCanvas removeSelector: #drawString:in:font:color:!
!methodRemoval: FormCanvas #drawStringEmbossed:from:to:in:font:color:!
FormCanvas removeSelector: #drawStringEmbossed:from:to:in:font:color:!
!methodRemoval: FormCanvas #drawStringEmbossed:in:font:color:!
FormCanvas removeSelector: #drawStringEmbossed:in:font:color:!
