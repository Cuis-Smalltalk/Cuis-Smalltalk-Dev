'From Cuis 4.2 of 25 July 2013 [latest update: #2734] on 14 April 2016 at 3:14:21.510211 pm'!

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 4/14/2016 15:09'!
downButtonPosition
	^0@0 + extent - ScrollBar scrollbarThickness! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 4/14/2016 15:09'!
upButtonPosition
	^extent x - ScrollBar scrollbarThickness@0! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 4/14/2016 15:10'!
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
			image: (BitBltCanvas arrowOfDirection: #up size: scrollbarThickness)
			at: self upButtonPosition.
		aCanvas
			image: (BitBltCanvas arrowOfDirection: #down size: scrollbarThickness)
			at: self downButtonPosition.
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

!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 4/14/2016 15:13'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	(self morphContainsPoint: localEventPosition)
		ifTrue: [
			((self upButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; moveUp ].
			((self downButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; moveDown ].
			self selected: (localEventPosition y // self class itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !

