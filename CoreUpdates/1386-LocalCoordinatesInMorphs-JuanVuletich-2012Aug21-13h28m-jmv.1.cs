'From Cuis 4.0 of 21 April 2012 [latest update: #1385] on 21 August 2012 at 1:29:09 pm'!

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 8/21/2012 12:38'!
enterClickableRegion: aMorphicEvent localPosition: localEventPosition
	| index isLink |
	aMorphicEvent hand hasSubmorphs ifTrue: [ ^self ].
	paragraph ifNotNil:[
		index _ (paragraph characterBlockAtPoint: localEventPosition) stringIndex.
		isLink _ (model actualContents attributesAt: index) 
					anySatisfy: [ :attr | attr mayActOnClick ].
		isLink ifTrue: [ Cursor webLink show ] ifFalse: [ Cursor normal show ]]! !


!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 8/21/2012 13:04'!
findSubmorphBinary: aBlock
	"Use binary search for finding a specific submorph of the receiver. Caller must be certain that the ordering holds for the submorphs."
	^submorphs findBinary: aBlock do: [ :found | found ] ifNone: [ :a :b | ]! !


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 13:23'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	(self containsPoint: aMouseButtonEvent eventPosition)
		ifTrue: [
			self selected: (localEventPosition y // self class itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 13:10'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |
	aMouseButtonEvent hand newKeyboardFocus: self.
	itemMorph _ self itemFromPoint: localEventPosition.
	(itemMorph notNil and: [
		itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)) ])
		ifTrue: [ ^self toggleExpandedState: itemMorph event: aMouseButtonEvent ]. 
	aMouseButtonEvent mouseButton2Pressed  "First check for option (menu) click"
		ifTrue: [ ^ self mouseButton2Activity ].
	itemMorph ifNil: [ ^super mouseDown: aMouseButtonEvent localPosition: localEventPosition ].
	itemMorph highlightForMouseDown.
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 13:10'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	| itemMorph |
	itemMorph _ self itemFromPoint: localEventPosition.
	itemMorph ifNil: [^self].
	itemMorph highlightedForMouseDown ifFalse: [^self].
	itemMorph highlightForMouseDown: false.
	"No change if model is locked"
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	((autoDeselect isNil or: [autoDeselect]) and: [itemMorph == selectedMorph]) 
		ifTrue: [self setSelectedMorph: nil]
		ifFalse: [self setSelectedMorph: itemMorph].
	Cursor normal show! !

!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 8/21/2012 13:08'!
itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	| ptY last internalPoint |
	internalPoint _ scroller internalize: aPoint.
	scroller hasSubmorphs ifFalse: [ ^nil ].
	(internalPoint > (0@0) and: [ internalPoint < scroller morphExtentInOwner ]) ifFalse: [ ^nil ].
	ptY _ internalPoint y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	scroller firstSubmorph morphPositionInOwner y > ptY ifTrue: [ ^nil ].
	last _ scroller lastSubmorph.
	last morphPositionInOwner y + last morphExtentInOwner y < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^scroller 
		findSubmorphBinary: [ :m |
			(m morphPositionInOwner y <= ptY and: [ m morphPositionInOwner y + m morphExtentInOwner y >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPositionInOwner y + (m morphExtentInOwner y // 2) > ptY ifTrue: [-1] ifFalse: [1]]]! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 13:13'!
drawLineToggleToTextOn: aCanvas lineColor: lineColor hasToggle: hasToggle
	"If I am not the only item in my container, draw the line between:
		- my toggle (if any) or my left edge (if no toggle)
		- and my text left edge"

	| myBounds myCenter hLineY hLineLeft |
	self isSoleItem ifTrue: [ ^self ].
	myBounds := self toggleRectangle translatedBy: self morphPositionInWorld.
	myCenter := myBounds center.
	hLineY := myCenter y.
	hasToggle
		ifTrue: [hLineLeft := myBounds right - 3]
		ifFalse: [hLineLeft := myCenter x - 1].
	"Draw line from toggle to text"
	aCanvas
		line: hLineLeft @ hLineY
		to: myBounds right + 0 @ hLineY
		width: 1
		color: lineColor! !

!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 13:13'!
drawLinesToFirstChildOn: aCanvas lineColor: lineColor 
	"Draw line from me to next sibling"

	| vLineX vLineTop vLineBottom childBounds childCenter |
	childBounds := self firstChild toggleRectangle translatedBy: self firstChild morphPositionInWorld.
	childCenter := childBounds center.
	vLineX := childCenter x - 1.
	vLineTop := self morphPositionInWorld y + self morphExtentInWorld y.
	self firstChild hasToggle
		ifTrue: [vLineBottom := childCenter y - 7]
		ifFalse: [vLineBottom := childCenter y].
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @ vLineBottom
		width: 1
		color: lineColor! !

!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 13:14'!
drawLinesToNextSiblingOn: aCanvas lineColor: lineColor hasToggle: hasToggle
	| myBounds nextSibBounds vLineX myCenter vLineTop vLineBottom |
	myBounds := self toggleRectangle translatedBy: self morphPositionInWorld.
	nextSibBounds := self nextSibling toggleRectangle translatedBy: self nextSibling morphPositionInWorld.
	myCenter := myBounds center.
	vLineX := myCenter x - 1.
	hasToggle
		ifTrue: [vLineTop := myCenter y + 5]
		ifFalse: [vLineTop := myCenter y].
	self nextSibling hasToggle
		ifTrue: [vLineBottom := nextSibBounds top + 2 ]
		ifFalse: [vLineBottom :=  nextSibBounds center y ].
	"Draw line from me to next sibling"
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @ vLineBottom
		width: 1
		color: lineColor! !

!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 13:15'!
drawOn: aCanvas

	| tRect sRect columnRect columnScanner columnData columnLeft colorToUse |

	tRect := self toggleRectangle translatedBy: self morphPositionInWorld.
	sRect := self morphBoundsInWorld withLeft: tRect right + 4.
	self drawToggleOn: aCanvas in: tRect.
	colorToUse _ complexContents preferredColor ifNil: [color].
	(container columns isNil or: [(contents asString indexOf: Character tab) = 0]) ifTrue: [
		aCanvas drawString: contents asString in: sRect font: self fontToUse color: colorToUse.
	] ifFalse: [
		columnLeft _ sRect left.
		columnScanner _ ReadStream on: contents asString.
		container columns do: [ :width |
			columnRect _ columnLeft @ sRect top extent: width @ sRect height.
			columnData _ columnScanner upTo: Character tab.
			columnData isEmpty ifFalse: [
				aCanvas drawString: columnData in: columnRect font: self fontToUse color: colorToUse.
			].
			columnLeft _ columnRect right + 5.
		].
	]
! !

!IndentingListItemMorph methodsFor: 'private' stamp: 'jmv 8/21/2012 12:47'!
toggleRectangle

	^(12*indentLevel @ 0) extent: 12@extent y! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 12:38'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent mouseButton1Pressed ifFalse: [
		^ self enterClickableRegion: aMouseMoveEvent localPosition: localEventPosition ].
	self handleInteraction: [
		editor mouseMove: aMouseMoveEvent localPosition: localEventPosition].
	owner scrollSelectionIntoView! !


!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 13:18'!
characterIndexAtPoint: aPoint

	| line block f |
	f _ self fontToUse.
	
	line _ TextLine 
		start: 1
		stop: contents size
		internalSpaces: 0
		paddingWidth: 0.
	line
		rectangle: (0@0 extent: extent);
		lineHeight: f height baseline: f ascent.
		
	block _ (CharacterBlockScanner new text: 
			(contents asText font: font))
		characterBlockAtPoint: aPoint index: nil
		in: line.

	^ block stringIndex! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 13:20'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	aMouseButtonEvent hand newKeyboardFocus: self.

	self handleInteraction: [
		self editor mouseDown: aMouseButtonEvent index: (self characterIndexAtPoint: localEventPosition) ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 13:20'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent mouseButton1Pressed ifFalse: [ ^ self ].
	self handleInteraction: [
		self editor mouseMove: aMouseMoveEvent index: (self characterIndexAtPoint: localEventPosition) ]! !


!TextDoIt class methodsFor: 'as yet unclassified' stamp: 'jmv 8/21/2012 12:36'!
evalString: str
	"
	('Some text. ',
	(Text string: '<click here>' attribute: (TextDoIt evalString: 'TranscriptWindow openTranscript')), 
	' more regular text') edit
	"
	^ self new evalString: str! !

!methodRemoval: InnerTextMorph #enterClickableRegion:!
InnerTextMorph removeSelector: #enterClickableRegion:!
!methodRemoval: IndentingListItemMorph #toggleBounds!
IndentingListItemMorph removeSelector: #toggleBounds!
