'From Cuis 4.0 of 21 April 2012 [latest update: #1444] on 19 September 2012 at 6:35:42 pm'!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent targetOffset damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevBounds prevFullBounds '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
!classDefinition: #ScrollBar category: #'Morphic-Support'!
PluggableMorph subclass: #ScrollBar
	instanceVariableNames: 'slider value setValueSelector sliderShadow upButton downButton scrollDelta pageDelta interval nextPageDirection grabPosition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!FormCanvas methodsFor: 'accessing' stamp: 'jmv 9/19/2012 17:17'!
currentTransformation
	"Warning. Only valid inside a #drawOn: method"
	^currentTransformation! !


!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 9/19/2012 18:22'!
scrollTo: handPositionRelativeToSlider
	| v handPositionRelativeToUs |
	handPositionRelativeToUs _ slider externalize: handPositionRelativeToSlider.
	v _ (self isHorizontal
		ifTrue: [ handPositionRelativeToUs x - grabPosition x ]
		ifFalse: [ handPositionRelativeToUs y - grabPosition y ])
			- borderWidth - self buttonExtent * 1.0
				/ self freeSliderRoom.
	self setValue: v! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 9/19/2012 18:22'!
sliderGrabbedAt: handPositionRelativeToSlider

	grabPosition _ handPositionRelativeToSlider.
	sliderShadow
		morphPositionInOwner: slider morphPositionInOwner;
		morphExtent: slider morphExtent;
		show! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/19/2012 17:54'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."
	aMouseButtonEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.

	"Make me modal during mouse transitions"
	aMouseButtonEvent hand newMouseFocus: self.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ ^self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition ].

	self mouseDown: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue:[
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue + self mouseStillDownThreshold
			arguments: {aMouseButtonEvent copy resetHandlerFields . localEventPosition}
			stepTime: self mouseStillDownStepRate ].
! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/19/2012 17:27'!
internalize: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."
	^ location internalizePosition: aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/19/2012 17:28'!
internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."
	| inOwners |
	self flag: #jmvVer2.
	inOwners _ owner
		ifNotNil: [ owner internalizeFromWorld: aPoint ]
		ifNil: [ aPoint ].
	^self internalize: inOwners! !


!DraggeableButtonMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 17:47'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Inform the model that this button has been released. "
	super mouseDown: aMouseButtonEvent localPosition: localEventPosition.
	grabSelector ifNotNil: [
		model perform: grabSelector with: localEventPosition ]! !

!DraggeableButtonMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 17:50'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	dragSelector ifNotNil: [
		model perform: dragSelector with: localEventPosition ]! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 9/19/2012 17:53'!
attachMorph: m
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."
	| delta |
	self releaseMouseFocus. "Break focus"
	self addMorphBack: m.
	delta _ m morphExtentInWorld // 2.
	m morphPosition: (self morphPosition - delta).! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 9/19/2012 17:54'!
grabMorph: aMorph from: formerOwner
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	self releaseMouseFocus. "Break focus"
	self addMorphBack: aMorph.
	aMorph justGrabbedFrom: formerOwner.! !


!InnerListMorph methodsFor: 'list management' stamp: 'jmv 9/19/2012 16:57'!
rowAtLocation: aPoint
	"return the number of the row at aPoint"

	^aPoint y // font height + 1 min: listItems size max: 1! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/19/2012 17:20'!
bottomVisibleRowForCanvas: aCanvas
	"return the bottom visible row in aCanvas's clip rectangle"
	| tx |
	tx _ aCanvas currentTransformation.
	^ (self rowAtLocation: (tx internalizePosition: aCanvas clipRect bottomLeft))
		max: (self rowAtLocation: (tx internalizePosition: aCanvas clipRect bottomRight))! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/19/2012 17:21'!
drawOn: aCanvas

	listItems size = 0 ifTrue: [ ^self ].
 
	self drawSelectionOn: aCanvas.

	(self topVisibleRowForCanvas: aCanvas)
		to: (self bottomVisibleRowForCanvas: aCanvas)
		do: [ :row |
			(owner itemSelectedAmongMultiple:  row) ifTrue: [
				self drawBackgroundForMulti: row on: aCanvas. ].
			self draw: (self item: row) asStringOrText atRow: row on: aCanvas ]! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/19/2012 17:21'!
topVisibleRowForCanvas: aCanvas
	"return the top visible row in aCanvas's clip rectangle"
	| tx |
	tx _ aCanvas currentTransformation.
	^ (self rowAtLocation: (tx internalizePosition: aCanvas clipRect topLeft))
		min: (self rowAtLocation: (tx internalizePosition: aCanvas clipRect topRight))! !


!MouseClickState methodsFor: 'actions' stamp: 'jmv 9/19/2012 17:54'!
didClickAndHalf

	clickAndHalfDone ifFalse: [
		clickAndHalfSelector ifNotNil: [
			"Focus was lost at buttonUp. Set it again."
			lastClickDown hand newMouseFocus: clickClient.
			clickClient perform: clickAndHalfSelector with: lastClickDown with: self lastClickLocalPosition ].
		clickAndHalfDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 9/19/2012 17:54'!
didCoubleClickAndHalf

	doubleClickAndHalfDone ifFalse: [
		dblClickAndHalfSelector ifNotNil: [
			"Focus was lost at buttonUp. Set it again."
			lastClickDown hand newMouseFocus: clickClient.
			clickClient perform: dblClickAndHalfSelector with: lastClickDown with: self lastClickLocalPosition ].
		doubleClickAndHalfDone _ true ]! !


!PluggableListMorph methodsFor: 'accessing' stamp: 'jmv 9/19/2012 17:00'!
rowAtLocation: aPoint
	"Return the row at the given point or 0 if outside"

	| m |
	m _ self listMorph.
	^m rowAtLocation: (m internalize: aPoint)! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 17:00'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	| index |
	doubleClickSelector ifNil: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index _ self rowAtLocation: localEventPosition.
	index = 0 ifTrue: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	"selectedMorph ifNil: [self setSelectedMorph: aMorph]."
	^ self model perform: doubleClickSelector! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 17:01'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	| row |
	"First check for option (menu) click"
	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		^ self mouseButton2Activity ].
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		self listMorph noSelection].
	row _ self rowAtLocation: localEventPosition.
	row = 0  ifTrue: [ ^super mouseDown: aMouseButtonEvent localPosition: localEventPosition ].
	"self dragEnabled ifTrue: [aMorph highlightForMouseDown]."
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 17:02'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	"The mouse came up within the list; take appropriate action"

	| row |
	row _ self rowAtLocation: localEventPosition.
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	(autoDeselect == false and: [row = 0]) ifTrue: [^ self].  "work-around the no-mans-land bug"
	"No change if model is locked"
	((autoDeselect == nil or: [autoDeselect]) and: [row == self selectionIndex])
		ifTrue: [self changeModelSelection: 0]
		ifFalse: [self changeModelSelection: row].
	Cursor normal show.
! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 9/19/2012 17:02'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	| oldIndex oldVal row w |
	aMouseButtonEvent mouseButton2Pressed ifTrue: [ ^ self mouseButton2Activity ].

	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].

	row _ self rowAtLocation: localEventPosition.

	row = 0 ifTrue: [ ^super mouseDown: aMouseButtonEvent localPosition: localEventPosition ].

	w _ self ownerThatIsA: SystemWindow.
	(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

		"Set meaning for subsequent dragging of selection"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row.
		oldIndex _ self getCurrentSelectionIndex.
		oldIndex ~= 0 ifTrue: [oldVal _ self listSelectionAt: oldIndex].

		"Set or clear new primary selection (listIndex)"
		dragOnOrOff
			ifTrue: [self changeModelSelection: row]
			ifFalse: [self changeModelSelection: 0].

		"Need to restore the old one, due to how model works, and set new one."
		oldIndex ~= 0 ifTrue: [self listSelectionAt: oldIndex put: oldVal].
		self listSelectionAt: row put: dragOnOrOff.
		"event hand releaseMouseFocus: aMorph."
		"aMorph changed"
	].
	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 9/19/2012 17:05'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row |
	row _ (localEventPosition y < 0 and: [ scrollBar value > 0.0 ])
		ifTrue: [
			scrollBar scrollUp: 1.
			"Leave at least one visible item unaffected, for better visual feedback to the user."
			(self rowAtLocation: 0@0) + 2 ]
		ifFalse: [
			(localEventPosition y > extent y and: [ scrollBar value < 1.0 ])
				ifTrue: [
					scrollBar scrollDown: 1.
					"Leave at least one visible item unaffected, for better visual feedback to the user."
					(self rowAtLocation: 0@extent y) - 3 ]
				ifFalse: [ self rowAtLocation: localEventPosition ]].
	row = 0 ifTrue: [ ^ self ].

	"No change if model is locked"
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].

	dragOnOrOff ifNil: [
		"Was not set at mouse down, which means the mouse must have gone down in an area where there was no list item"
		dragOnOrOff _ (self listSelectionAt: row) not ].

	"Set meaning for subsequent dragging of selection"
	oldIndex _ self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [ oldVal _ self listSelectionAt: oldIndex ].

	"Set or clear new primary selection (listIndex)"
	dragOnOrOff 
		ifTrue: [ self changeModelSelection: row ]
		ifFalse: [ self changeModelSelection: 0 ].

	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [ self listSelectionAt: oldIndex put: oldVal ].
	
	"Select all in between if drag was too fast"
	"self listSelectionAt: row put: dragOnOrOff."
	(row min: dragStartRow) to: (row max: dragStartRow) do: [ :r |
		self listSelectionAt: r put: dragOnOrOff ]! !


!PointerExplorerWrapper methodsFor: 'reference chain' stamp: 'jmv 9/19/2012 17:29'!
shortestPathFromRoot: anArrayOfWrappers

	| nextLevel allPointers eachWrapper pointersToEachObject alreadyIncluded |
	alreadyIncluded _ IdentitySet new.
	nextLevel _ Array streamContents: [ :strm |
		allPointers _ Smalltalk pointersToEachIn: (anArrayOfWrappers collect: [ :eachWrapper1 | eachWrapper1 item ]).

		1 to: anArrayOfWrappers size do: [ :i |
			eachWrapper _ anArrayOfWrappers at: i.
			pointersToEachObject _ allPointers at: i.

			"Can we have any other root?"
			eachWrapper item == Smalltalk specialObjectsArray ifTrue: [
				^eachWrapper ].		"Found it!!"

			pointersToEachObject do: [ :pointingObject |
				"Reject PointerExplorer stuff (wrapper and main model).
				Reject weak refs, unles includeWeakRefs is true."
				(pointingObject class = self class or: [ pointingObject class = PointerExplorer or: [
				pointingObject class isWeak and: [model includeWeakRefs not]]]) ifFalse: [
					(alreadyIncluded includes: pointingObject) ifFalse: [
						alreadyIncluded add: pointingObject.
						strm nextPut: (self class
							with: pointingObject
							name: pointingObject identityHash asString
							model: model
							parent: eachWrapper )]]]]].

	"Release unneeded references"
	allPointers do: [ :oc |
		oc setContents: #() ].
	alreadyIncluded _ nil.
	nextLevel isEmpty ifTrue: [ ^nil ].
	^self shortestPathFromRoot: nextLevel! !


!ScrollBar methodsFor: 'initialize' stamp: 'jmv 9/19/2012 17:47'!
initializeSlider
	"initialize the receiver's slider"

	sliderShadow _ RectangleLikeMorph new.
	self addMorph: sliderShadow.
	sliderShadow hide.
		
	slider _ self sliderClass new.
	slider model: self.
	slider grabSelector: #sliderGrabbedAt:.
	slider dragSelector: #scrollTo:.
	slider action: #sliderReleased.
	self addMorph: slider.

	self computeSlider! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 9/19/2012 17:48'!
sliderReleased

	grabPosition _ nil.
	sliderShadow hide! !


!TextEditor methodsFor: 'as yet unclassified' stamp: 'jmv 9/19/2012 18:34'!
visibleHeight

	^morph owner morphHeight! !

!methodRemoval: ScrollBar #scrollAbsolute:!
ScrollBar removeSelector: #scrollAbsolute:!
!methodRemoval: ScrollBar #scrollTo::!
ScrollBar removeSelector: #scrollTo::!
!methodRemoval: ScrollBar #sliderGrabbed!
ScrollBar removeSelector: #sliderGrabbed!
!classDefinition: #ScrollBar category: #'Morphic-Support'!
PluggableMorph subclass: #ScrollBar
	instanceVariableNames: 'slider value setValueSelector sliderShadow upButton downButton scrollDelta pageDelta interval nextPageDirection grabPosition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!methodRemoval: Rectangle #isZero!
Rectangle removeSelector: #isZero!
!methodRemoval: PluggableListMorph #zzrowAtLocation:!
PluggableListMorph removeSelector: #zzrowAtLocation:!
!methodRemoval: MouseEvent #targetPoint!
MouseEvent removeSelector: #targetPoint!
!methodRemoval: InnerListMorph #zzrowAtLocation:!
InnerListMorph removeSelector: #zzrowAtLocation:!
!methodRemoval: HandMorph #newMouseFocus:event:!
HandMorph removeSelector: #newMouseFocus:event:!
!methodRemoval: HandMorph #targetOffset!
HandMorph removeSelector: #targetOffset!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevBounds prevFullBounds'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
!methodRemoval: Morph #validateNotSent!
Morph removeSelector: #validateNotSent!
!methodRemoval: Collection #isZero!
Collection removeSelector: #isZero!
