'From Cuis 5.0 [latest update: #4688] on 21 July 2021 at 12:09:48 pm'!
!classDefinition: #WindowEdgeAdjustingMorph category: #'Morphic-Layouts'!
LayoutAdjustingMorph subclass: #WindowEdgeAdjustingMorph
	instanceVariableNames: 'selector coordinateGetter cursorKey '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 7/21/2021 10:40:53'!
referencePositionInOwner
	^self externalize: self rotationCenter! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 7/21/2021 11:38:57'!
morphBottomLeft
	"Local coordinates
	Must be consistent with #morphTopLef and #morphExtent"

	^0 @ extent y.! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 7/21/2021 11:28:32'!
morphBottomRight
	"Local coordinates
	Must be consistent with #morphTopLef and #morphExtent"

	^extent.! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 7/21/2021 11:28:51'!
morphTopRight
	"Local coordinates
	Must be consistent with #morphTopLef and #morphExtent"

	^extent x @ 0.! !


!Morph methodsFor: 'events' stamp: 'jmv 7/21/2021 12:08:56'!
dragEvent: aMouseEvent localPosition: aPoint

	aMouseEvent hand halo: nil.
	aMouseEvent hand grabMorph: self! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 4/8/2021 15:09:29'!
grabMorph: aMorph delta: delta
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner formerPositionInWorld |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		(h target == aMorph or: [h target hasOwner: aMorph]) ifTrue: [
			self addMorphBack: h position: h morphPositionInWorld - self morphPositionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	formerOwner _ aMorph owner.
	formerOwner ifNotNil: [
		formerPositionInWorld _ aMorph morphPositionInWorld.
		grabMorphData
			at: aMorph 
			put: { formerOwner. formerPositionInWorld. } ].
	self addMorphBack: aMorph position: delta.

	aMorph displayBounds ifNotNil: [ :r | | d |
		formerPositionInWorld ifNotNil: [
			d _ aMorph morphPositionInWorld - formerPositionInWorld.
			d isZero ifFalse: [
				aMorph adjustDisplayBoundsBy: d ]]].

	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		lastPosition _ self morphPosition ].! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 7/21/2021 11:29:26'!
morphTopLeft
	"Local coordinates.
	Our hierarchy occupies a rectangle specified by #morphTopLef and #morphExtent"

	^`0@0`.! !

!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 7/21/2021 11:57:06'!
isOrthoRectangularMorph
	"True if we are of rectangular shape, aligned with local coordinate axes.
	Doesn't take into account possible rotation of our or some owner's local coordinates.
	Note: SystemWindow answers true, but implements #addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^self requiresVectorCanvas not! !


!LayoutAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 7/21/2021 10:49:13'!
adjustOwnerAt: aGlobalPoint

	owner
		adjustBy: self
		at: aGlobalPoint! !

!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 7/21/2021 11:59:05'!
stepAt: millisecondSinceLast
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [ Cursor defaultCursor activateCursor ].
		^ self stopStepping ].
	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPosition.
	hand lastMouseEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p.
			(owner isOrthoRectangularMorph and: [
			Preferences cheapWindowReframe or: [
			millisecondSinceLast > 200]]) ifTrue: [
				owner displayBounds newRectFrom: [ :f |
					self adjustOwnerAt: Sensor mousePoint.
					owner morphPosition extent: owner morphExtent ]]]
		ifFalse: [
			self stopStepping.
			hand _ nil.
			Cursor defaultCursor activateCursor ].! !


!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 7/21/2021 10:58:37'!
initializeBottom
	selector _ #windowBottom:.
	cursorKey _ #resizeBottomCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 7/21/2021 10:58:42'!
initializeBottomLeft
	selector _ #windowBottomLeft:.
	cursorKey _ #resizeBottomLeftCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 7/21/2021 10:59:29'!
initializeBottomRight
	selector _ #windowBottomRight:.
	cursorKey _ #resizeBottomRightCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 7/21/2021 10:59:35'!
initializeLeft
	selector _ #windowLeft:.
	cursorKey _ #resizeLeftCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 7/21/2021 10:59:39'!
initializeRight
	selector _ #windowRight:.
	cursorKey _ #resizeRightCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 7/21/2021 10:59:44'!
initializeTop
	selector _ #windowTop:.
	cursorKey _ #resizeTopCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 7/21/2021 10:59:54'!
initializeTopLeft
	selector _ #windowTopLeft:.
	cursorKey _ #resizeTopLeftCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 7/21/2021 10:59:58'!
initializeTopRight
	selector _ #windowTopRight:.
	cursorKey _ #resizeTopRightCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 7/21/2021 10:58:28'!
adjustOwnerAt: aGlobalPoint
	| pointInTargetOwners |

	owner ifNotNil: [
		pointInTargetOwners _ aGlobalPoint + 1.
		owner owner ifNotNil: [ :parent |
			pointInTargetOwners _ parent internalizeFromWorld: pointInTargetOwners ].
		owner perform: selector with: pointInTargetOwners ]! !

!WindowEdgeAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 7/21/2021 10:30:19'!
drawOn: aCanvas

	| c |
	c _ owner windowFrameColor.
	selector caseOf: {
		[ #windowTopLeft: ] -> [
			aCanvas roundTopLeftCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
		[ #windowTopRight: ] -> [
			aCanvas roundTopRightCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
		[ #windowBottomLeft: ] -> [
			aCanvas roundBottomLeftCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
		[ #windowBottomRight: ] -> [
			aCanvas roundBottomRightCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c ].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."	
		aCanvas roundEdge: self morphLocalBounds color: c ].! !


!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 7/21/2021 10:42:59'!
adjustBy: aLayoutAdjustMorph at: aGlobalPoint
	"See Class Comment of LayoutAdjustingMorph"

	| localPoint |
	localPoint _ self internalizeFromWorld: aGlobalPoint.
	direction == #horizontal ifTrue: [
		self adjustHorizontallyBy: aLayoutAdjustMorph at: localPoint ].

	direction == #vertical ifTrue: [
		self adjustVerticallyBy: aLayoutAdjustMorph at: localPoint ].! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 7/21/2021 10:44:12'!
adjustHorizontallyBy: aLayoutAdjustMorph at: localPoint
	| delta l ls r rs lNewWidth rNewWidth i lCurrentWidth rCurrentWidth |
	i _ submorphs indexOf: aLayoutAdjustMorph.
	((i = 1) or: [i = self submorphs size]) ifTrue: [
			 self inform: 'LayoutAdjustingMorphs require morphs before and after'.
			^self 
	].
	l _ self submorphs at: i +1.
	ls _ l layoutSpec.
	lCurrentWidth _ l morphWidth max: 1.	"avoid division by zero"
	r _ self submorphs at: i - 1.
	rs _ r layoutSpec.
	rCurrentWidth _ r morphWidth max: 1.	"avoid division by zero"
	delta _ localPoint x - aLayoutAdjustMorph referencePositionInOwner x.
	delta _ delta max: l minimumShrinkWidth - lCurrentWidth.
	delta _ delta min: rCurrentWidth - r minimumShrinkWidth.
	delta = 0 ifTrue: [ ^self ].
	rNewWidth _ rCurrentWidth - delta.
	lNewWidth _ lCurrentWidth + delta.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [ | leftNewProportion rightNewProportion toDistribute |	"If both proportional, update them"
			leftNewProportion _ lNewWidth / (lNewWidth + rNewWidth).
			rightNewProportion _ 1.0 - leftNewProportion.
			toDistribute _ ls proportionalLayoutWidth + rs proportionalLayoutWidth.
			ls setProportionalWidth: leftNewProportion * toDistribute.
			rs setProportionalWidth: rightNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 7/21/2021 10:41:44'!
adjustVerticallyBy: aLayoutAdjustMorph at: localPoint
	| delta t ts b bs tNewHeight bNewHeight i tCurrentHeight bCurrentHeight |
	i _ submorphs indexOf: aLayoutAdjustMorph.
	((i = 1) or: [i = self submorphs size]) ifTrue: [
			 self inform: 'LayoutAdjustingMorphs require morphs above and below'.
			^self 
	].	t _ self submorphs at: i +1.
	ts _ t layoutSpec.
	tCurrentHeight _ t morphHeight max: 1.	"avoid division by zero"
	b _ self submorphs at: i - 1.
	bs _ b layoutSpec.
	bCurrentHeight _ b morphHeight max: 1.	"avoid division by zero"
	delta _ localPoint y - aLayoutAdjustMorph referencePositionInOwner y.
	delta _ delta max: t minimumShrinkHeight - tCurrentHeight.
	delta _ delta min: bCurrentHeight - b minimumShrinkHeight.
	delta = 0 ifTrue: [ ^self ].
	tNewHeight _ tCurrentHeight + delta.
	bNewHeight _ bCurrentHeight - delta.
	(ts isProportionalHeight and: [ bs isProportionalHeight ])
		ifTrue: [ | bottomNewProportion toDistribute topNewProportion |	"If both proportional, update them"
			topNewProportion _ tNewHeight / (tNewHeight + bNewHeight).
			bottomNewProportion _ 1.0 - topNewProportion.
			toDistribute _ ts proportionalLayoutHeight + bs proportionalLayoutHeight.
			ts setProportionalHeight: topNewProportion * toDistribute.
			bs setProportionalHeight: bottomNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ts isProportionalHeight ifFalse: [
				ts fixedOrMorphHeight: tNewHeight ].
			bs isProportionalHeight ifFalse: [
				bs fixedOrMorphHeight: bNewHeight ]].
	self layoutSubmorphs! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 7/21/2021 12:01:11'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included."

	 | bounds radius |
	self isOrthoRectangularMorph ifFalse: [
		aCollection add: aRectangle.
		^self ].

	color mightBeTranslucent ifTrue: [
		aCollection add: aRectangle.
		^self ].

	bounds _ self displayBounds.
	bounds ifNil: [
		aCollection add: aRectangle.
		^self ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!!"
	Theme current roundWindowCorners ifFalse: [
		aRectangle areasOutside: bounds do: [ :rect |  aCollection add: rect ].
		^self ].

	"The solid rectangle does not include the corners.
	Report a couple of rows (top and bottom) or columns (left and right) as uncovered areas.
	We could also try to be more careful and answer each rounded corner...
	Right now, report top and bottom rows as uncovered areas"
	radius _ Theme current roundedWindowRadius.
	aRectangle areasOutside: (bounds insetBy: 0@radius) do: [ :rect |  aCollection add: rect ]! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 7/21/2021 11:43:49'!
windowBottom: aPointInOwner
	"aPointInOwner is in the owner's coordinate system"

	self morphHeight: (self internalize: aPointInOwner) y.! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 7/21/2021 11:39:10'!
windowBottomLeft: aPointInOwner
	"aPointInOwner is in the owner's coordinate system"

	| delta |
	delta _ (self internalize: aPointInOwner) - self morphBottomLeft.
	self morphExtent: self morphExtent + (delta x negated @ delta y).
	self morphPosition: self morphPosition + aPointInOwner - (self externalize: self morphBottomLeft).! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 7/21/2021 11:12:57'!
windowBottomRight: aPointInOwner
	"aPointInOwner is in the owner's coordinate system"

	self morphExtent: (self internalize: aPointInOwner).! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 7/21/2021 11:52:58'!
windowLeft: aPointInOwner
	"aPointInOwner is in the owner's coordinate system"

	| cornerInOwner newPositionInOwnCoordinates |
	cornerInOwner _ self externalize: extent.
	newPositionInOwnCoordinates _ (self internalize: aPointInOwner) x @ 0.
	self morphPosition: (self externalize: newPositionInOwnCoordinates).
	self morphExtent: (self internalize: cornerInOwner).! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 7/21/2021 11:44:22'!
windowRight: aPointInOwner
	"aPointInOwner is in the owner's coordinate system"

	self morphWidth: (self internalize: aPointInOwner) x.! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 7/21/2021 11:53:56'!
windowTop: aPointInOwner
	"aPointInOwner is in the owner's coordinate system"

	| cornerInOwner newPositionInOwnCoordinates |
	cornerInOwner _ self externalize: extent.
	newPositionInOwnCoordinates _ 0 @ (self internalize: aPointInOwner) y.
	self morphPosition: (self externalize: newPositionInOwnCoordinates).
	self morphExtent: (self internalize: cornerInOwner).! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 7/21/2021 11:18:31'!
windowTopLeft: aPointInOwner
	"aPointInOwner is in the owner's coordinate system"

	| cornerInOwner |
	cornerInOwner _ self externalize: extent.
	self morphPosition: aPointInOwner.
	self morphExtent: (self internalize: cornerInOwner).! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 7/21/2021 11:35:50'!
windowTopRight: aPointInOwner
	"aPointInOwner is in the owner's coordinate system"

	| delta |
	delta _ (self internalize: aPointInOwner) - self morphTopRight.
	self morphExtent: self morphExtent + (delta x @ delta y negated).
	self morphPosition: self morphPosition + aPointInOwner - (self externalize: self morphTopRight).! !


!LayoutAdjustingMorph reorganize!
('accessing' adoptWidgetsColor: cursor)
('initialization' defaultBorderWidth)
('adjusting' adjustOwnerAt:)
('drawing' drawOn:)
('events' mouseButton1Down:localPosition: mouseEnter: mouseLeave:)
('event handling testing' handlesMouseDown: handlesMouseOver:)
('stepping' stepAt: stepTime)
!

!classDefinition: #WindowEdgeAdjustingMorph category: #'Morphic-Layouts'!
LayoutAdjustingMorph subclass: #WindowEdgeAdjustingMorph
	instanceVariableNames: 'selector cursorKey'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
