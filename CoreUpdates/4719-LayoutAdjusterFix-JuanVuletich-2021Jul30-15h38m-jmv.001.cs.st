'From Cuis 5.0 [latest update: #4716] on 30 July 2021 at 3:44:24 pm'!

!LayoutAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 7/30/2021 15:20:27'!
adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast

	owner
		adjustBy: self
		at: aGlobalPoint! !


!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 7/30/2021 15:22:03'!
adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast

	self basicAdjustOwnerAt: aGlobalPoint.
	(owner isOrthoRectangularMorph and: [
	Preferences cheapWindowReframe or: [
	millisecondSinceLast > 200]]) ifTrue: [
		owner displayBounds newRectFrom: [ :f |
			self basicAdjustOwnerAt: Sensor mousePoint.
			owner morphPosition extent: owner morphExtent ]]! !

!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 7/30/2021 15:18:57'!
basicAdjustOwnerAt: aGlobalPoint
	| pointInTargetOwners |

	owner ifNotNil: [
		pointInTargetOwners _ aGlobalPoint + 1.
		owner owner ifNotNil: [ :parent |
			pointInTargetOwners _ parent internalizeFromWorld: pointInTargetOwners ].
		owner perform: selector with: pointInTargetOwners ]! !


!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 7/30/2021 15:21:34'!
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
			self adjustOwnerAt: p millisecondSinceLast: millisecondSinceLast ]
		ifFalse: [
			self stopStepping.
			hand _ nil.
			Cursor defaultCursor activateCursor ].! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 7/30/2021 15:41:46'!
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
	Right now, report left and right columns as uncovered areas"
	radius _ Theme current roundedWindowRadius.
	aRectangle areasOutside: (bounds insetBy: radius@0) do: [ :rect |  aCollection add: rect ].! !

!methodRemoval: WindowEdgeAdjustingMorph #adjustOwnerAt: stamp: 'jmv 7/30/2021 15:39:54'!
WindowEdgeAdjustingMorph removeSelector: #adjustOwnerAt:!
!methodRemoval: LayoutAdjustingMorph #adjustOwnerAt: stamp: 'jmv 7/30/2021 15:39:55'!
LayoutAdjustingMorph removeSelector: #adjustOwnerAt:!
