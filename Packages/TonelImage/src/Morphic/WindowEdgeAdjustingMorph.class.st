"
I am a LayoutAdjustingMorph which acts on window edges or corners.

"
Class {
	#name : 'WindowEdgeAdjustingMorph',
	#superclass : 'LayoutAdjustingMorph',
	#instVars : [
		'selector',
		'cursorKey'
	],
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'instance creation' }
WindowEdgeAdjustingMorph class >> forBottom [
	^self new initializeBottom
]

{ #category : 'instance creation' }
WindowEdgeAdjustingMorph class >> forBottomLeft [
	^self new initializeBottomLeft
]

{ #category : 'instance creation' }
WindowEdgeAdjustingMorph class >> forBottomRight [
	^self new initializeBottomRight
]

{ #category : 'instance creation' }
WindowEdgeAdjustingMorph class >> forLeft [
	^self new initializeLeft
]

{ #category : 'instance creation' }
WindowEdgeAdjustingMorph class >> forRight [
	^self new initializeRight
]

{ #category : 'instance creation' }
WindowEdgeAdjustingMorph class >> forTop [
	^self new initializeTop
]

{ #category : 'instance creation' }
WindowEdgeAdjustingMorph class >> forTopLeft [
	^self new initializeTopLeft
]

{ #category : 'instance creation' }
WindowEdgeAdjustingMorph class >> forTopRight [
	^self new initializeTopRight
]

{ #category : 'adjusting' }
WindowEdgeAdjustingMorph >> adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast [

	self basicAdjustOwnerAt: aGlobalPoint.

	"If UI is becoming slow or is optimized for slow systems, resize without
	showing window contents, but only edges. But don't do it for rotated Windows!"
	(owner isOrAnyOwnerIsRotated not and: [
		(Preferences at: #cheapWindowReframe) or: [millisecondSinceLast > 100]]) ifTrue: [
			owner displayBounds newRect2From: [ :f |
				self stopStepping.
				hand := nil.
				self basicAdjustOwnerAt: Sensor mousePoint.
				owner morphPosition extent: owner extentInWorld ].
			Cursor defaultCursor activateCursor ].
]

{ #category : 'adjusting' }
WindowEdgeAdjustingMorph >> basicAdjustOwnerAt: aGlobalPoint [
	| pointInTargetOwners |

	owner ifNotNil: [
		pointInTargetOwners := aGlobalPoint + 1.
		owner owner ifNotNil: [ :parent |
			pointInTargetOwners := parent internalizeFromWorld: pointInTargetOwners ].
		owner perform: selector with: pointInTargetOwners ]
]

{ #category : 'geometry services' }
WindowEdgeAdjustingMorph >> coversLocalPoint: aLocalPoint [
	"We don't completely cover our bounds. Account for that."

	| sensitiveBorder |
	sensitiveBorder := owner borderWidth.
	((self localBounds outsetBy: sensitiveBorder) containsPoint: aLocalPoint) ifFalse: [ ^false ].
	selector caseOf: {
		[ #windowTopLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowTopRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowBottomLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ extent y- aLocalPoint y <= sensitiveBorder ]].
		[ #windowBottomRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ extent y - aLocalPoint y <= sensitiveBorder ]].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."
		^true ]
]

{ #category : 'accessing' }
WindowEdgeAdjustingMorph >> cursor [
	^ Cursor cursorAt: cursorKey.
]

{ #category : 'drawing' }
WindowEdgeAdjustingMorph >> drawOn: aCanvas [

	| c |
	(owner is: #SystemWindow) ifFalse: [
		^super drawOn: aCanvas ].

	"Use almost transparent, so effect on antialiasing for drawing exactly on top of Window is kept to a minimum."
	c := owner windowFrameColor alpha: 0.1.
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
		aCanvas roundEdge: self localBounds border: owner borderWidth color: c ].
]

{ #category : 'initialization' }
WindowEdgeAdjustingMorph >> initializeBottom [
	selector := #windowBottom:.
	cursorKey := #resizeBottomCursor.
	self setBalloonText: `

'Bottom resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s bottom edge.
	
', 'Resize to bottom screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s bottom half.

', 'Move to bottom screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s bottom edge.'`
]

{ #category : 'initialization' }
WindowEdgeAdjustingMorph >> initializeBottomLeft [
	selector := #windowBottomLeft:.
	cursorKey := #resizeBottomLeftCursor.
	self setBalloonText: `

'Bottom Left resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to bottom left corner.
	
', 'Resize to bottom left screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s bottom left quadrant.

', 'Move to bottom left screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s bottom left corner.'`
]

{ #category : 'initialization' }
WindowEdgeAdjustingMorph >> initializeBottomRight [
	selector := #windowBottomRight:.
	cursorKey := #resizeBottomRightCursor.
	self setBalloonText: `

'Bottom Right resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to bottom right corner.
	
', 'Resize to bottom right screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s bottom right quadrant.

', 'Move to bottom right screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s bottom right corner.'`
]

{ #category : 'initialization' }
WindowEdgeAdjustingMorph >> initializeLeft [
	selector := #windowLeft:.
	cursorKey := #resizeLeftCursor.
	self setBalloonText: `

'Left resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s left edge.
	
', 'Resize to left screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screens''s left half.

', 'Move to left screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s left edge.'`
]

{ #category : 'initialization' }
WindowEdgeAdjustingMorph >> initializeRight [
	selector := #windowRight:.
	cursorKey := #resizeRightCursor.
	self setBalloonText: `

'Right resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to right edge.
	
', 'Resize to right screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s right half.

', 'Move to right screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s right edge.'`
]

{ #category : 'initialization' }
WindowEdgeAdjustingMorph >> initializeTop [
	selector := #windowTop:.
	cursorKey := #resizeTopCursor.
	self setBalloonText: `

'Top resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s top edge.
	
', 'Resize to top screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screens''s top half.

', 'Move to top screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s top edge.'`
]

{ #category : 'initialization' }
WindowEdgeAdjustingMorph >> initializeTopLeft [
	selector := #windowTopLeft:.
	cursorKey := #resizeTopLeftCursor.
	self setBalloonText: `

'Top Left resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to top left corner.
	
', 'Resize to top left screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s top left quadrant.

', 'Move to top left screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s top left corner.'`
]

{ #category : 'initialization' }
WindowEdgeAdjustingMorph >> initializeTopRight [
	selector := #windowTopRight:.
	cursorKey := #resizeTopRightCursor.
	self setBalloonText: `

'Top Right resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to top right corner.
	
', 'Resize to top right screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s top right quadrant.

', 'Move to top right screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s top right corner.'`
]

{ #category : 'geometry' }
WindowEdgeAdjustingMorph >> minimumExtent [
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ `0@0`
]

{ #category : 'events' }
WindowEdgeAdjustingMorph >> mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition [
	"Some nice window expansions."
	
	self owner ifNotNil: [ :o |
		o perform: ((selector copyUpToLast: $:) 
			copyReplaceAll: 'window' 
			with: (aMouseButtonEvent shiftPressed ifFalse: [ 'resize' ] ifTrue: [ 'move' ])) asSymbol ]
]
