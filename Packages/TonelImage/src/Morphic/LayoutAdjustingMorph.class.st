"
When added to the submorphs of a LayoutMorph, 
I am a way of directly adjusting layout width or height.

I take care of the user interactions and delegate the actual changes to my containing LayoutMorph.
See LayoutAdjustingMorph>>adjustOwnerAt:

The way adjustments are made differs between fixed and proportional Morphs.

If I am between two proportional Morphs, the relative Morph proportions on both sides of me are adjusted.

If the user is moving me between a fixed and a proportional morph, the fixed size is adjusted.

The general effect is that if between 2 proportional Morphs, the proportions are adjusted and the
user sees the boundary between two morphs change.  If one Morph is fixed, then the user sees
the proportional Morphs ALL adjust.  An "accordian like" effect.  This is NOT due to a change 
in relative proportions but just the proportional expansion or squeezing of the proportional 
Morphs themselves.

E.g. note LayoutMorph class>>example11 which has a fixed Morph at each end.

"
Class {
	#name : 'LayoutAdjustingMorph',
	#superclass : 'ColoredBoxMorph',
	#instVars : [
		'hand'
	],
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'new-morph participation' }
LayoutAdjustingMorph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu"
	^ false
]

{ #category : 'adjusting' }
LayoutAdjustingMorph >> adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast [

	owner
		adjustBy: self
		at: aGlobalPoint
]

{ #category : 'accessing' }
LayoutAdjustingMorph >> adoptWidgetsColor: paneColor [
	super adoptWidgetsColor: paneColor.
	self color: paneColor
]

{ #category : 'accessing' }
LayoutAdjustingMorph >> cursor [

	^self localBounds isTall 
		ifTrue: [ Cursor cursorAt: #resizeLeftCursor ]
		ifFalse: [ Cursor cursorAt: #resizeTopCursor ].
]

{ #category : 'initialization' }
LayoutAdjustingMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 0
]

{ #category : 'drawing' }
LayoutAdjustingMorph >> drawOn: aCanvas [

	aCanvas
		fillRectangle: self localBounds
		color: color
]

{ #category : 'event handling testing' }
LayoutAdjustingMorph >> handlesMouseDown: aMouseButtonEvent [

	^ true
]

{ #category : 'event handling testing' }
LayoutAdjustingMorph >> handlesMouseOver: evt [
	"implements #mouseEnter: and/or #mouseLeave:"
	^ true
]

{ #category : 'events' }
LayoutAdjustingMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	self cursor activateCursor.
	hand := aMouseButtonEvent hand.
	self startStepping
]

{ #category : 'events' }
LayoutAdjustingMorph >> mouseEnter: aMouseEnterEvent [
	super mouseEnter: aMouseEnterEvent.
	self cursor activateCursor.
]

{ #category : 'events' }
LayoutAdjustingMorph >> mouseLeave: aMouseLeaveEvent [
	super mouseLeave: aMouseLeaveEvent.
	aMouseLeaveEvent mouseButton1Pressed ifFalse: [
		Cursor defaultCursor activateCursor ].
]

{ #category : 'stepping' }
LayoutAdjustingMorph >> stepAt: millisecondSinceLast [
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [ Cursor defaultCursor activateCursor ].
		^ self stopStepping ].
	"hasn't got the #mouseLeave: message (yet)"
	p := hand morphPosition.
	hand lastMouseEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p millisecondSinceLast: millisecondSinceLast ]
		ifFalse: [
			self stopStepping.
			hand := nil.
			Cursor defaultCursor activateCursor ].
]

{ #category : 'stepping' }
LayoutAdjustingMorph >> stepTime [
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!"
	^ 20
]
