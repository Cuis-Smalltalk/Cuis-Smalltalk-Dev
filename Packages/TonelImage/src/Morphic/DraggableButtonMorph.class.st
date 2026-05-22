Class {
	#name : 'DraggableButtonMorph',
	#superclass : 'PluggableButtonMorph',
	#instVars : [
		'grabSelector',
		'dragSelector'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
DraggableButtonMorph class >> includeInNewMorphMenu [
	"Not to be instantiated from the menu"
	^ false
]

{ #category : 'accessing' }
DraggableButtonMorph >> dragSelector: aSymbol [
	dragSelector := aSymbol
]

{ #category : 'accessing' }
DraggableButtonMorph >> grabSelector: aSymbol [
	grabSelector := aSymbol
]

{ #category : 'initialization' }
DraggableButtonMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.

	grabSelector := nil.
	dragSelector := nil.
]

{ #category : 'events' }
DraggableButtonMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	"Inform the model that this button has been released. "
	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	grabSelector ifNotNil: [
		model perform: grabSelector with: localEventPosition ]
]

{ #category : 'events' }
DraggableButtonMorph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [
	isPressed := false.
	mouseIsOver := false.
	actWhen == #buttonUp
		ifTrue: [ self performAction ].
	self redrawNeeded
]

{ #category : 'events' }
DraggableButtonMorph >> mouseMove: aMouseMoveEvent localPosition: localEventPosition [

	aMouseMoveEvent anyButtonPressed ifTrue: [
		dragSelector ifNotNil: [
			model perform: dragSelector with: localEventPosition ]]
]

{ #category : 'drawing' }
DraggableButtonMorph >> roundButtonRect [
	"Use full bounds for drawing.
	See #computeSlider and #expandSlider"
	^self localBounds
]
