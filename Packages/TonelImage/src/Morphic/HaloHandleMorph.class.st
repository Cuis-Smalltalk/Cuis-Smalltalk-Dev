Class {
	#name : 'HaloHandleMorph',
	#superclass : 'ColoredBoxMorph',
	#instVars : [
		'mouseDownSelector',
		'mouseUpSelector',
		'mouseMoveSelector'
	],
	#category : 'Morphic-Halos',
	#package : 'Morphic',
	#tag : 'Halos'
}

{ #category : 'new-morph participation' }
HaloHandleMorph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu"

	^ false
]

{ #category : 'drawing' }
HaloHandleMorph >> drawOn: aCanvas [

	aCanvas
		ellipseCenter: extent // 2 radius: extent // 2 borderWidth: 0 borderColor: nil fillColor: color
]

{ #category : 'event handling testing' }
HaloHandleMorph >> handlesMouseDown: aMouseButtonEvent [
	"Do I want to receive mouseDown events (mouseDown:, mouseUp:)?"
	
	^ mouseDownSelector notNil | mouseUpSelector notNil
]

{ #category : 'testing' }
HaloHandleMorph >> isIncludedInTaskbar [
	"Answer true a button for us should be added to any TaskbarMorph."

	^ false
]

{ #category : 'events' }
HaloHandleMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse down event."
	self send: mouseDownSelector withEvent: aMouseButtonEvent
]

{ #category : 'events' }
HaloHandleMorph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse up event."
	self send: mouseUpSelector withEvent: aMouseButtonEvent
]

{ #category : 'accessing' }
HaloHandleMorph >> mouseDownSelector: aSymbol [
	mouseDownSelector := aSymbol
]

{ #category : 'events' }
HaloHandleMorph >> mouseMove: aMouseMoveEvent localPosition: localEventPosition [
	"Handle a mouse move event."
	aMouseMoveEvent anyButtonPressed ifTrue: [
		self send: mouseMoveSelector withEvent: aMouseMoveEvent ]
]

{ #category : 'accessing' }
HaloHandleMorph >> mouseMoveSelector: aSymbol [
	mouseMoveSelector := aSymbol
]

{ #category : 'accessing' }
HaloHandleMorph >> mouseUpSelector: aSymbol [
	mouseUpSelector := aSymbol
]

{ #category : 'act' }
HaloHandleMorph >> send: selector withEvent: aMouseEvent [
	| arity |
	owner ifNil: [ ^ self ].
	selector ifNil: [ ^ self ].
	arity := selector numArgs.
	arity = 0 ifTrue: [ ^ owner perform: selector ].
	arity = 1 ifTrue: [
		^ owner
			perform: selector
			with: aMouseEvent ].
	arity = 2 ifTrue: [
		^ owner
			perform: selector
			with: aMouseEvent
			with: self ].
	self error: 'Event handling selectors must be Symbols and take 0-2 arguments'
]
