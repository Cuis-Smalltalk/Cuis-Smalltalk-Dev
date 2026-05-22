"
I'm an event related to the host window, only dispatched to the World. 
"
Class {
	#name : 'WindowEvent',
	#superclass : 'MorphicEvent',
	#instVars : [
		'action',
		'rectangle'
	],
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'dispatching' }
WindowEvent >> dispatchIn: aWorldMorph [
	"Host window events do not have a position and are only dispatched to the World"

	self wasHandled not ifTrue: [
		(aWorldMorph wantsWindowEvent: self) ifTrue: [
			self sendEventTo: aWorldMorph ]].
]

{ #category : 'accessing' }
WindowEvent >> eventPosition [
	"Answer something..."
	^rectangle topLeft
]

{ #category : 'testing' }
WindowEvent >> isWindowEvent [
	^true
]

{ #category : 'accessing' }
WindowEvent >> rectangle [
	^rectangle
]

{ #category : 'accessing' }
WindowEvent >> rectangle: aValue [
	rectangle := aValue.
]

{ #category : 'dispatching' }
WindowEvent >> sendEventTo: aMorph [
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processWindowEvent: self.
]

{ #category : 'accessing' }
WindowEvent >> windowAction: aValue [

	action := aValue
]

{ #category : 'accessing' }
WindowEvent >> windowEventType [
	"This should match the definitions in sq.h"
	^#(
		windowMetricChange
		windowClose
		windowIconise
		windowActivated
		windowPaint
	) at: action ifAbsent: [#windowEventUnknown]
]
