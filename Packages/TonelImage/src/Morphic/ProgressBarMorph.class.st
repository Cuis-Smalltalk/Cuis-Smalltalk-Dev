Class {
	#name : 'ProgressBarMorph',
	#superclass : 'BorderedBoxMorph',
	#instVars : [
		'value',
		'progressColor'
	],
	#category : 'Morphic-Composite Widgets',
	#package : 'Morphic',
	#tag : 'Composite Widgets'
}

{ #category : 'new-morph participation' }
ProgressBarMorph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu"

	^ false
]

{ #category : 'initialization' }
ProgressBarMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 1
]

{ #category : 'initialization' }
ProgressBarMorph >> defaultColor [
	^ `Color white`
]

{ #category : 'drawing' }
ProgressBarMorph >> drawOn: aCanvas [

	| twoBorders |
	super drawOn: aCanvas.
	twoBorders := borderWidth + borderWidth.
	aCanvas
		fillRectangle: (borderWidth @ borderWidth extent: extent x * value @ extent y - twoBorders)
		color: progressColor
]

{ #category : 'initialization' }
ProgressBarMorph >> initialize [
	super initialize.
	progressColor := `Color gray`.
	value := 0.0
]

{ #category : 'accessing' }
ProgressBarMorph >> progressValue [
	^value
]

{ #category : 'accessing' }
ProgressBarMorph >> progressValue: aValue [
	value := aValue.
	self redrawNeeded
]
