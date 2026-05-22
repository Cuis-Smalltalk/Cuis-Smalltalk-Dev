"
BorderedBoxMorph adds customizable border, see instanceVariables borderWidth and borderColor.
Subclasses can use a variety of border styles: simple, inset, raised. It also adds padding, a separation between the border and inner contents. Some subclasses may ignore either border or padding.
"
Class {
	#name : 'BorderedBoxMorph',
	#superclass : 'ColoredBoxMorph',
	#instVars : [
		'borderWidth',
		'borderColor',
		'padding'
	],
	#category : 'Morphic-Kernel',
	#package : 'Morphic',
	#tag : 'Kernel'
}

{ #category : 'accessing' }
BorderedBoxMorph >> borderColor [
	^ borderColor
]

{ #category : 'accessing' }
BorderedBoxMorph >> borderColor: aColor [
	borderColor = aColor ifFalse: [
		borderColor := aColor.
		self redrawNeeded]
]

{ #category : 'accessing' }
BorderedBoxMorph >> borderWidth [
	^ borderWidth
]

{ #category : 'accessing' }
BorderedBoxMorph >> borderWidth: anInteger [

	borderWidth = anInteger ifFalse: [
		self adjustExtentFor: [ borderWidth := anInteger max: 0 ]].
]

{ #category : 'initialization' }
BorderedBoxMorph >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ `Color gray`
]

{ #category : 'initialization' }
BorderedBoxMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 5
]

{ #category : 'initialization' }
BorderedBoxMorph >> defaultColor [
	^ `Color orange`
]

{ #category : 'initialization' }
BorderedBoxMorph >> defaultPadding [
	"Answer the default inner padding for the receiver.
	Redefine this method for a different initial padding in your morphs."

	^ `0@0`
]

{ #category : 'drawing' }
BorderedBoxMorph >> drawOn: aCanvas [
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.

	"Make the existence of the normal invisible padding evident.
	This is usually not done, and subclasses may assume this inherited behavior.
	The following code is only included for throw-away experimentation."
"
	aCanvas
		frameRectangle: (self localBounds insetBy: borderWidth)
		borderWidth: padding x
		color: `Color lightGreen alpha: 0.5`.
	aCanvas
		frameRectangle: (self localBounds insetBy: borderWidth)
		borderWidth: padding y
		color: `Color lightGreen alpha: 0.5`.
"
]

{ #category : 'initialization' }
BorderedBoxMorph >> initialize [
	super initialize.
	borderColor := self defaultBorderColor.
	borderWidth := self defaultBorderWidth.
	padding := self defaultPadding.
]

{ #category : 'geometry' }
BorderedBoxMorph >> interiorExtent [

	"Answer extent inset from borderWidth and Padding"
	^self morphExtent - (2 * borderWidth) - (2 * padding)
]

{ #category : 'geometry' }
BorderedBoxMorph >> interiorOrigin [

	"Answer point from borderWidth and Padding"
	| x y |
	x := borderWidth + padding x.
	y := borderWidth + padding y.
	^x @ y
]

{ #category : 'testing' }
BorderedBoxMorph >> is: aSymbol [
	^ aSymbol == #BorderedBoxMorph or: [ super is: aSymbol ]
]

{ #category : 'geometry' }
BorderedBoxMorph >> minimumExtent [
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ super minimumExtent + (2 * self interiorOrigin)
]

{ #category : 'accessing' }
BorderedBoxMorph >> padding [
	^ padding
]

{ #category : 'accessing' }
BorderedBoxMorph >> padding: numberOrPoint [

	| p |
	p := numberOrPoint asPoint max: `0@0`.
	padding = p ifFalse: [
		self adjustExtentFor: [ padding := p ]].
]
