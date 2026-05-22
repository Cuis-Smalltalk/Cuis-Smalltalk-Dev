"
ColoredBoxMorph adds a customizable background color to BoxMorph.
"
Class {
	#name : 'ColoredBoxMorph',
	#superclass : 'BoxMorph',
	#instVars : [
		'color'
	],
	#category : 'Morphic-Kernel',
	#package : 'Morphic',
	#tag : 'Kernel'
}

{ #category : 'accessing' }
ColoredBoxMorph >> color [

	^ color
]

{ #category : 'accessing' }
ColoredBoxMorph >> color: aColor [
	"Set the receiver's color. "
	color = aColor ifFalse: [
		color := aColor.
		self redrawNeeded ]
]

{ #category : 'initialization' }
ColoredBoxMorph >> defaultColor [
	^ `Color lightOrange duller`
]

{ #category : 'drawing' }
ColoredBoxMorph >> drawOn: aCanvas [
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: color.
]

{ #category : 'initialization' }
ColoredBoxMorph >> initialize [
	super initialize.
	color := self defaultColor.
]

{ #category : 'testing' }
ColoredBoxMorph >> is: aSymbol [
	^ aSymbol == #ColoredBoxMorph or: [ super is: aSymbol ]
]
