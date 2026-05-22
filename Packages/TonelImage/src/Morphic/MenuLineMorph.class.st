"
Just a line for separating items in menus.
"
Class {
	#name : 'MenuLineMorph',
	#superclass : 'BoxMorph',
	#category : 'Morphic-Menus',
	#package : 'Morphic',
	#tag : 'Menus'
}

{ #category : 'new-morph participation' }
MenuLineMorph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu"

	^ false
]

{ #category : 'initialization' }
MenuLineMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 2
]

{ #category : 'drawing' }
MenuLineMorph >> drawOn: aCanvas  [
	| baseColor |
	baseColor := owner color.
	aCanvas
		fillRectangle: (`0@0` corner: extent x @ (extent y / 2))
		color: baseColor twiceDarker.
			
	aCanvas
		fillRectangle: (0 @ (extent y / 2) corner: extent)
		color: baseColor twiceLighter
]

{ #category : 'initialization' }
MenuLineMorph >> initialize [
	super initialize.
	extent := `50 @ 2`
]

{ #category : 'layout' }
MenuLineMorph >> minItemWidth [
	^ 0

]

{ #category : 'geometry' }
MenuLineMorph >> minimumExtent [

	^`10@2`
]
