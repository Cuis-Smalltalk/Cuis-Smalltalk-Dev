"
The morph that displays the list in a PluggableScrollPane
"
Class {
	#name : 'InnerPluggableMorph',
	#superclass : 'ColoredBoxMorph',
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
InnerPluggableMorph class >> includeInNewMorphMenu [
	"Not to be instantiated from the menu"
	^ false
]
