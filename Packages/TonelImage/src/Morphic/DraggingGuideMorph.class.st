Class {
	#name : 'DraggingGuideMorph',
	#superclass : 'BoxMorph',
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
DraggingGuideMorph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu"

	^ false
]

{ #category : 'dropping/grabbing' }
DraggingGuideMorph >> dragCancel [
	self delete
]

{ #category : 'drawing' }
DraggingGuideMorph >> drawOn: aCanvas [
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		fillRectangle: self localBounds
		color: (Theme current listHighlightFocused: true).
]

{ #category : 'testing' }
DraggingGuideMorph >> is: aSymbol [

	^aSymbol == #DraggingGuideMorph or: [ super is: aSymbol ]
]

{ #category : 'dropping/grabbing' }
DraggingGuideMorph >> justDroppedInto: newOwnerMorph event: aDropEvent  [
	
	self delete.
	aDropEvent hand redrawNeeded.
]

{ #category : 'dropping/grabbing' }
DraggingGuideMorph >> wantsToBeDroppedInto: aMorph [
	"Return true if it's okay to drop the receiver into aMorph.
	This check is symmetric to #wantsDroppedMorph:event: to give both parties a chance of figuring out
	whether they like each other."

	^(aMorph is: #ListMorph)
		and: [ (self valueOfProperty: #dragSource) = (aMorph valueOfProperty: #acceptedDragSource) ]
]
