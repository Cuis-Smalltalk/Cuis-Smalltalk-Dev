"
Read 'FormLayout examples' in LayoutMorph class.
"
Class {
	#name : 'FormLayout',
	#superclass : 'Layout',
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'testing' }
FormLayout >> isFormLayout [
	^true
]

{ #category : 'layout' }
FormLayout >> layoutMorphs: morphs in: boundsForLayout [
	"Compute a new layout based on the given layout bounds."

	morphs do: [ :eachMorph | | ls r |
		eachMorph visible ifTrue: [
			ls := eachMorph layoutSpec.
			r := (boundsForLayout interpolateAt: ls topLeftEdgesWeight) + ls topLeftOffset
				corner: 
					(boundsForLayout interpolateAt: ls bottomRightEdgesWeight) + ls bottomRightOffset.
			eachMorph fitInto: r ]]
]
