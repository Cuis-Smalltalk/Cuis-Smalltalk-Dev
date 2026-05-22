"
Read 'FormLayout examples' in LayoutMorph class.
"
Class {
	#name : 'FormLayoutMorph',
	#superclass : 'LayoutMorph',
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'submorphs-add/remove' }
FormLayoutMorph >> addMorph: aMorph layoutSpec: aLayoutSpec [
	"Add a submorph, with aLayoutSpec"
	
	aLayoutSpec isLayoutEdgesSpec ifFalse: [ self error: 'Only LayoutEdgesSpec can be used in FormLayoutMorphs' ].
	super addMorph: aMorph layoutSpec: aLayoutSpec
]

{ #category : 'aux' }
FormLayoutMorph >> defaultLayoutSpecFor: aMorph [
	^aMorph defaultLayoutEdgesSpec
]

{ #category : 'initialization' }
FormLayoutMorph >> initialize [

	super initialize.
	layoutObject := FormLayout new.
]

{ #category : 'testing' }
FormLayoutMorph >> is: aSymbol [
	^ aSymbol == #FormLayoutMorph or: [ super is: aSymbol ]
]

{ #category : 'testing' }
FormLayoutMorph >> isLayoutSpecOk: aLayoutSpecOrNil [
	"Answer whether the argument can work with us."
	
	^aLayoutSpecOrNil notNil and: [aLayoutSpecOrNil isLayoutEdgesSpec]
]

{ #category : 'geometry' }
FormLayoutMorph >> minimumExtent [
	"Ojo!"
	^100@100
]
