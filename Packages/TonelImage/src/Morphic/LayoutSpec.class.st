Class {
	#name : 'LayoutSpec',
	#superclass : 'Object',
	#instVars : [
		'morph'
	],
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'for LinearLayouts' }
LayoutSpec class >> fixedExtent: aPoint [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec fixedExtent: aPoint
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> fixedHeight: aNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec fixedHeight: aNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> fixedWidth: aNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec fixedWidth: aNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> fixedWidth: aNumber fixedHeight: otherNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec fixedWidth: aNumber fixedHeight: otherNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> fixedWidth: aNumber fixedHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec fixedWidth: aNumber fixedHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> fixedWidth: aNumber proportionalHeight: otherNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec fixedWidth: aNumber proportionalHeight: otherNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> fixedWidth: aNumber proportionalHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec fixedWidth: aNumber proportionalHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> keepMorphExtent [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec keepMorphExtent
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> morphHeightFixedWidth: aNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec morphHeightFixedWidth: aNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> morphHeightProportionalWidth: aNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec morphHeightProportionalWidth: aNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> morphWidthFixedHeight: aNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec morphWidthFixedHeight: aNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> morphWidthProportionalHeight: aNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec morphWidthProportionalHeight: aNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> proportionalHeight: aNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec proportionalHeight: aNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> proportionalWidth: aNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec proportionalWidth: aNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> proportionalWidth: aNumber fixedHeight: otherNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec proportionalWidth: aNumber fixedHeight: otherNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> proportionalWidth: aNumber fixedHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec proportionalWidth: aNumber fixedHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> proportionalWidth: aNumber proportionalHeight: otherNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec proportionalWidth: aNumber proportionalHeight: otherNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> proportionalWidth: aNumber proportionalHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec proportionalWidth: aNumber proportionalHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber
]

{ #category : 'for LinearLayouts' }
LayoutSpec class >> useAll [
	"Instance creation.
	To be used with LinearLayouts."
	^LayoutSizeSpec useAll
]

{ #category : 'testing' }
LayoutSpec >> isLayoutEdgesSpec [
	^false
]

{ #category : 'testing' }
LayoutSpec >> isLayoutSizeSpec [
	^false
]

{ #category : 'accessing' }
LayoutSpec >> morph [
	"For #showHalo"
	^ morph
]

{ #category : 'accessing' }
LayoutSpec >> morph: aMorph [
	morph := aMorph
]

{ #category : 'private' }
LayoutSpec >> updateContainingLayout [
	| owningLayoutMorph |
	morph ifNotNil: [
		owningLayoutMorph := morph owner.
		(owningLayoutMorph is: #LayoutMorph) ifTrue:
			[ owningLayoutMorph layoutNeeded: true ]].
]
