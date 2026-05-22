"
LayoutSizeSpecs are the basis for the LinearLayout mechanism. Any Morph can be given a LayoutSizeSpecs, but in order to honor it, its owner must be a LinearLayout.

A LayoutSpec specifies how a morph wants to be layed out. In particular, a LayoutSizeSpec It can specify either a fixed width or a fraction of some available owner width. Same goes for height. If a fraction is specified, a minimum extent is also possible.


Alternatives:
	- proportionalWidth notNil, fixedWidth notNil	->		Use fraction of available space, take fixedWidth as minimum desired width
	- proportionalWidth isNil, fixedWidth isNil		->		Use current morph width
	- proportionalWidth isNil, fixedWidth notNil	->		Use fixedWidth
	- proportionalWidth notNil, fixedWidth isNil	->		NOT VALID

Same goes for proportionalHeight and fixedHeight

"
Class {
	#name : 'LayoutSizeSpec',
	#superclass : 'LayoutSpec',
	#instVars : [
		'fixedWidth',
		'fixedHeight',
		'offAxisEdgeWeight',
		'proportionalWidth',
		'proportionalHeight'
	],
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'instance creation' }
LayoutSizeSpec class >> fixedExtent: aPoint [
	^ self fixedWidth: aPoint x fixedHeight: aPoint y
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> fixedHeight: aNumber [
	"Will use all available width"
	^self new fixedHeight: aNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> fixedWidth: aNumber [
	"Will use all available height"
	^self new fixedWidth: aNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> fixedWidth: aNumber fixedHeight: otherNumber [
	^ LayoutSizeSpec new
		fixedWidth: aNumber;
		fixedHeight: otherNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> fixedWidth: aNumber fixedHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber [
	^ LayoutSizeSpec new
		fixedWidth: aNumber;
		fixedHeight: otherNumber;
		offAxisEdgeWeight: aSymbolOrNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> fixedWidth: aNumber proportionalHeight: otherNumber [

	^self new
		fixedWidth: aNumber;
		proportionalHeight: otherNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> fixedWidth: aNumber proportionalHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber [

	^self new
		fixedWidth: aNumber;
		proportionalHeight: otherNumber;
		offAxisEdgeWeight: aSymbolOrNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> keepMorphExtent [
	^self new
		useMorphWidth;
		useMorphHeight
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> morphHeightFixedWidth: aNumber [

	^self new
		fixedWidth: aNumber;
		useMorphHeight
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> morphHeightProportionalWidth: aNumber [

	^self new
		proportionalWidth: aNumber;
		useMorphHeight
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> morphWidthFixedHeight: aNumber [

	^self new
		useMorphWidth;
		fixedHeight: aNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> morphWidthProportionalHeight: aNumber [

	^self new
		useMorphWidth;
		proportionalHeight: aNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> proportionalHeight: aNumber [
	"Will use all available width"
	^self new proportionalHeight: aNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> proportionalWidth: aNumber [
	"Will use all available height"
	^self new proportionalWidth: aNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> proportionalWidth: aNumber fixedHeight: otherNumber [

	^self new
		proportionalWidth: aNumber;
		fixedHeight: otherNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> proportionalWidth: aNumber fixedHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber [

	^self new
		proportionalWidth: aNumber;
		fixedHeight: otherNumber;
		offAxisEdgeWeight: aSymbolOrNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> proportionalWidth: aNumber proportionalHeight: otherNumber [

	^self new
		proportionalWidth: aNumber;
		proportionalHeight: otherNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> proportionalWidth: aNumber proportionalHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber [

	^self new
		proportionalWidth: aNumber;
		proportionalHeight: otherNumber;
		offAxisEdgeWeight: aSymbolOrNumber
]

{ #category : 'instance creation' }
LayoutSizeSpec class >> useAll [
	"Use all available space"
	^self new
]

{ #category : 'accessing' }
LayoutSizeSpec >> fixedHeight [
	^proportionalHeight isNil ifTrue: [fixedHeight ifNil: [morph extentInOwner y]] ifFalse: [ 0 ]
]

{ #category : 'accessing' }
LayoutSizeSpec >> fixedHeight: aNumber [
	"aNumber is taken as the fixed height to use.
	No proportional part."
	fixedHeight := aNumber.
	proportionalHeight := nil.
	self updateContainingLayout.
]

{ #category : 'layout' }
LayoutSizeSpec >> fixedOrMinimumLayoutHeight [
	^fixedHeight ifNil: [ morph extentInOwner y ]
]

{ #category : 'layout' }
LayoutSizeSpec >> fixedOrMinimumLayoutWidth [
	^fixedWidth ifNil: [ morph extentInOwner x ]
]

{ #category : 'accessing' }
LayoutSizeSpec >> fixedOrMorphHeight: aNumber [
	"aNumber is taken as the fixed height to use.
	No proportional part."
	fixedHeight
		ifNotNil: [ fixedHeight := aNumber ]
		ifNil: [ fixedHeight := aNumber.
			  morph heightInOwner: aNumber 
		].
	proportionalHeight := nil
]

{ #category : 'accessing' }
LayoutSizeSpec >> fixedOrMorphWidth: aNumber [
	"aNumber is taken as the fixed width to use.
	No proportional part."
	fixedWidth
		ifNotNil: [ fixedWidth := aNumber ]
		ifNil: [ fixedWidth := aNumber.
			morph widthInOwner: aNumber ].
	proportionalWidth := nil.
	self updateContainingLayout.
]

{ #category : 'accessing' }
LayoutSizeSpec >> fixedWidth [
	^proportionalWidth isNil ifTrue: [fixedWidth ifNil: [morph extentInOwner x]] ifFalse: [ 0 ]
]

{ #category : 'accessing' }
LayoutSizeSpec >> fixedWidth: aNumber [
	"aNumber is taken as the fixed width to use.
	No proportional part."
	fixedWidth := aNumber.
	proportionalWidth := nil.
	self updateContainingLayout.
]

{ #category : 'initialization' }
LayoutSizeSpec >> initialize [
	"Just some reasonable defaults, use all available space"
	offAxisEdgeWeight := 0.5.
	fixedWidth := 0.
	fixedHeight := 0.
	proportionalWidth := 1.0.
	proportionalHeight := 1.0
]

{ #category : 'testing' }
LayoutSizeSpec >> isLayoutSizeSpec [
	^true
]

{ #category : 'testing' }
LayoutSizeSpec >> isProportionalHeight [
	"Zero actuaqlly means fixed height."

	^ proportionalHeight notNil and: [ proportionalHeight isZero not ].
]

{ #category : 'testing' }
LayoutSizeSpec >> isProportionalWidth [
	"Zero actuaqlly means fixed width."

	^ proportionalWidth notNil and: [ proportionalWidth isZero not ].
]

{ #category : 'layout' }
LayoutSizeSpec >> minimumShrinkHeight [
	"Generally prefer asking the morph itself!"

	proportionalHeight ifNil: [ ^0 ].
	^fixedHeight ifNil: [0]
]

{ #category : 'layout' }
LayoutSizeSpec >> minimumShrinkWidth [
	"Generally prefer asking the morph itself!"

	proportionalWidth ifNil: [ ^0 ].
	^fixedWidth ifNil: [0]
]

{ #category : 'layout' }
LayoutSizeSpec >> minimumSpecExtent [
	"If fixedHeight is not nil, use it.  
	 If fixdHeight and propostionlHeight are nil, use morphHeight"

	| meo h w |
	meo := morph extentInOwner.
	w := fixedWidth ifNil: [ proportionalWidth ifNotNil: [ 0 ] ifNil: [  meo x ] ].
	h := fixedHeight ifNil: [ proportionalHeight ifNotNil: [ 0 ] ifNil: [ meo y ] ].
	^w@h
]

{ #category : 'accessing' }
LayoutSizeSpec >> offAxisEdgeWeight [
	^offAxisEdgeWeight ifNil: [ 0.5 ]
]

{ #category : 'accessing' }
LayoutSizeSpec >> offAxisEdgeWeight: aSymbolOrNumber [
	"A LayoutSpec may indicate a weighting perpendicular to the Layout Axis.   
	
	 This is the LayoutSpec's offAxisEdgeWeight which is between 0.0 and 1.0.

	As with LayoutSpec's a symbol may be used.

	If in a Row (preferred): { #rowTop (0.0), #center (0.5), #rowBottom (1.0)}

	If in a Column (preferred): { #columnLeft (0.0), #center (0.5), #columnRight (1.0) }

	Also accepted: { #leftOrTop (0.0), #center (0.5), #rightOrBottom (1.0) }
	"
	| edgeWeight |
	edgeWeight := (aSymbolOrNumber is: #Number)
		ifTrue: [ (aSymbolOrNumber between: 0.0 and: 1.0)
			ifTrue: [ aSymbolOrNumber ] 
			ifFalse: [self error: 'axisEdgeWeight ', aSymbolOrNumber printString, ' is out of range 0.0 to 1.0']
		]
		ifFalse: [ aSymbolOrNumber
			caseOf: {
				[ #leftOrTop ] 			-> [ 0.0 ].
				[ #rowTop ] 			-> [ 0.0 ].
				[ #columnLeft ]			-> [ 0.0 ].
				[ #center ] 				-> [ 0.5 ].
				[ #rightOrBottom ]		-> [ 1.0 ].
				[ #rowBottom ] 		-> [ 1.0 ].
				[ #columnRight ] 		-> [ 1.0 ]
			}
			otherwise: [ self error: 'bad offAxisEdgeWeight specifier: ', aSymbolOrNumber  printString ]
		].
	offAxisEdgeWeight := edgeWeight.
	self updateContainingLayout.
]

{ #category : 'private' }
LayoutSizeSpec >> privateFixedHeight [

	^ fixedHeight
]

{ #category : 'private' }
LayoutSizeSpec >> privateFixedWidth [

	^ fixedWidth
]

{ #category : 'private' }
LayoutSizeSpec >> privateProportionalHeight [
	"deprecated
	Please use #proportionalHeight"

	^ proportionalHeight
]

{ #category : 'private' }
LayoutSizeSpec >> privateProportionalWidth [
	"deprecated
	Please use #proportionalWidth"

	^ proportionalWidth 
]

{ #category : 'accessing' }
LayoutSizeSpec >> proportionalHeight [
	^proportionalHeight
]

{ #category : 'accessing' }
LayoutSizeSpec >> proportionalHeight: aNumber [
	^self proportionalHeight: aNumber minimum: 0.0
]

{ #category : 'accessing' }
LayoutSizeSpec >> proportionalHeight: aNumberOrNil minimum: otherNumberOrNil [
	"Alternatives: same as in #proportionalWidth:minimum:, see comment there"
	proportionalHeight := aNumberOrNil.
	fixedHeight := otherNumberOrNil.
	self updateContainingLayout.
]

{ #category : 'layout' }
LayoutSizeSpec >> proportionalLayoutHeight [

	^ proportionalHeight ifNil: [ 0 ]
]

{ #category : 'layout' }
LayoutSizeSpec >> proportionalLayoutWidth [

	^ proportionalWidth ifNil: [ 0 ]
]

{ #category : 'accessing' }
LayoutSizeSpec >> proportionalWidth [
	^proportionalWidth
]

{ #category : 'accessing' }
LayoutSizeSpec >> proportionalWidth: aNumber [
	^self proportionalWidth: aNumber minimum: 0
]

{ #category : 'accessing' }
LayoutSizeSpec >> proportionalWidth: aNumberOrNil minimum: otherNumberOrNil [
	"Alternatives:
		- proportionalWidth notNil, fixedWidth notNil 	->		Use fraction of available space, take fixedWidth as minimum desired width
		- proportionalWidth isNil, fixedWidth isNil  		->		Use current morph width
		- proportionalWidth isNil, fixedWidth notNil 	->		Use fixedWidth
		- proportionalWidth notNil, fixedWidth isNil 	->		NOT VALID"
	proportionalWidth := aNumberOrNil.
	fixedWidth := otherNumberOrNil.
	self updateContainingLayout.
]

{ #category : 'accessing' }
LayoutSizeSpec >> setProportionalHeight: aNumberOrNil [
	"Alternatives: same as in #proportionalWidth:minimum:, see comment there"
	proportionalHeight := aNumberOrNil.
	self updateContainingLayout.
]

{ #category : 'accessing' }
LayoutSizeSpec >> setProportionalWidth: aNumberOrNil [
	"Alternatives:
		- proportionalWidth notNil, fixedWidth notNil	->		Use fraction of available space, take fixedWidth as minimum desired width
		- proportionalWidth isNil, fixedWidth isNil		->		Use current morph width
		- proportionalWidth isNil, fixedWidth notNil	->		Use fixedWidth
		- proportionalWidth notNil, fixedWidth isNil	->		NOT VALID"
	proportionalWidth := aNumberOrNil.
	self updateContainingLayout.
]

{ #category : 'accessing' }
LayoutSizeSpec >> useMorphHeight [
	"Do not attempt to layout height. Use current morph height if at all possible."
	fixedHeight := nil.
	proportionalHeight := nil.
	self updateContainingLayout.
]

{ #category : 'accessing' }
LayoutSizeSpec >> useMorphWidth [
	"Do not attempt to layout width. Use current morph width if at all possible."
	fixedWidth := nil.
	proportionalWidth := nil.
	self updateContainingLayout.
]

{ #category : 'testing' }
LayoutSizeSpec >> usesMorphExtent [
	"Does not attempt to layout width or height. Uses current morph extent."

	^fixedWidth isNil and: [ proportionalWidth isNil 
		and: [ fixedHeight isNil and: [ proportionalHeight isNil ]]]
]

{ #category : 'testing' }
LayoutSizeSpec >> usesMorphHeight [
	"Does not attempt to layout height. Uses current morph height."

	^fixedHeight isNil and: [ proportionalHeight isNil ]
]

{ #category : 'testing' }
LayoutSizeSpec >> usesMorphWidth [
	"Does not attempt to layout width. Uses current morph width."

	^fixedWidth isNil and: [ proportionalWidth isNil ]
]
