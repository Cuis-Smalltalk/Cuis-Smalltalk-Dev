'From Cuis 6.0 [latest update: #5969] on 7 August 2023 at 10:44:30 am'!

!LayoutSpec methodsFor: 'private' stamp: 'KenD 8/7/2023 10:42:22'!
updateContainingLayout
	morph ifNotNil: [
		morph owner ifNotNil: [ :layoutMorph |
			layoutMorph layoutSubmorphs ]].! !


!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 8/4/2023 08:11:42'!
fixedHeight: aNumber
	"aNumber is taken as the fixed height to use.
	No proportional part."
	fixedHeight := aNumber.
	proportionalHeight := nil.
	self updateContainingLayout.! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 8/4/2023 08:11:58'!
fixedOrMorphWidth: aNumber
	"aNumber is taken as the fixed width to use.
	No proportional part."
	fixedWidth
		ifNotNil: [ fixedWidth := aNumber ]
		ifNil: [ fixedWidth := aNumber.
			morph morphWidthInOwner: aNumber ].
	proportionalWidth := nil.
	self updateContainingLayout.! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 8/4/2023 08:12:04'!
fixedWidth: aNumber
	"aNumber is taken as the fixed width to use.
	No proportional part."
	fixedWidth := aNumber.
	proportionalWidth := nil.
	self updateContainingLayout.! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 8/4/2023 08:12:19'!
offAxisEdgeWeight: aSymbolOrNumber
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
				[ #leftOrTop ] -> [ 0.0 ].
				[ #rowTop ] -> [ 0.0 ].
				[ #columnLeft ] -> [ 0.0 ].
				[ #center ] -> [ 0.5 ].
				[ #rightOrBottom ] -> [ 1.0 ].
				[ #rowBottom ] -> [ 1.0 ].
				[ #columnRight ] -> [ 1.0 ]
			}
			otherwise: [ self error: 'bad offAxisEdgeWeight specifier: ', aSymbolOrNumber  printString ]
		].
	offAxisEdgeWeight := edgeWeight.
	self updateContainingLayout.! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 8/4/2023 08:12:44'!
proportionalHeight: aNumberOrNil minimum: otherNumberOrNil
	"Alternatives: same as in #proportionalWidth:minimum:, see comment there"
	proportionalHeight := aNumberOrNil.
	fixedHeight := otherNumberOrNil.
	self updateContainingLayout.! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 8/4/2023 08:12:50'!
proportionalWidth: aNumberOrNil minimum: otherNumberOrNil
	"Alternatives:
		- proportionalWidth notNil, fixedWidth notNil 	->		Use fraction of available space, take fixedWidth as minimum desired width
		- proportionalWidth isNil, fixedWidth isNil  	->		Use current morph width
		- proportionalWidth isNil, fixedWidth notNil 	->		Use fixedWidth
		- proportionalWidth notNil, fixedWidth isNil 	->		NOT VALID"
	proportionalWidth := aNumberOrNil.
	fixedWidth := otherNumberOrNil.
	self updateContainingLayout.! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 8/4/2023 08:13:01'!
setProportionalHeight: aNumberOrNil
	"Alternatives: same as in #proportionalWidth:minimum:, see comment there"
	proportionalHeight := aNumberOrNil.
	self updateContainingLayout.! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 8/4/2023 08:13:07'!
setProportionalWidth: aNumberOrNil
	"Alternatives:
		- proportionalWidth notNil, fixedWidth notNil	->		Use fraction of available space, take fixedWidth as minimum desired width
		- proportionalWidth isNil, fixedWidth isNil		->		Use current morph width
		- proportionalWidth isNil, fixedWidth notNil		->		Use fixedWidth
		- proportionalWidth notNil, fixedWidth isNil		->		NOT VALID"
	proportionalWidth := aNumberOrNil.
	self updateContainingLayout.! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 8/4/2023 08:13:15'!
useMorphHeight
	"Do not attempt to layout height. Use current morph height if at all possible."
	fixedHeight := nil.
	proportionalHeight := nil.
	self updateContainingLayout.! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 8/4/2023 08:13:21'!
useMorphWidth
	"Do not attempt to layout width. Use current morph width if at all possible."
	fixedWidth := nil.
	proportionalWidth := nil.
	self updateContainingLayout.! !

