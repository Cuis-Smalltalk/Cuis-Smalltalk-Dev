'From Cuis7.3 [latest update: #7029] on 28 January 2025 at 3:30:07 pm'!

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 1/28/2025 15:08:05'!
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
	self updateContainingLayout.! !

!LayoutSpec methodsFor: 'private' stamp: 'jmv 1/28/2025 15:10:24'!
updateContainingLayout
	| owningLayout |
	morph ifNotNil: [
		owningLayout := morph owner.
		(owningLayout is: #LayoutMorph) ifTrue:
			[ owningLayout layoutSubmorphs ]].! !

