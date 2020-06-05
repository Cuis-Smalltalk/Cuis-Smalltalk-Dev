'From Cuis 5.0 [latest update: #4202] on 3 June 2020 at 3:28:27 pm'!

!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 6/2/2020 19:15:47'!
axisEdgeWeight: aSymbolOrNumber
	"Along the Major or Layout Axis, subMorphs may be attracted toward one side (0.0) or the other (1.0).  This is the 	 	 LayoutMorph's axisEdgeWeight.  A numeric value is between 0.0 and 1.0

	If a Row, one can specify this weight symbolically as
	  { #rowLeft (0.0), #center (0.5), #rowRight (1.0)}

	If a Column, one can specify the weight symbolically as:
	  { #columnTop (0.0), #center (0.5), #columnBottom (1.0) }"
"	
	self axisEdgeWeight: #rowLeft.	-- axis must be horizontal 
	self axisEdgeWeight: #columnTop.	-- axis must be vertical
"
	| edgeWeight |
	(aSymbolOrNumber is: #Number)
		ifTrue: [ (aSymbolOrNumber between: 0.0 and: 1.0)
			ifTrue: [edgeWeight := aSymbolOrNumber ] 
			ifFalse: [self error: 'axisEdgeWeight ', aSymbolOrNumber printString, ' is out of range 0.0 to 1.0']
		]
		ifFalse: [
			(self direction = #horizontal) 
				ifTrue: [ "Row"
					edgeWeight := aSymbolOrNumber
						caseOf: {
							[ #rowLeft ] -> [ 0.0 ].
							[ #center ] -> [ 0.5 ].
							[ #rowRight ] -> [ 1.0 ]
						}
						otherwise: [ self error: 'bad axisEdgeWeight for Row: ', aSymbolOrNumber printString ].
				]
				ifFalse: [ "vertical => Column"
					edgeWeight := aSymbolOrNumber
						caseOf: {
							[ #columnTop ] -> [ 0.0 ].
							[ #center ] -> [ 0.5 ].
							[ #columnBottom ] -> [ 1.0 ]
						}
						otherwise: [ self error: 'bad axisEdgeWeight for Column: ', aSymbolOrNumber printString ].
				]
		].
	axisEdgeWeight := edgeWeight.
	self layoutSubmorphs! !

!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 6/2/2020 19:16:08'!
separation: aNumberOrPoint
	separation _ aNumberOrPoint.
	self layoutSubmorphs! !

!methodRemoval: LayoutSpec #widthFor: stamp: 'KenD 6/3/2020 15:03:04'!
LayoutSpec removeSelector: #widthFor:!
!methodRemoval: LayoutSpec #widthForComfortable: stamp: 'KenD 6/3/2020 15:02:40'!
LayoutSpec removeSelector: #widthForComfortable:!
!methodRemoval: LayoutMorph #desiredLayoutWidth stamp: 'KenD 6/2/2020 19:17:14'!
LayoutMorph removeSelector: #desiredLayoutWidth!
