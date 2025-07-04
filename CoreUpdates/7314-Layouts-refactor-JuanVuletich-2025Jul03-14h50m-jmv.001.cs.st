'From Cuis7.5 [latest update: #7313] on 3 July 2025 at 2:56:12 pm'!

!ColumnLayout methodsFor: 'accessing' stamp: 'jmv 7/3/2025 14:55:32'!
axisEdgeWeight: aSymbolOrNumber
	"Along the Major or Layout Axis, subMorphs may be attracted toward one side (0.0) or the other (1.0).  This is the LayoutMorph's axisEdgeWeight.  A numeric value is between 0.0 and 1.0

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
			"vertical => Column"
			edgeWeight := aSymbolOrNumber
				caseOf: {
					[ #columnTop ] -> [ 0.0 ].
					[ #center ] -> [ 0.5 ].
					[ #columnBottom ] -> [ 1.0 ]
				}
				otherwise: [ self error: 'bad axisEdgeWeight for Column: ', aSymbolOrNumber printString ].
		].
	super axisEdgeWeight: edgeWeight.! !


!RowLayout methodsFor: 'accessing' stamp: 'jmv 7/3/2025 14:54:50'!
axisEdgeWeight: aSymbolOrNumber
	"Along the Major or Layout Axis, subMorphs may be attracted toward one side (0.0) or the other (1.0).  This is the LayoutMorph's axisEdgeWeight.  A numeric value is between 0.0 and 1.0

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
			"Row"
			edgeWeight := aSymbolOrNumber
				caseOf: {
					[ #rowLeft ] 	-> [ 0.0 ].
					[ #center ] 		-> [ 0.5 ].
					[ #rowRight ] 	-> [ 1.0 ]
				}
				otherwise: [ self error: 'bad axisEdgeWeight for Row: ', aSymbolOrNumber printString ].
		].
	super axisEdgeWeight: edgeWeight.! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/3/2025 14:53:45'!
axisEdgeWeight: aSymbolOrNumber
	"Along the Major or Layout Axis, subMorphs may be attracted toward one side (0.0) or the other (1.0).  This is the LayoutMorph's axisEdgeWeight.  A numeric value is between 0.0 and 1.0

	If a Row, one can specify this weight symbolically as
	  { #rowLeft (0.0), #center (0.5), #rowRight (1.0)}

	If a Column, one can specify the weight symbolically as:
	  { #columnTop (0.0), #center (0.5), #columnBottom (1.0) }"
"	
	self axisEdgeWeight: #rowLeft.	-- axis must be horizontal 
	self axisEdgeWeight: #columnTop.	-- axis must be vertical
"
	layout axisEdgeWeight: aSymbolOrNumber.
	self layoutSubmorphs! !


!ColumnLayout reorganize!
('accessing' axisEdgeWeight:)
('initialization' initialize)
('layout' heightsFor:within:minLayoutExtentCache: layoutMorphs:in: offAxisWidthFor:within:minLayoutExtentCache: offAxisXOffsetFor:within:)
('testing' isColumn)
!


!RowLayout reorganize!
('accessing' axisEdgeWeight:)
('initialization' initialize)
('layout' layoutMorphs:in: offAxisHeightFor:within:minLayoutExtentCache: offAxisYOffsetFor:within: widthsFor:within:minLayoutExtentCache:)
('testing' isRow)
!

