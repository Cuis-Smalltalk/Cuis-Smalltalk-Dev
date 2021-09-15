'From Cuis 5.0 [latest update: #4206] on 4 June 2020 at 6:40:09 pm'!

!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 6/4/2020 18:28:12'!
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

!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 6/4/2020 18:27:53'!
separation: aNumberOrPoint
	separation _ aNumberOrPoint.
	self layoutSubmorphs ! !


!LayoutSpec class methodsFor: 'deprecated' stamp: 'KenD 6/4/2020 18:39:02'!
fixedWidth: aNumber fixedHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self flag: #deprecated. "use fixedWidth:fixedHeight:offAxisEdgeWeight:"
	^ LayoutSpec new
		fixedWidth: aNumber;
		fixedHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'deprecated' stamp: 'KenD 6/4/2020 18:40:09'!
fixedWidth: aNumber proportionalHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self flag: #deprecated. "use fixedWidth:proportionalHeight:offAxisEdgeWeight:"
	^self new
		fixedWidth: aNumber;
		proportionalHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'deprecated' stamp: 'KenD 6/4/2020 18:41:13'!
proportionalWidth: aNumber fixedHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self flag: #deprecated. "proportionalWidth:fixedHeight:offAxisEdgeWeight:"
	^self new
		proportionalWidth: aNumber;
		fixedHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'deprecated' stamp: 'KenD 6/4/2020 18:42:09'!
proportionalWidth: aNumber proportionalHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self flag: #deprecated. "use proportionalWidth:proportionalHeight:offAxisEdgeWeight:
"
	^self new
		proportionalWidth: aNumber;
		proportionalHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !

!methodRemoval: LayoutSpec #minimumLayoutHeight stamp: 'KenD 6/4/2020 18:22:41'!
LayoutSpec removeSelector: #minimumLayoutHeight!
!methodRemoval: LayoutSpec #heightFor: stamp: 'KenD 6/4/2020 18:32:49'!
LayoutSpec removeSelector: #heightFor:!
!methodRemoval: LayoutSpec #widthFor: stamp: 'KenD 6/4/2020 18:33:04'!
LayoutSpec removeSelector: #widthFor:!
!methodRemoval: LayoutSpec #fixedOrMorphWidth stamp: 'KenD 6/4/2020 18:26:55'!
LayoutSpec removeSelector: #fixedOrMorphWidth!
!methodRemoval: LayoutSpec #heightForComfortable: stamp: 'KenD 6/4/2020 18:26:20'!
LayoutSpec removeSelector: #heightForComfortable:!
!methodRemoval: LayoutSpec #minimumLayoutWidth stamp: 'KenD 6/4/2020 18:22:35'!
LayoutSpec removeSelector: #minimumLayoutWidth!
!methodRemoval: LayoutSpec #widthForComfortable: stamp: 'KenD 6/4/2020 18:33:09'!
LayoutSpec removeSelector: #widthForComfortable:!
!methodRemoval: Morph #minLayoutWidth stamp: 'KenD 6/4/2020 18:34:55'!
Morph removeSelector: #minLayoutWidth!
!methodRemoval: Morph #minLayoutHeight stamp: 'KenD 6/4/2020 18:34:51'!
Morph removeSelector: #minLayoutHeight!
