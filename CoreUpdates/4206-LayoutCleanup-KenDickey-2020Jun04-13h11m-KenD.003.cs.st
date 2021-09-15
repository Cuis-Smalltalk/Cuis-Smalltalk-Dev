'From Cuis 5.0 [latest update: #4204] on 4 June 2020 at 2:26:09 pm'!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
RectangleLikeMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation padding doAdoptWidgetsColor axisEdgeWeight '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
!classDefinition: #LayoutSpec category: #'Morphic-Layouts'!
Object subclass: #LayoutSpec
	instanceVariableNames: 'morph fixedWidth fixedHeight minorDirectionPadding proportionalWidth proportionalHeight offAxisEdgeWeight '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 6/4/2020 13:11:42'!
axisEdgeWeight

	^ axisEdgeWeight! !

!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 6/4/2020 13:11:42'!
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
	"self layoutSubmorphs"! !

!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 6/4/2020 13:13:50'!
direction: horizOrVert

	direction := horizOrVert! !

!LayoutMorph methodsFor: 'initialization' stamp: 'KenD 6/4/2020 14:22:33'!
initialize
	super initialize.
	separation _ 0.
	axisEdgeWeight _ 0.0.
	doAdoptWidgetsColor _ false! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 6/4/2020 14:25:55'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs sumOfWidths theSeparation widthsArray 
	widthToAllocate leftOver nextX | 
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	visibleSubmorphs := self submorphsToLayout reversed.  "Display Order"
	theSeparation := self xSeparation.

	widthToAllocate := (boundsForLayout width - ((visibleSubmorphs size + 1) * theSeparation)).
	widthsArray := self widthsFor: visibleSubmorphs within: widthToAllocate.
	sumOfWidths := widthsArray sum: [ :w | w ] .
	leftOver := widthToAllocate - sumOfWidths.
	nextX := boundsForLayout origin x
				+ theSeparation 
				+ (leftOver * (self axisEdgeWeight ifNil: [0.0])). "first X, edge shifted"
"Transcript log: 'first X=', nextX asString, ' leftOver=', leftOver asString; cr.
"	1 to: visibleSubmorphs size do: [ :index | | sm smX smWidth heightAndY newExtent |
		sm := visibleSubmorphs at: index.
		smX := nextX.
		smWidth := widthsArray at: index.
		nextX := smX + smWidth + theSeparation.
		heightAndY := self offHorizontalMetricFor: sm withinExtent: boundsForLayout.
		sm morphPosition: smX @ (heightAndY y).
		newExtent :=  smWidth @ (heightAndY x).
		(sm morphExtent = newExtent) ifFalse: [ sm morphExtent: newExtent ].
	]
! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 6/4/2020 14:26:19'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs sumOfHeights theSeparation heightsArray 
	heightToAllocate leftOver nextY | 
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	visibleSubmorphs := self submorphsToLayout reversed.  "Display Order"

	theSeparation := self ySeparation.
	heightToAllocate := (boundsForLayout height - ((visibleSubmorphs size + 1) * theSeparation)).
	heightsArray := self heightsFor: visibleSubmorphs within: heightToAllocate.
	sumOfHeights := heightsArray sum: [ :w | w ] .
	leftOver := heightToAllocate - sumOfHeights.
	nextY := boundsForLayout origin y
				+ theSeparation 
				+ (leftOver * (self axisEdgeWeight ifNil: [0.5])). "first Y, edge shifted"
"Transcript log: 'first Y=', nextY asString, ' leftOver=', leftOver asString; cr.
"	1 to: visibleSubmorphs size do: [ :index | | sm smY smHeight xAndWidth newExtent |
		sm := visibleSubmorphs at: index.
		smY := nextY.
		smHeight := heightsArray at: index.
		nextY := smY + smHeight + theSeparation.
		xAndWidth := self offVerticalMetricFor: sm withinExtent: boundsForLayout.
		sm morphPosition: (xAndWidth x) @ smY.
		newExtent :=  (xAndWidth y) @ smHeight.
		(sm morphExtent = newExtent) ifFalse: [ sm morphExtent: newExtent ].
	]
! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 6/4/2020 14:21:05'!
offHorizontalMetricFor: aMorph withinExtent: boundsForLayout 
	"Answer a point: height@y for a single morph -- offAxis calculation for a Row"
	
	| spec minMorphHeight theSeparation proportionalHeight 
	availableForPropHeight actualPropHeight
	leftOver yPos |
	spec := aMorph layoutSpec.
	theSeparation := self ySeparation.
	minMorphHeight := aMorph minimumLayoutHeight.
	availableForPropHeight := (boundsForLayout height)  - (2 * theSeparation)..
	proportionalHeight := spec proportionalLayoutHeight min: 1.0.
	actualPropHeight := (availableForPropHeight * proportionalHeight)
							 max: minMorphHeight.
	leftOver := availableForPropHeight - actualPropHeight.
		
	yPos := boundsForLayout origin y
			+ theSeparation 
			+ (leftOver * (spec offAxisEdgeWeight ifNil: [0.5])). "first Y, edge shifted"

	^ actualPropHeight @ yPos
	
	! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 6/4/2020 14:21:30'!
offVerticalMetricFor: aMorph withinExtent: boundsForLayout
	"Answer a point:  x@width for a single morph -- offAxis calculation for a Column"

	| spec minMorphWidth theSeparation proportionalWidth 
 	availableForPropWidth actualPropWidth
	leftOver xPos |
	spec := aMorph layoutSpec.
	theSeparation := self xSeparation.
	minMorphWidth := aMorph minimumLayoutWidth.
	availableForPropWidth := (boundsForLayout width) - (2 * theSeparation).
	proportionalWidth := spec proportionalLayoutWidth min: 1.0.
	actualPropWidth := (availableForPropWidth * proportionalWidth)
						 	max: minMorphWidth.
	leftOver := availableForPropWidth - actualPropWidth.
	
	xPos := boundsForLayout origin x 
			+ theSeparation 
			+ (leftOver * (spec offAxisEdgeWeight ifNil: [0.5])). "first X, edge shifted"

	^ xPos @ actualPropWidth 
	! !

!LayoutMorph methodsFor: 'deprecated' stamp: 'KenD 6/4/2020 13:14:39'!
padding

	self flag: #deprecated. "use axisEdgeWeight"
	^ axisEdgeWeight! !

!LayoutMorph methodsFor: 'deprecated' stamp: 'KenD 6/4/2020 13:14:22'!
padding: aSymbolOrNumber
	"This sets how extra space is used when doing layout. For example, a column might have extra , unneded vertical space. #top means widgets are set close to the top, and extra space is at bottom. Conversely, #bottom means widgets are set close to the bottom, and extra space is at top. Valid values include #left and #right (for rows) and #center. Alternatively, any number between 0.0 and 1.0 might be used.
	self new padding: #center
	self new padding: 0.9
	"
	self flag: #deprecated. "use axisEdgeWeight:"
	axisEdgeWeight _ aSymbolOrNumber
		caseOf: {
			[ #top ] -> [ 0.0 ].
			[ #left ] -> [ 0.0 ].
			[ #center ] -> [ 0.5 ].
			[ #right ] -> [ 1.0 ].
			[ #bottom ] -> [ 1.0 ]
		}
		otherwise: [ aSymbolOrNumber ]! !


!LayoutSpec methodsFor: 'initialization' stamp: 'KenD 6/4/2020 13:11:57'!
initialize
	"Just some reasonable defaults, use all available space"
	offAxisEdgeWeight _ 0.5.
	fixedWidth _ 0.
	fixedHeight _ 0.
	proportionalWidth _ 1.0.
	proportionalHeight _ 1.0! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 6/4/2020 13:14:56'!
morph
	"For #showHalo"
	^ morph! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 6/4/2020 13:11:57'!
offAxisEdgeWeight
	^offAxisEdgeWeight! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 6/4/2020 13:11:57'!
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
	offAxisEdgeWeight _ edgeWeight! !

!LayoutSpec methodsFor: 'deprecated' stamp: 'KenD 6/4/2020 13:16:05'!
minorDirectionPadding
	self flag: #deprecated. "use offAxisEdgeWeight"
	^offAxisEdgeWeight! !

!LayoutSpec methodsFor: 'deprecated' stamp: 'KenD 6/4/2020 13:15:55'!
minorDirectionPadding: aSymbolOrNumber
	"This sets how padding is done in the secondary direction. For instance, if the owning morph is set in a row, the row will control horizontal layout. But if there is unused vertical space, it will be used according to this parameter. For instance, #top sets the owning morph at the top. Same for #bottom and #center. If the owner is contained in a column, #left, #center or #right should be used. Alternatively, any number between 0.0 and 1.0 can be used.
	self new minorDirectionPadding: #center
	self new minorDirectionPadding: 0.9
	"
	self flag: #deprecated. "use offAxisEdgeWeight:"
	offAxisEdgeWeight _ aSymbolOrNumber
		caseOf: {
			[ #top ] -> [ 0.0 ].
			[ #left ] -> [ 0.0 ].
			[ #center ] -> [ 0.5 ].
			[ #right ] -> [ 1.0 ].
			[ #bottom ] -> [ 1.0 ]
		}
		otherwise: [ aSymbolOrNumber ]! !

!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
RectangleLikeMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation axisEdgeWeight doAdoptWidgetsColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutMorph reorganize!
('accessing' addMorphFrontFromWorldPosition: adoptWidgetsColor: axisEdgeWeight axisEdgeWeight: direction direction: separation: xSeparation ySeparation)
('adjust' adjustBy:at: adjustHorizontallyBy:at: adjustVerticallyBy:at:)
('convenience methods' addAdjusterAndMorph:fixedHeight: addAdjusterAndMorph:layoutSpec: addAdjusterAndMorph:proportionalHeight: addAdjusterAndMorph:proportionalWidth: addMorph:fixedHeight: addMorph:fixedWidth: addMorph:proportionalHeight: addMorph:proportionalWidth: addMorphKeepMorphHeight: addMorphUseAll: addMorphs: addMorphs:widthProportionalTo:)
('geometry' desiredLayoutHeight desiredLayoutWidth minimumExtent)
('initialization' beColumn beRow defaultColor doAdoptWidgetsColor initialize)
('layout' heightsFor:within: layoutBounds layoutSubmorphs layoutSubmorphsHorizontallyIn: layoutSubmorphsVerticallyIn: offHorizontalMetricFor:withinExtent: offVerticalMetricFor:withinExtent: refreshExtent submorphsToLayout widthsFor:within:)
('submorphs-add/remove' addAdjusterMorph addMorph: addMorph:layoutSpec: addMorphFront: removedMorph:)
('testing' is: isColumn isRow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position:)
('layout in owner' layoutSpec)
('deprecated' padding padding:)
!

!classDefinition: #LayoutSpec category: #'Morphic-Layouts'!
Object subclass: #LayoutSpec
	instanceVariableNames: 'morph fixedWidth fixedHeight offAxisEdgeWeight proportionalWidth proportionalHeight'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutSpec reorganize!
('initialization' initialize)
('accessing' fixedHeight fixedHeight: fixedOrMorphHeight: fixedOrMorphWidth: fixedWidth fixedWidth: morph morph: offAxisEdgeWeight offAxisEdgeWeight: proportionalHeight: proportionalHeight:minimum: proportionalWidth: proportionalWidth:minimum: setProportionalHeight: setProportionalWidth: useMorphHeight useMorphWidth)
('layout' fixedOrMinimumLayoutHeight fixedOrMinimumLayoutWidth heightFor: heightForComfortable: minimumLayoutHeight minimumLayoutWidth minimumShrinkHeight minimumShrinkWidth minimumSpecExtent minimumSpecHeight minimumSpecWidth proportionaLayoutlHeight proportionalLayoutHeight proportionalLayoutWidth widthFor: widthForComfortable:)
('testing' isProportionalHeight isProportionalWidth usesMorphExtent usesMorphHeight usesMorphWidth)
('deprecated' fixedOrMorphHeight fixedOrMorphWidth minorDirectionPadding minorDirectionPadding:)
('private' privateFixedHeight privateFixedWidth privateProportionalHeight privateProportionalWidth)
!

