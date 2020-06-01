'From Cuis 5.0 [latest update: #4195] on 31 May 2020 at 3:50:57 pm'!

!LayoutAdjustingMorph commentStamp: '<historical>' prior: 0!
When added to the submorphs of a LayoutMorph, 
I am a way of directly adjusting layout width or height.

I take care of the user interactions and delegate the actual changes to my containing LayoutMorph.
See LayoutAdjustingMorph>>adjustOwnerAt:

The way adjustments are made differs between fixed and proportional Morphs.

If I am between two proportional Morphs, the relative Morph proportions on both sides of me are adjusted.

If the user is moving me between a fixed and a proportional morph, the fixed size is adjusted.

The general effect is that if between 2 proportional Morphs, the proportions are adjusted and the
user sees the boundary between two morphs change.  If one Morph is fixed, then the user sees
the proportional Morphs ALL adjust.  An "accordian like" effect.  This is NOT due to a change 
in relative proportions but just the proportional expansion or squeezing of the proportional 
Morphs themselves.

E.g. note LayoutMorph class>>example11 which has a fixed Morph at each end.
!


!WindowEdgeAdjustingMorph commentStamp: '<historical>' prior: 0!
I am a LayoutAdjustingMorph which acts on window edges or corners.
!


!Symbol methodsFor: 'testing' stamp: 'KenD 4/30/2020 11:52:10'!
is: aSymbol
	"Answer false if I am not a Symbol.
	 ??Note: A Symbol is also a String; should we check for #String and answer false??"
	^#Symbol = aSymbol or: [ super is: aSymbol]! !


!Morph methodsFor: 'geometry' stamp: 'KenD 5/21/2020 15:17:30'!
allocHeightForFactor: aFactor

	"Morphs with LayoutSpecs may specify propottional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minHeight
	so breakeven is when
	  amountToAlloc = minHeight / proportion * factor"
	| minHeight |
	minHeight := self minimumLayoutHeight.
	^(self isProportionalHeight)
		ifFalse:  [ minHeight ]
		ifTrue: [ minHeight / (aFactor * self layoutSpec privateProportionalHeight) ]! !

!Morph methodsFor: 'geometry' stamp: 'KenD 5/21/2020 13:46:57'!
allocWidthForFactor: aFactor

	"Morphs with LayoutSpecs may specify propottional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minWidth
	so breakeven is when
	  amountToAlloc = minWidth / proportion * factor"
	| minWidth |
	minWidth := self minimumLayoutWidth.
	^(self isProportionalWidth)
		ifFalse:  [ minWidth ]
		ifTrue: [ minWidth / (aFactor * self layoutSpec privateProportionalWidth) ]! !

!Morph methodsFor: 'layout' stamp: 'KenD 5/11/2020 12:48:34'!
minLayoutHeight

	self flag: #deprecated.
	^ self layoutSpecOrNil 
		ifNil: [ self morphHeight ] 
		ifNotNil: [ :ls | ls minimumSpecHeight ]
! !

!Morph methodsFor: 'layout' stamp: 'KenD 5/11/2020 12:48:26'!
minLayoutWidth
	
	self flag: #deprecated.
	^ self layoutSpecOrNil 
		ifNil: [ self morphWidth ] 
		ifNotNil: [ :ls |  ls minimumSpecWidth ]! !

!Morph methodsFor: 'layout' stamp: 'KenD 5/11/2020 12:42:30'!
minimumLayoutHeight
	"I combine information from a Morph and its optional LayoutSpec"
	| minHeight |

	minHeight _  self minimumExtent y. "from morph (me)."
	^ self layoutSpecOrNil 
		ifNil: [ minHeight ] 
		ifNotNil: [ :ls | minHeight max: (ls minimumSpecHeight )]
! !

!Morph methodsFor: 'layout' stamp: 'KenD 5/11/2020 12:43:28'!
minimumLayoutWidth
	"I combine information from a Morph and its optional LayoutSpec"
	| minWidth |

	minWidth _  self minimumExtent x. "from morph (me)."
	^ self layoutSpecOrNil 
		ifNil: [ minWidth ] 
		ifNotNil: [ :ls | minWidth max: (ls minimumSpecWidth )]
! !

!Morph methodsFor: 'layout' stamp: 'KenD 5/28/2020 21:38:39'!
minimumShrinkExtent
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	| minExtent |
	minExtent _ self minimumExtent.
	^ layoutSpec
		ifNil: [ minExtent ]
		ifNotNil: [ minExtent max: layoutSpec minimumShrinkWidth @ layoutSpec minimumShrinkHeight ]! !

!Morph methodsFor: 'layout' stamp: 'KenD 5/31/2020 14:20:14'!
minimumShrinkHeight
	"This returns the minimum height that the morph may be shrunk to by a LayoutAdjustingMorph.
	It is expressed in the morph own coordinates, like morphExtent."

	| minShrinkHeight |
	minShrinkHeight _ self minimumExtent y.
	^ layoutSpec
		ifNil: [ minShrinkHeight ]
		ifNotNil: [ minShrinkHeight max: layoutSpec minimumShrinkHeight ]! !

!Morph methodsFor: 'layout' stamp: 'KenD 5/28/2020 21:47:43'!
minimumShrinkWidth
	"This returns the minimum width that the morph may be shrunk to by a LayoutAdjustingMorph.
	It is expressed in the morph own coordinates, like morphExtent."

	| minShrinkWidth |
	minShrinkWidth _ self minimumExtent x.
	^ layoutSpec
		ifNil: [ minShrinkWidth ]
		ifNotNil: [ minShrinkWidth max: layoutSpec minimumShrinkWidth ]! !

!Morph methodsFor: 'testing' stamp: 'KenD 5/11/2020 13:04:22'!
isProportionalHeight
	"Answer true if I have a layoutSpec which specifies a proportional layout"

	^self layoutSpecOrNil 
		ifNil:  [ false ]
		ifNotNil: [ :ls |  ls isProportionalHeight ]! !

!Morph methodsFor: 'testing' stamp: 'KenD 5/11/2020 13:04:10'!
isProportionalWidth
	"Answer true if I have a layoutSpec which specifies a proportional layout"

	^self layoutSpecOrNil 
		ifNil:  [ false ]
		ifNotNil: [ :ls |  ls isProportionalWidth ]! !


!PluggableButtonMorph methodsFor: 'geometry' stamp: 'KenD 5/5/2020 12:15:55'!
minLayoutHeight

	^ self minimumExtent y! !

!PluggableButtonMorph methodsFor: 'geometry' stamp: 'KenD 5/5/2020 12:16:08'!
minLayoutWidth

	^ self minimumExtent x! !

!PluggableButtonMorph methodsFor: 'geometry' stamp: 'KenD 5/5/2020 17:07:16'!
minimumExtent

	| e |
	e _ Preferences windowTitleFont pointSize + 2.
	^e@e! !


!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 5/2/2020 15:09:07'!
axisEdgeWeight

	^ padding! !

!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 4/30/2020 11:54:47'!
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
	padding := edgeWeight.
	"self layoutSubmorphs"! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 5/27/2020 12:32:46'!
heightsFor: visibleSubs within: overallHeight
	"Answer array of morphHeights for visibleSubs."
	"Preconditions: 
		ySepararations already subtracted from overallHeight.
		overallHeight is large enough to contain minimumLayoutHeights of visibleSubs."
	
	| numSubs scaleFactor heightToAllocate
	  sumOfPropMin sumOfPropDesired sumOfFixedMin
	  allocatedHeights propIndices desiredProps
	  maxOfMinAllocHeight
	 |
	numSubs := visibleSubs size.
	sumOfFixedMin := 0.  "pixels"
	sumOfPropMin  := 0.  "pixels"
	sumOfPropDesired := 0. "sum of percentage fractions; may be more than 100%"
	allocatedHeights := Array ofSize: numSubs.
	desiredProps       := Array ofSize: numSubs.
	propIndices          := Set new. "keep set of proportional morph indices"
	1 to: numSubs do: [ :index | | sm layoutHeightMin |
		sm := visibleSubs at: index.
		layoutHeightMin := sm minimumLayoutHeight.
		allocatedHeights at: index put: layoutHeightMin.
		(sm isProportionalHeight)
			ifTrue:  [ | propDesired |
				propIndices add: index.
				propDesired := sm layoutSpec privateProportionalHeight.
				desiredProps at: index put: propDesired.
				sumOfPropDesired := sumOfPropDesired + propDesired.
				sumOfPropMin := sumOfPropMin + layoutHeightMin.
			]
			ifFalse: [ "Allocate height for non-proportional-height morphs"
				sumOfFixedMin := sumOfFixedMin + layoutHeightMin.
			]
	 ].

	((propIndices size = 0) "already finished"
	  or: [(overallHeight - (sumOfFixedMin + sumOfPropMin)) < 2]) "close enough"
		ifTrue: [ ^ allocatedHeights ]. 
			
	"All fixed heights allocated; rest is for proportional + leftOver"
	heightToAllocate := (overallHeight - sumOfFixedMin) max: 0.
	scaleFactor := 1.0 / (sumOfPropDesired max: 1.0).  "if > 100% then below 1"

	"Do simple default scaling"
	propIndices do: [ :morphIndex |
			allocatedHeights at: morphIndex 
							put:  ((allocatedHeights at: morphIndex) max:
									(heightToAllocate 
										* scaleFactor 
											* (desiredProps at: morphIndex)))
	].

	"Find the min alloc size at which adding height is above max of minHeights
	 Keep the proportions over the minHeights at this allocation."
	maxOfMinAllocHeight := propIndices max: [ :morphIndex | 
		(visibleSubs at: morphIndex) allocHeightForFactor: scaleFactor 
	].

    "Below the balance point, calculate proportions from belowBalanceDeltas,
	above, use desiredProps"
	(heightToAllocate <= maxOfMinAllocHeight) ifTrue: [ | belowBalanceDeltas sumOfDeltas | 
		(sumOfPropDesired < 1.0) "Trim off space to save"
			ifTrue: [ heightToAllocate := heightToAllocate * sumOfPropDesired ].
		belowBalanceDeltas := Array ofSize: visibleSubs size.
		propIndices do: [ :morphIndex | | heightAtBalance | 
			heightAtBalance := maxOfMinAllocHeight * scaleFactor * (desiredProps at: morphIndex).
			belowBalanceDeltas at: morphIndex
								   put: heightAtBalance "delta above min height"
							   	- ((visibleSubs at: morphIndex) minimumLayoutHeight)
		].
		sumOfDeltas := belowBalanceDeltas sum: [ :b | b ifNil: [0]].
		(sumOfDeltas > propIndices size) ifTrue: [ "space desired < 100%"
			propIndices do: [ :morphIndex | 
				allocatedHeights at: morphIndex
								put: ((visibleSubs at: morphIndex) minimumLayoutHeight)
									+ (((heightToAllocate - sumOfPropMin ) max: 0.0)
										* (belowBalanceDeltas at: morphIndex) / sumOfDeltas)
			]
		]
	].
	
	^ allocatedHeights! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 5/12/2020 14:07:31'!
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
			+ (leftOver * spec offAxisEdgeWeight). "first Y, edge shifted"

	^ actualPropHeight @ yPos
	
	! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 5/12/2020 14:07:42'!
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
			+ (leftOver * spec offAxisEdgeWeight). "first X, edge shifted"

	^ xPos @ actualPropWidth 
	! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 5/27/2020 12:33:28'!
widthsFor: visibleSubs within: overallWidth
	"Answer array of morphWidths for visibleSubs."
	"Preconditions: 
		xSepararations already subtracted from overallWidth.
		overallWidth is large enough to contain minimumLayoutWidths of visibleSubs."
	
	| numSubs scaleFactor widthToAllocate
	  sumOfPropMin sumOfPropDesired sumOfFixedMin
	  allocatedWidths propIndices desiredProps
	  maxOfMinAllocWidth 
	 |
	numSubs := visibleSubs size.
	sumOfFixedMin := 0. "pixels"
	sumOfPropMin   := 0. "pixels"
	sumOfPropDesired := 0. "sum of percentage fractions; may be more than 100%"
	allocatedWidths := Array ofSize: numSubs.
	desiredProps      := Array ofSize: numSubs.
	propIndices         := Set new. "keep set of proportional morph indices"
	1 to: numSubs do: [ :index | | sm layoutWidthMin |
		sm := visibleSubs at: index.
		layoutWidthMin := sm minimumLayoutWidth.
		allocatedWidths at: index put: layoutWidthMin.
		(sm isProportionalWidth)
			ifTrue:  [ | propDesired |
				propIndices add: index.
				propDesired := sm layoutSpec privateProportionalWidth.
				desiredProps at: index put: propDesired.
				sumOfPropDesired := sumOfPropDesired + propDesired.
				sumOfPropMin := sumOfPropMin + layoutWidthMin.
			]
			ifFalse: [ "Allocate width for non-proportional-width morphs"
				sumOfFixedMin := sumOfFixedMin + layoutWidthMin.
			]
	 ].

	((propIndices size = 0) "already finished"
	  or: [(overallWidth - (sumOfFixedMin + sumOfPropMin)) < 2]) "close enough"
		ifTrue: [ ^ allocatedWidths ]. 
			
	"All fixed widths allocated; rest is for proportional + leftOver"
	widthToAllocate := (overallWidth - sumOfFixedMin) max: 0.
	scaleFactor := 1.0 / (sumOfPropDesired max: 1.0).  "if > 100% then below 1"
	
	"Do simple default scaling"
	propIndices do: [ :morphIndex |
			allocatedWidths at: morphIndex 
							put:  ((allocatedWidths at: morphIndex) max:
									(widthToAllocate 
										* scaleFactor 
											* (desiredProps at: morphIndex)))
	].

	"Find the min alloc size at which adding width is above max of minWidths
	 Keep the proportions over the minWidths at this allocation."
	maxOfMinAllocWidth := propIndices max: [ :morphIndex | 
		(visibleSubs at: morphIndex) allocWidthForFactor: scaleFactor 
	].

     "Below the balance point, calculate proportions from belowBalanceDeltas,
	above, use desiredProps"
	(widthToAllocate <= maxOfMinAllocWidth) ifTrue: [ | belowBalanceDeltas sumOfDeltas | 
		(sumOfPropDesired < 1.0) "Trim off space to save"
			ifTrue: [ widthToAllocate := widthToAllocate * sumOfPropDesired ].
		belowBalanceDeltas := Array ofSize: visibleSubs size.
		propIndices do: [ :morphIndex | | widthAtBalance | 
			widthAtBalance := maxOfMinAllocWidth * scaleFactor * (desiredProps at: morphIndex).
			belowBalanceDeltas at: morphIndex
								   put: widthAtBalance "delta above min width"
							   	- ((visibleSubs at: morphIndex) minimumLayoutWidth)
		].
		sumOfDeltas := belowBalanceDeltas sum: [ :b | b ifNil: [0]].
		(sumOfDeltas > propIndices size) ifTrue: [ "space desired < 100%"
			propIndices do: [ :morphIndex |
				allocatedWidths at: morphIndex
								put: ((visibleSubs at: morphIndex) minimumLayoutWidth) 
									+ (((widthToAllocate - sumOfPropMin ) max: 0.0)
										* (belowBalanceDeltas at: morphIndex) / sumOfDeltas)
			]
		]
	].
	
	^ allocatedWidths! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'KenD 5/26/2020 15:14:45'!
addMorphFront: aMorph
	"Add a submorph, at the bottom or right, with a default LayoutSpec if none was provided."

	aMorph layoutSpec.
	super addMorphFront: aMorph! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'KenD 5/28/2020 07:33:52'!
removedMorph: aMorph
	"One of my submorphs has been removed."

	super removedMorph: aMorph.
	self layoutSubmorphs! !

!LayoutMorph methodsFor: 'testing' stamp: 'KenD 5/4/2020 12:58:45'!
isColumn

	^ direction = #vertical! !

!LayoutMorph methodsFor: 'testing' stamp: 'KenD 5/4/2020 12:57:59'!
isRow

	^ direction = #horizontal! !

!LayoutMorph methodsFor: 'private' stamp: 'KenD 5/29/2020 13:57:00'!
privateAddAllMorphs: aCollection atIndex: index
    aCollection do: [ :m | m layoutSpec ].
    ^super privateAddAllMorphs: aCollection atIndex: index! !

!LayoutMorph methodsFor: 'private' stamp: 'KenD 5/29/2020 13:57:16'!
privateAddMorph: aMorph atIndex: index
    aMorph layoutSpec.
    ^super privateAddMorph: aMorph atIndex: index! !

!LayoutMorph methodsFor: 'private' stamp: 'KenD 5/29/2020 13:57:32'!
privateAddMorph: aMorph atIndex: index position: aPoint
    aMorph layoutSpec.
    ^super privateAddMorph: aMorph atIndex: index position: aPoint! !

!LayoutMorph methodsFor: 'deprecated' stamp: 'KenD 5/31/2020 14:13:54'!
padding

	self flag: #deprecated
	^ padding! !

!LayoutMorph methodsFor: '*morphic-misc1' stamp: 'KenD 12/24/2013 15:14'!
direction: horizOrVert

	self flag: #jmvVer. "Move to category #accessing"

	direction := horizOrVert! !


!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 4/30/2020 09:34:28'!
offAxisEdgeWeight
	^minorDirectionPadding! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 5/28/2020 07:31:23'!
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
	minorDirectionPadding _ edgeWeight! !

!LayoutSpec methodsFor: 'layout' stamp: 'KenD 5/28/2020 21:35:37'!
minimumShrinkHeight
	"Generally prefer asking the morph itself!!"

	proportionalHeight ifNil: [ ^0 ].
	^fixedHeight ifNil: [0]! !

!LayoutSpec methodsFor: 'layout' stamp: 'KenD 5/28/2020 21:35:20'!
minimumShrinkWidth
	"Generally prefer asking the morph itself!!"

	proportionalWidth ifNil: [ ^0 ].
	^fixedWidth ifNil: [0]! !

!LayoutSpec methodsFor: 'layout' stamp: 'KenD 5/11/2020 12:51:04'!
minimumSpecExtent

	^ self minimumSpecWidth @ self minimumSpecHeight ! !

!LayoutSpec methodsFor: 'layout' stamp: 'KenD 5/11/2020 12:36:37'!
minimumSpecHeight
	"If fixedHeight is not nil, use it.  
	 If fixdHeight and propostionlHeight are nil, use morphHeight"

	^ fixedHeight ifNil: [ proportionalHeight ifNotNil: [ 0 ] ifNil: [ morph morphHeight ] ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'KenD 5/11/2020 12:37:14'!
minimumSpecWidth
	"If fixedWidth is not nil, use it.  
	 If fixdWidth and propostionlWidth are nil, use morphWidth"

	^ fixedWidth ifNil: [ proportionalWidth ifNotNil: [ 0 ] ifNil: [ morph morphWidth ] ]! !

!LayoutSpec methodsFor: 'deprecated' stamp: 'KenD 5/31/2020 14:19:05'!
fixedOrMorphHeight

	self flag: #deprecated.
	
	^fixedHeight ifNil: [ morph morphHeight ]! !

!LayoutSpec methodsFor: 'deprecated' stamp: 'KenD 5/31/2020 14:18:45'!
fixedOrMorphWidth

	self flag: #deprecated.
	
	^fixedWidth ifNil: [ morph morphWidth ]! !

!LayoutSpec methodsFor: '*morphic-misc1' stamp: 'KenD 4/18/2015 20:18'!
morph
	"For #showHalo"
	^ morph! !

!LayoutSpec methodsFor: '*morphic-misc1' stamp: 'KenD 12/24/2013 15:15'!
privateFixedWidth

	^ fixedWidth! !

!LayoutSpec methodsFor: '*morphic-misc1' stamp: 'KenD 12/24/2013 15:15'!
privateProportionalWidth

	^ proportionalWidth ! !


!LayoutSpec class methodsFor: 'instance creation' stamp: 'KenD 5/22/2020 13:49:12'!
fixedWidth: aNumber fixedHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber
	^ LayoutSpec new
		fixedWidth: aNumber;
		fixedHeight: otherNumber;
		offAxisEdgeWeight: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'instance creation' stamp: 'KenD 5/22/2020 13:54:56'!
fixedWidth: aNumber proportionalHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber

	^self new
		fixedWidth: aNumber;
		proportionalHeight: otherNumber;
		offAxisEdgeWeight: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'instance creation' stamp: 'KenD 5/22/2020 13:55:19'!
proportionalWidth: aNumber fixedHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber

	^self new
		proportionalWidth: aNumber;
		fixedHeight: otherNumber;
		offAxisEdgeWeight: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'instance creation' stamp: 'KenD 5/22/2020 13:56:21'!
proportionalWidth: aNumber proportionalHeight: otherNumber offAxisEdgeWeight: aSymbolOrNumber

	^self new
		proportionalWidth: aNumber;
		proportionalHeight: otherNumber;
		offAxisEdgeWeight: aSymbolOrNumber! !


!Morph methodsFor: 'geometry' stamp: 'KenD 5/29/2020 14:15:06'!
minimumLayoutExtent
	"This returns the minimum extent that the morph may be shrunk to, 
	when resizing LayoutMorphs. 
	It is expressed in the morph own coordinates, like morphExtent."

	"This method combines the information from a Morph and its optional LayoutSpec"
	| minExtent |
	minExtent _ self minimumExtent.
	^ layoutSpec
		ifNil: [ minExtent ]
		ifNotNil: [ minExtent max: ( layoutSpec minimumSpecWidth @ layoutSpec minimumSpecHeight ) ]! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'KenD 5/29/2020 14:03:30'!
addAllMorphs: aCollection
	^ (aCollection size > 0) 
		ifTrue: [ self privateAddAllMorphs: aCollection atIndex: submorphs size ]
		ifFalse: [ self ]! !


!BrowserCommentTextMorph methodsFor: 'displaying' stamp: 'KenD 5/5/2020 17:24:59'!
hidePane

	self hide.
	separator visible ifTrue: [
		proportionalHeight _ self layoutSpec proportionalLayoutHeight.
		separatorHeight _ separator layoutSpec fixedOrMorphHeight ].
	separator layoutSpec fixedHeight: 0.
	self layoutSpec proportionalHeight: 0.
	separator ifNotNil: [ separator hide ]! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 5/22/2020 13:59:53'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout updateReqButton |
	requirements := PluggableListMorph
		model: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	requirements color: Theme current textPane.
		
	deleteReqButton := PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'delete'.
	deleteReqButton color: self widgetsColor.
	updateReqButton _ PluggableButtonMorph 
							model: requirements model
							action: #updateSelectedRequirement 
							label: 'update'.
	updateReqButton color: self widgetsColor.
							
	buttonLayout := LayoutMorph newRow.
	buttonLayout 
		addMorph: deleteReqButton 
		layoutSpec: (LayoutSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop);
		color: self widgetsColor quiteWhiter;
		addMorph: updateReqButton 
		layoutSpec: (LayoutSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop);
		color: self widgetsColor quiteWhiter.
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LayoutMorph newRow.
	^ reqLayout 
		doAdoptWidgetsColor;
		addMorph: requirements 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.8
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #leftOrTop);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.2 
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #rightOrBottom);
		color: `Color transparent`;
		yourself
		! !


!StringRequestMorph class methodsFor: 'private' stamp: 'KenD 5/25/2020 12:37:58'!
deltaToTextPane
	"Answer a distance to translate an instance of the receiver by when it is opened in the world in order to have the hand be over the text pane (so the text pane has focus).
	Distance is relative to font size"
	| e |
	e _ Preferences windowTitleFont lineSpacing.
	^ (2 * e)@(1.5 * e)! !


!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 5/15/2020 17:49:30'!
addMorphFrontFromWorldPosition: aMorph

	aMorph layoutSpecOrNil ifNil: [aMorph layoutSpec: LayoutSpec keepMorphExtent ].
	self addMorphFront: aMorph.
	self layoutSubmorphs.
! !

!LayoutMorph methodsFor: 'adjust' stamp: 'KenD 5/31/2020 15:38:28'!
adjustBy: aLayoutAdjustMorph at: aPoint
	"See Class Comment of LayoutAdjustingMorph"

	direction == #horizontal ifTrue: [
		self adjustHorizontallyBy: aLayoutAdjustMorph at: aPoint ].

	direction == #vertical ifTrue: [
		self adjustVerticallyBy: aLayoutAdjustMorph at: aPoint ].! !

!LayoutMorph methodsFor: 'adjust' stamp: 'KenD 5/28/2020 21:46:22'!
adjustHorizontallyBy: aLayoutAdjustMorph at: aPoint
	| delta l ls r rs lNewWidth rNewWidth i lCurrentWidth rCurrentWidth |
	i _ submorphs indexOf: aLayoutAdjustMorph.
	l _ self submorphs at: i +1.
	ls _ l layoutSpec.
	lCurrentWidth _ l morphWidth max: 1.	"avoid division by zero"
	r _ self submorphs at: i - 1.
	rs _ r layoutSpec.
	rCurrentWidth _ r morphWidth max: 1.	"avoid division by zero"
	delta _ aPoint x - aLayoutAdjustMorph referencePosition x.
	delta _ delta max: l minimumShrinkWidth - lCurrentWidth.
	delta _ delta min: rCurrentWidth - r minimumShrinkWidth.
	delta = 0 ifTrue: [ ^self ].
	rNewWidth _ rCurrentWidth - delta.
	lNewWidth _ lCurrentWidth + delta.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [ | leftNewProportion rightNewProportion toDistribute |	"If both proportional, update them"
			leftNewProportion _ lNewWidth / (lNewWidth + rNewWidth).
			rightNewProportion _ 1.0 - leftNewProportion.
			toDistribute _ ls proportionalLayoutWidth + rs proportionalLayoutWidth.
			ls setProportionalWidth: leftNewProportion * toDistribute.
			rs setProportionalWidth: rightNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'adjust' stamp: 'KenD 5/28/2020 21:49:25'!
adjustVerticallyBy: aLayoutAdjustMorph at: aPoint
	| delta t ts b bs tNewHeight bNewHeight i tCurrentHeight bCurrentHeight |
	i _ submorphs indexOf: aLayoutAdjustMorph.
	t _ self submorphs at: i +1.
	ts _ t layoutSpec.
	tCurrentHeight _ t morphHeight max: 1.	"avoid division by zero"
	b _ self submorphs at: i - 1.
	bs _ b layoutSpec.
	bCurrentHeight _ b morphHeight max: 1.	"avoid division by zero"
	delta _ aPoint y - aLayoutAdjustMorph referencePosition y.
	delta _ delta max: t minimumShrinkHeight - tCurrentHeight.
	delta _ delta min: bCurrentHeight - b minimumShrinkHeight.
	delta = 0 ifTrue: [ ^self ].
	tNewHeight _ tCurrentHeight + delta.
	bNewHeight _ bCurrentHeight - delta.
	(ts isProportionalHeight and: [ bs isProportionalHeight ])
		ifTrue: [ | bottomNewProportion toDistribute topNewProportion |	"If both proportional, update them"
			topNewProportion _ tNewHeight / (tNewHeight + bNewHeight).
			bottomNewProportion _ 1.0 - topNewProportion.
			toDistribute _ ts proportionalLayoutHeight + bs proportionalLayoutHeight.
			ts setProportionalHeight: topNewProportion * toDistribute.
			bs setProportionalHeight: bottomNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ts isProportionalHeight ifFalse: [
				ts fixedOrMorphHeight: tNewHeight ].
			bs isProportionalHeight ifFalse: [
				bs fixedOrMorphHeight: bNewHeight ]].
	self layoutSubmorphs! !

!LayoutMorph methodsFor: 'geometry' stamp: 'KenD 5/15/2020 19:24:31'!
minimumExtent
	"Answer size sufficient to frame my submorphs."
	
	| width height |
	width  := 0.
	height := 0.
	(self direction =  #vertical)
		ifTrue: [ "Column"
			self submorphsToLayout do: [ :sm | 
				"use maximum width across submorphs"
				width := width max: sm minimumLayoutWidth.
				"sum up submorph heights, including separation"
				height := height + (sm minimumLayoutHeight) + self ySeparation.
			].
			width  := width + (2 * self xSeparation). "separation on each side"
			height := height + self ySeparation. "one side already separated"
		]
		ifFalse: [ "Row"
			self submorphsToLayout do: [ :sm |
				"sum up submorphs width"
				width := width + (sm minimumLayoutWidth) + self xSeparation.
				"use maximum height across submorph"
				height := height max: sm minimumLayoutHeight.
			].
			height := height + (2 * self ySeparation). "separation on each side"
			width  := width + self xSeparation.       "one side already separated"
		].

	^ (width @ height) + self extentBorder! !

!LayoutMorph methodsFor: 'initialization' stamp: 'KenD 5/12/2020 09:23:30'!
beColumn
	"Establish the major layout axis, with default edge weight" 

	direction _ #vertical.
	self axisEdgeWeight ifNil: [self axisEdgeWeight: #center].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'KenD 5/12/2020 09:23:35'!
beRow
	"Establish the major layout axis, with default edge weight" 

	direction _ #horizontal.
	self axisEdgeWeight ifNil: [self axisEdgeWeight: #rowLeft].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'layout' stamp: 'pb 3/17/2020 13:41:27'!
layoutBounds
	"Return the bounds for laying out children of the receiver"

	^ self morphLocalBounds! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 5/9/2020 14:25:12'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		layoutNeeded _ false.
		^self].

	"Invariant: morphExtent >=  minimumLayoutExtent"
	self refreshExtent.
	
	direction == #horizontal ifTrue: [
		self layoutSubmorphsHorizontallyIn: self layoutBounds ].

	direction == #vertical ifTrue: [
		self layoutSubmorphsVerticallyIn: self layoutBounds ].

	layoutNeeded _ false! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 5/12/2020 14:10:38'!
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
				+ (leftOver * self axisEdgeWeight). "first X, edge shifted"
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

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 5/12/2020 14:09:59'!
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
				+ (leftOver * self axisEdgeWeight). "first Y, edge shifted"
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

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 5/9/2020 14:24:33'!
refreshExtent
	"Invariant: my morphExtent >= my minimumExtent"
		
	self morphExtent: (self morphExtent max: self minimumExtent)! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'KenD 5/26/2020 15:15:01'!
addMorph: aMorph
	"Add a submorph, at the bottom or right, with a default LayoutSpec if none was provided."

	aMorph layoutSpec.
	super addMorphFront: aMorph! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'KenD 5/26/2020 15:15:38'!
addMorph: aMorph layoutSpec: aLayoutSpec

	"Add a submorph, at the bottom or right, with aLayoutSpec"
	aMorph layoutSpec: aLayoutSpec.
	super addMorphFront: aMorph! !

!LayoutMorph methodsFor: 'deprecated' stamp: 'KenD 5/28/2020 07:27:28'!
padding: aSymbolOrNumber
	"This sets how extra space is used when doing layout. For example, a column might have extra , unneded vertical space. #top means widgets are set close to the top, and extra space is at bottom. Conversely, #bottom means widgets are set close to the bottom, and extra space is at top. Valid values include #left and #right (for rows) and #center. Alternatively, any number between 0.0 and 1.0 might be used.
	self new padding: #center
	self new padding: 0.9
	"
	self flag: #deprecated.
	padding _ aSymbolOrNumber
		caseOf: {
			[ #top ] -> [ 0.0 ].
			[ #left ] -> [ 0.0 ].
			[ #center ] -> [ 0.5 ].
			[ #right ] -> [ 1.0 ].
			[ #bottom ] -> [ 1.0 ]
		}
		otherwise: [ aSymbolOrNumber ]! !


!LayoutMorph class methodsFor: 'examples' stamp: 'KenD 5/22/2020 13:57:31'!
example13
	"
	self example13
	"
	| pane row innerRow |
pane _ (LayoutMorph newColumn separation: 5)	name: #example13.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	innerRow _ (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (BorderedRectMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'KenD 5/22/2020 13:55:23'!
example2
	"
	self example2
	"
	| pane row |
	pane _ (LayoutMorph newColumn separation: 5)	name: #example2.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #A)
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 offAxisEdgeWeight: #rightOrBottom);
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #B)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #C)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 offAxisEdgeWeight: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 200@180 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'KenD 5/22/2020 13:58:02'!
example3
	"
	self example3
	"
	| pane row innerRow |
pane _ (LayoutMorph newColumn separation: 5)	name: #example3.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	innerRow _ (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (BorderedRectMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 offAxisEdgeWeight: #center);
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 offAxisEdgeWeight: #leftOrTop);
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'KenD 5/22/2020 13:51:32'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane _ LayoutMorph newRow separation: 5. "1"
pane addMorph: (StringMorph contents: '1').

rect1 := BorderedRectMorph new color: (Color lightOrange); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect1.
rect2 := BorderedRectMorph new color: (Color cyan); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect2.
pane
	color: Color lightGreen;
	morphPosition: 120 @ 50 extent: 180 @ 100;
	openInWorld.

pane _ LayoutMorph newRow separation: 5. "2"
pane addMorph: (StringMorph contents: '2').

rect1 := BorderedRectMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect1.
rect2 := BorderedRectMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane
	color: Color lightGreen;
	morphPosition: 320 @ 50 extent: 180 @ 100;
	openInWorld.


pane _ LayoutMorph newRow separation: 5. "3"
pane addMorph: (StringMorph contents: '3').

rect1 := BorderedRectMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
rect2 := BorderedRectMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 offAxisEdgeWeight: #center).
pane addMorph: rect2.
pane
	color: Color lightGreen;
	morphPosition: 520 @ 50 extent: 180 @ 100;
	openInWorld! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'KenD 5/22/2020 14:00:30'!
initialize
	super initialize.
	viewBox _ LayoutMorph newRow color: self defaultColor.
	self
		addMorph: UpdatingStringMorph initializedInstance
		layoutSpec:  (LayoutSpec morphWidthProportionalHeight: 0.5).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			offAxisEdgeWeight: #rightOrBottom).
	viewBox separation: self defaultHeight // 8
! !


!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 5/5/2020 08:16:57'!
fixedOrMorphHeight: aNumber
	"aNumber is taken as the fixed height to use.
	No proportional part."
	fixedHeight
		ifNotNil: [ fixedHeight _ aNumber ]
		ifNil: [ fixedHeight _ aNumber.
			    morph morphHeight: aNumber 
		].
	proportionalHeight _ nil! !

!LayoutSpec methodsFor: 'accessing' stamp: 'KenD 5/5/2020 08:17:15'!
fixedOrMorphWidth: aNumber
	"aNumber is taken as the fixed width to use.
	No proportional part."
	fixedWidth
		ifNotNil: [ fixedWidth _ aNumber ]
		ifNil: [ fixedWidth _ aNumber. morph morphWidth: aNumber ].
	proportionalWidth _ nil! !

!LayoutSpec methodsFor: 'layout' stamp: 'KenD 5/5/2020 17:24:59'!
proportionalLayoutHeight

	^ proportionalHeight ifNil: [ 0 ]! !

!LayoutSpec methodsFor: 'deprecated' stamp: 'KenD 5/28/2020 07:28:09'!
minorDirectionPadding
	self flag: #deprecated.
	^minorDirectionPadding! !

!LayoutSpec methodsFor: 'deprecated' stamp: 'KenD 5/28/2020 07:27:51'!
minorDirectionPadding: aSymbolOrNumber
	"This sets how padding is done in the secondary direction. For instance, if the owning morph is set in a row, the row will control horizontal layout. But if there is unused vertical space, it will be used according to this parameter. For instance, #top sets the owning morph at the top. Same for #bottom and #center. If the owner is contained in a column, #left, #center or #right should be used. Alternatively, any number between 0.0 and 1.0 can be used.
	self new minorDirectionPadding: #center
	self new minorDirectionPadding: 0.9
	"
	self flag: #deprecated.
	minorDirectionPadding _ aSymbolOrNumber
		caseOf: {
			[ #top ] -> [ 0.0 ].
			[ #left ] -> [ 0.0 ].
			[ #center ] -> [ 0.5 ].
			[ #right ] -> [ 1.0 ].
			[ #bottom ] -> [ 1.0 ]
		}
		otherwise: [ aSymbolOrNumber ]! !

!LayoutSpec methodsFor: '*morphic-misc1' stamp: 'KenD 12/24/2013 15:14'!
privateFixedHeight

	^ fixedHeight! !

!LayoutSpec methodsFor: '*morphic-misc1' stamp: 'KenD 12/24/2013 15:15'!
privateProportionalHeight

	^ proportionalHeight! !


!LayoutSpec class methodsFor: 'deprecated' stamp: 'KenD 5/28/2020 07:29:16'!
fixedWidth: aNumber fixedHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self flag: #deprecated.
	^ LayoutSpec new
		fixedWidth: aNumber;
		fixedHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'deprecated' stamp: 'KenD 5/28/2020 07:29:23'!
fixedWidth: aNumber proportionalHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self flag: #deprecated.
	^self new
		fixedWidth: aNumber;
		proportionalHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'deprecated' stamp: 'KenD 5/28/2020 07:29:29'!
proportionalWidth: aNumber fixedHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self flag: #deprecated.
	^self new
		proportionalWidth: aNumber;
		fixedHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !

!LayoutSpec class methodsFor: 'deprecated' stamp: 'KenD 5/28/2020 07:30:00'!
proportionalWidth: aNumber proportionalHeight: otherNumber minorDirectionPadding: aSymbolOrNumber
	self flag: #deprecated.
	^self new
		proportionalWidth: aNumber;
		proportionalHeight: otherNumber;
		minorDirectionPadding: aSymbolOrNumber! !


!Theme methodsFor: 'other options' stamp: 'KenD 5/31/2020 08:09:44'!
layoutAdjusterThickness

	self flag: #todo.  "Revisit this; consider moving proportional stuff out of Theme entirely."
	
	^ Preferences standardListFont pointSize // 2! !

!methodRemoval: LayoutMorph #proportionalHeightNormalizationFactor stamp: 'KenD 5/27/2020 07:20:40'!
LayoutMorph removeSelector: #proportionalHeightNormalizationFactor!
!methodRemoval: LayoutMorph #addAllMorphs: stamp: 'KenD 5/29/2020 13:56:01'!
LayoutMorph removeSelector: #addAllMorphs:!
!methodRemoval: LayoutMorph #addAllMorphs:after: stamp: 'KenD 5/29/2020 13:55:44'!
LayoutMorph removeSelector: #addAllMorphs:after:!
!methodRemoval: LayoutMorph #proportionalWidthNormalizationFactor stamp: 'KenD 5/27/2020 07:20:40'!
LayoutMorph removeSelector: #proportionalWidthNormalizationFactor!
!methodRemoval: Morph #requiredWidthOrSpec stamp: 'KenD 5/31/2020 14:17:12'!
Morph removeSelector: #requiredWidthOrSpec!

!LayoutMorph reorganize!
('accessing' addMorphFrontFromWorldPosition: adoptWidgetsColor: axisEdgeWeight axisEdgeWeight: direction separation: xSeparation ySeparation)
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
('*morphic-misc1' addCustomMenuItems:hand: direction: editMe)
!


!LayoutSpec reorganize!
('initialization' initialize)
('accessing' fixedHeight fixedHeight: fixedOrMorphHeight: fixedOrMorphWidth: fixedWidth fixedWidth: morph: offAxisEdgeWeight offAxisEdgeWeight: proportionalHeight: proportionalHeight:minimum: proportionalWidth: proportionalWidth:minimum: setProportionalHeight: setProportionalWidth: useMorphHeight useMorphWidth)
('layout' fixedOrMinimumLayoutHeight fixedOrMinimumLayoutWidth heightFor: heightForComfortable: minimumLayoutHeight minimumLayoutWidth minimumShrinkHeight minimumShrinkWidth minimumSpecExtent minimumSpecHeight minimumSpecWidth proportionaLayoutlHeight proportionalLayoutHeight proportionalLayoutWidth widthFor: widthForComfortable:)
('testing' isProportionalHeight isProportionalWidth usesMorphExtent usesMorphHeight usesMorphWidth)
('deprecated' fixedOrMorphHeight fixedOrMorphWidth minorDirectionPadding minorDirectionPadding:)
('private')
('*morphic-misc1' morph privateFixedHeight privateFixedWidth privateProportionalHeight privateProportionalWidth)
!


!LayoutSpec class reorganize!
('instance creation' fixedExtent: fixedHeight: fixedWidth: fixedWidth:fixedHeight: fixedWidth:fixedHeight:offAxisEdgeWeight: fixedWidth:proportionalHeight: fixedWidth:proportionalHeight:offAxisEdgeWeight: keepMorphExtent morphHeightFixedWidth: morphHeightProportionalWidth: morphWidthFixedHeight: morphWidthProportionalHeight: proportionalHeight: proportionalWidth: proportionalWidth:fixedHeight: proportionalWidth:fixedHeight:offAxisEdgeWeight: proportionalWidth:proportionalHeight: proportionalWidth:proportionalHeight:offAxisEdgeWeight: useAll)
('deprecated' fixedWidth:fixedHeight:minorDirectionPadding: fixedWidth:proportionalHeight:minorDirectionPadding: proportionalWidth:fixedHeight:minorDirectionPadding: proportionalWidth:proportionalHeight:minorDirectionPadding:)
!

