'From Cuis 6.0 [latest update: #5972] on 7 August 2023 at 12:22:34 pm'!

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 8/7/2023 12:16:36'!
heightsFor: visibleSubs within: overallHeight
	"Answer array of morphHeights for visibleSubs."
	"Preconditions: 
		ySepararations already subtracted from overallHeight.
		overallHeight is large enough to contain minimumLayoutHeights of visibleSubs."

	| numSubs scaleFactor heightToAllocate
	  sumOfPropMin sumOfPropDesired sumOfPropOriginallyDesired sumOfFixedMin
	  allocatedHeights propIndices desiredProps
	  maxOfMinAllocHeight
	 |
	numSubs := visibleSubs size.
	sumOfFixedMin := 0.  "pixels"
	sumOfPropMin  := 0.  "pixels"
	sumOfPropDesired := 0. "sum of percentage fractions; may be more than 100%"
	sumOfPropOriginallyDesired := 0. "Different from sumOfPropDesired if some #maximumLayoutHeight"
	allocatedHeights := Array ofSize: numSubs.
	desiredProps       := Array ofSize: numSubs.
	propIndices          := Set new. "keep set of proportional morph indices"
	1 to: numSubs do: [ :index | | sm layoutHeightMin |
		sm := visibleSubs at: index.
		layoutHeightMin := sm minimumLayoutHeight.
		allocatedHeights at: index put: layoutHeightMin.
		(sm isProportionalHeight)
			ifTrue:  [ | propDesired maxHeight |
				propDesired := sm layoutSpec privateProportionalHeight.
				sumOfPropOriginallyDesired := sumOfPropOriginallyDesired + propDesired.
				maxHeight := sm maximumLayoutHeight.
				(maxHeight notNil and: [ maxHeight < (propDesired * overallHeight)])
					ifTrue: [
						allocatedHeights at: index put: maxHeight.
						sumOfFixedMin := sumOfFixedMin + maxHeight ]
					ifFalse: [
						propIndices add: index.
						desiredProps at: index put: propDesired.
						sumOfPropDesired := sumOfPropDesired + propDesired.
						sumOfPropMin := sumOfPropMin + layoutHeightMin ].
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
	scaleFactor := 1.0.
	"If sum of requested proportional heights is more than 1.0,
	scale to make it all fit."
	sumOfPropOriginallyDesired > 0.999 ifTrue: [
		scaleFactor := 1.0 / sumOfPropDesired ].

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

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 8/7/2023 11:53:35'!
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
	scaleFactor := 1.0.
	"If sum of requested proportional widths is more than 1.0,
	scale to make it all fit."
	sumOfPropDesired > 1.0 ifTrue: [
		scaleFactor := 1.0 / sumOfPropDesired ].
	
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


!MessageSetWindow methodsFor: 'GUI building' stamp: 'jmv 8/7/2023 11:44:05'!
buildMorphicWindow
	"Answer a morphic window with the given label that can display the receiver"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := self buildMorphicMessageList.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + ScrollBar scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].
	model changed: #editSelection! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 8/7/2023 11:44:12'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := PluggableListMorphOfMany
		model: model
		listGetter: #list
		primarySelectionGetter: #listIndex
		primarySelectionSetter: #toggleListIndex:
		listSelectionGetter: #listSelectionAt:
		listSelectionSetter: #listSelectionAt:put:
		mainView: self
		menuGetter: #listMenu
		keystrokeAction: #changeListKey:from:.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + ScrollBar scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].! !


!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 8/7/2023 11:44:19'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := PluggableListMorph
		model: model
		listGetter: #list
		indexGetter: #listIndex
		indexSetter: #toggleListIndex:
		mainView: self
		menuGetter: #listMenu
		keystrokeAction: #changeListKey:from:.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + ScrollBar scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].! !

