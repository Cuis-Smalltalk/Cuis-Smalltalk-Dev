'From Cuis 5.0 [latest update: #4737] on 5 August 2021 at 12:51:24 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:26:43'!
morphLocalBoundsForError
	"Only to be used for drawing in an error condition. See senders."

	^`0@0 corner: 75@70`.! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:33:56'!
morphExtentInWorld
	"World coordinates"

	^(self externalizeDistanceToWorld: extent) ceiling! !

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:21:05'!
morphLocalBounds

	^`0@0` extent: extent.! !

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:26:40'!
morphLocalBoundsForError
	"Only to be used for drawing in an error condition. See senders."

	^self morphLocalBounds.! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:33:59'!
morphExtentInWorld
	"World coordinates"

	^(self externalizeDistanceToWorld: extent) ceiling! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:21:10'!
morphLocalBounds

	^`0@0` extent: extent.! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:26:47'!
morphLocalBoundsForError
	"Only to be used for drawing in an error condition. See senders."

	^self morphLocalBounds.! !

!WidgetMorph methodsFor: 'layout' stamp: 'jmv 8/5/2021 12:06:55'!
minItemWidth

	^extent x! !


!Morph methodsFor: 'drawing' stamp: 'jmv 8/5/2021 12:26:13'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: `-75@-70 corner: 75@70`
		color: `Color blue`! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:33:16'!
morphExtentInWorld: newExtent
	"world coordinates"

	self morphExtent: (self internalizeDistanceFromWorld: newExtent).! !

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:10:08'!
morphHeight
	"In own's coordinates"

	^ extent y! !

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:07:26'!
morphWidth
	"In own's coordinates"

	^ extent x! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 8/5/2021 12:01:46'!
attachMorphBeside: aMorph

	"Position the given morph beside this hand, then grab it."

	aMorph aboutToBeGrabbedBy: self.
	self grabMorph: aMorph delta: extent x@0.
! !


!WorldMorph methodsFor: 'canvas' stamp: 'jmv 8/5/2021 11:49:46'!
ensureNonDisplayCanvas
	(canvas isNil or: [
		canvas drawsOnDisplay or: [
		(canvas extent ~= extent) or: [
		canvas form depth ~= Display depth]]]) ifTrue: [
			"allocate a new offscreen canvas the size of the window"
			self setCanvas: (MorphicCanvas withExtent: extent depth: Display depth)]! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:33:12'!
morphExtentInWorld: newExtent
	"world coordinates"

	self morphExtent: (self internalizeDistanceFromWorld: newExtent).! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:10:12'!
morphHeight
	"In own's coordinates"

	^ extent y! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:07:31'!
morphWidth
	"In own's coordinates"

	^ extent x! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 8/5/2021 11:43:29'!
refreshExtent
	"Invariant: my morphExtent >= my minimumExtent"
		
	self morphExtent: (extent max: self minimumExtent)! !


!ProgressMorph methodsFor: 'accessing' stamp: 'jmv 8/5/2021 12:02:29'!
label: aString subLabel: otherString
	| label subLabel n |
	label _ aString.
	subLabel _ otherString.
	layoutSpec notNil ifTrue: [
		n _ label size * extent x * 0.95 // (FontFamily defaultFamilyAndPointSize widthOfString: label).
		label _ label squeezedTo: n.
		n _ subLabel size * extent x * 0.95  // (FontFamily defaultFamilyAndPointSize widthOfString: subLabel).
		subLabel _ subLabel squeezedTo: n ].
	self whenUIinSafeState: [
		labelMorph contents: label.
		subLabelMorph contents: subLabel.
		self updatePositionAndExtent. ]! !


!ImageMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2021 12:50:40'!
drawOn: aCanvas

	aCanvas image: image at: borderWidth@borderWidth.
	(borderWidth > 0) ifTrue: [
		aCanvas
			frameRectangle:  (`0@0` extent: extent)
			color:  borderColor
			borderWidth:  borderWidth 
			borderStyleSymbol:  #simple ]! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 12:12:41'!
itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	| ptY last |
	self hasSubmorphs ifFalse: [ ^nil ].
	(aPoint > `0@0` and: [ aPoint < extent ]) ifFalse: [ ^nil ].
	ptY _ aPoint y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	self firstSubmorph morphPosition y > ptY ifTrue: [ ^nil ].
	last _ self lastSubmorph.
	last morphPosition y + last morphHeight < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^self 
		findSubmorphBinary: [ :m |
			(m morphPosition y <= ptY and: [ m morphPosition y + m morphHeight >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPosition y + (m morphHeight // 2) > ptY ifTrue: [-1] ifFalse: [1]]]! !


!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2021 12:13:33'!
drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		textComposition: self textComposition
		bounds: self morphLocalBounds
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus).

	model actualContents isEmpty ifTrue: [
		owner
			valueOfProperty: #emptyTextDisplayMessage
			ifPresentDo: [ :msg |
				aCanvas
					drawString: msg
					at: `0@0`
					font: nil
					color: Theme current textEmptyDisplayMessage ]].! !


!LabelMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2021 12:14:09'!
drawOn: aCanvas
	aCanvas
		drawString: (contents ifNil: [ '' ])
		at: borderWidth@borderWidth
		font: self fontToUse
		color: color.
	(borderWidth > 0) ifTrue: [
		aCanvas
			frameRectangle: (`0@0` extent: extent)
			color: borderColor
			borderWidth: borderWidth
			borderStyleSymbol: #simple ]! !


!SystemWindow methodsFor: 'change reporting' stamp: 'jmv 8/5/2021 12:14:44'!
invalidateTitleArea

	"not really pretty... also invalidating the top border, regardless of it being above or below the title area
	(Different themes use various looks, this covers them all)"
	self invalidateLocalRect: (`0@0` extent: extent x @ (self labelHeight + borderWidth))! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/5/2021 11:48:18'!
makeMeFullyVisible 

	self world morphExtent > `0@0` ifFalse: [^ self].

	(self morphPosition >= `0@0` and: [ self morphPosition < (self world morphExtent-extent)]) ifTrue: [
		^ self "OK -- visible"].

	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: self morphExtentInWorld world: self world) topLeft! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 8/5/2021 11:48:34'!
windowBottomLeft: aPointInOwner
	"aPointInOwner is in the owner's coordinate system"

	| delta |
	delta _ (self internalize: aPointInOwner) - self morphBottomLeft.
	self morphExtent: extent + (delta x negated @ delta y).
	self morphPosition: self morphPosition + aPointInOwner - (self externalize: self morphBottomLeft).! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 8/5/2021 11:48:41'!
windowTopRight: aPointInOwner
	"aPointInOwner is in the owner's coordinate system"

	| delta |
	delta _ (self internalize: aPointInOwner) - self morphTopRight.
	self morphExtent: extent + (delta x @ delta y negated).
	self morphPosition: self morphPosition + aPointInOwner - (self externalize: self morphTopRight).! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 8/5/2021 12:08:28'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| newPos x y |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [ ^self ].
	x _ aHand morphPosition x - 20.
	y _ aHand morphPosition y + 20.
	x + extent x > aHand world morphWidth ifTrue: [
		x _ aHand world morphWidth - extent x ].
	y + extent y > aHand world morphHeight ifTrue: [
		y _ aHand morphPosition y - extent y - 12 ].
	newPos _ x@y.
	aHand world addMorphFront: self position: newPos.
	aHand balloonHelp: self! !


!MenuLineMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2021 12:14:26'!
drawOn: aCanvas 
	| baseColor |
	baseColor _ owner color.
	aCanvas
		fillRectangle: (`0@0` corner: extent x @ (extent y / 2))
		color: baseColor twiceDarker.
			
	aCanvas
		fillRectangle: (0 @ (extent y / 2) corner: extent)
		color: baseColor twiceLighter! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 8/5/2021 11:44:30'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| trialRect |
	popUpOwner _ sourceItem.
	sourceItem world addMorphFront: self position: rightOrLeftPointInWorld first.
	trialRect _ rightOrLeftPointInWorld first extent: extent.
	trialRect right > sourceItem world morphWidth ifTrue: [
		self morphPosition: rightOrLeftPointInWorld second - (extent x@0)].
	self fitInWorld.! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 8/5/2021 11:43:50'!
fitInWorld

	| delta trialRect |
	trialRect _ Rectangle origin: self morphPosition extent: extent.
	delta _ trialRect amountToTranslateWithin: owner displayBounds.
	self morphPosition: trialRect origin + delta.! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/5/2021 12:27:03'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r r2 w |
	r _ currentMorph morphLocalBoundsForError.
	w _ r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	r2 _ r origin corner: r corner-w.
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	self line: r2 topLeft to: r2 bottomRight width: w color: `Color yellow`.
	self line: r2 topRight to: r2 bottomLeft width: w color: `Color yellow`.
	currentMorph displayBoundsSetFrom: self.! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 8/5/2021 12:18:57'!
boundingRectOfCurrentMorphFromLocalBounds
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid for morphs where #morphLocalBounds is known"

	| boundingRect |
	boundingRect _ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 8/5/2021 12:17:34'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	BitBltCanvas doesn't keep track of Morph bounds as it draws them. But it is limited to drawing morphs where
	#morphLocalBounds is known. Use it, then."

	| boundingRect |
	boundingRect _ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !

!methodRemoval: PasteUpMorph #fontPreferenceChanged stamp: 'jmv 8/5/2021 12:50:22'!
PasteUpMorph removeSelector: #fontPreferenceChanged!
!methodRemoval: KernelMorph #morphTopLeft stamp: 'jmv 8/5/2021 12:50:22'!
KernelMorph removeSelector: #morphTopLeft!
!methodRemoval: Morph #morphHeight stamp: 'jmv 8/5/2021 12:50:22'!
Morph removeSelector: #morphHeight!
!methodRemoval: Morph #morphLocalBounds stamp: 'jmv 8/5/2021 12:50:22'!
Morph removeSelector: #morphLocalBounds!
!methodRemoval: Morph #morphContainsPoint: stamp: 'jmv 8/5/2021 12:50:22'!
Morph removeSelector: #morphContainsPoint:!
!methodRemoval: Morph #morphExtentInWorld stamp: 'jmv 8/5/2021 12:50:22'!
Morph removeSelector: #morphExtentInWorld!
!methodRemoval: Morph #inATwoWayScrollPane stamp: 'jmv 8/5/2021 12:50:22'!
Morph removeSelector: #inATwoWayScrollPane!
!methodRemoval: Morph #morphExtent stamp: 'jmv 8/5/2021 12:50:22'!
Morph removeSelector: #morphExtent!
!methodRemoval: Morph #morphTopLeft stamp: 'jmv 8/5/2021 12:50:22'!
Morph removeSelector: #morphTopLeft!
!methodRemoval: Morph #morphExtent: stamp: 'jmv 8/5/2021 12:50:22'!
Morph removeSelector: #morphExtent:!
!methodRemoval: Morph #morphExtentInWorld: stamp: 'jmv 8/5/2021 12:50:22'!
Morph removeSelector: #morphExtentInWorld:!
!methodRemoval: Morph #morphWidth stamp: 'jmv 8/5/2021 12:50:22'!
Morph removeSelector: #morphWidth!
!methodRemoval: Morph #minItemWidth stamp: 'jmv 8/5/2021 12:50:22'!
Morph removeSelector: #minItemWidth!

!WidgetMorph reorganize!
('accessing' borderColor borderColor: borderWidth borderWidth: color color:)
('initialization' defaultBorderColor defaultBorderWidth defaultColor initialize noBorder)
('drawing' drawOn:)
('geometry' displayBoundsSetFrom: displayBoundsUpdateFrom: ensureMinimimExtent extentBorder morphBottomLeft morphBottomRight morphExtent morphExtent: morphExtentInWorld morphExtentInWorld: morphHeight morphHeight: morphLocalBounds morphLocalBoundsForError morphPosition:extent: morphTopLeft morphTopRight morphWidth morphWidth: privateExtent: rotationCenter)
('geometry testing' hasVariableExtent isOrthoRectangularMorph morphContainsPoint: requiresVectorCanvas submorphsMightProtrude wantsContour)
('layout' minItemWidth)
('layout-properties' layoutSpec:)
('testing' is:)
!

