'From Cuis 5.0 [latest update: #4280] on 22 July 2020 at 11:17:45 pm'!

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 7/22/2020 23:13:46'!
maximumUsableAreaInWorld: aWorldOrNil

	| allowedArea |
	allowedArea _ Display boundingBox.
	aWorldOrNil ifNotNil: [
		allowedArea _ allowedArea intersect: aWorldOrNil viewBox.
		aWorldOrNil taskbar ifNotNil: [ :tb |
			tb displayBounds ifNotNil: [ :r |
				allowedArea _ (allowedArea areasOutside: r) first ]]].
	^allowedArea
! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 7/22/2020 23:13:55'!
strictlyStaggeredInitialFrameFor: aStandardSystemView initialExtent: initialExtent world: aWorld 
	"This method implements a staggered window placement policy that I (di) like.
	Basically it provides for up to 4 windows, staggered from each of the 4 corners.
	The windows are staggered so that there will always be a corner visible."

	| allowedArea grid initialFrame otherFrames cornerSel corner delta putativeCorner free maxLevel |
	allowedArea := (self maximumUsableAreaInWorld: aWorld) 
				insetBy: (self scrollBarSetback @ self screenTopSetback extent: `0 @ 0`).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel := allowedArea area > 300000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid := allowedArea area > 500000 ifTrue: [40] ifFalse: [20].
	initialFrame := `0 @ 0` extent: initialExtent.
	"min: (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))))
							min: 600@400"
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [ :w | w visible and: [w isCollapsed not]]) 
						collect: [:w | w displayBounds].
	otherFrames _ otherFrames reject: [ :f | f isNil ].
	0 to: maxLevel do: [ :level | 
			1 to: 4 do: [:ci | 
					cornerSel := #(#topLeft #topRight #bottomRight #bottomLeft) at: ci.
					corner := allowedArea perform: cornerSel.
					"The extra grid//2 in delta helps to keep title tabs distinct"
					delta := ((maxLevel - level) * grid + (grid // 2)) @ (level * grid).
					1 to: ci - 1 do: [ :i | delta _ delta y negated @ delta x ].	"slow way"
					putativeCorner := corner + delta.
					free := true.
					otherFrames do: [ :w |
						free := free & ((w perform: cornerSel) ~= putativeCorner)].
					free 
						ifTrue: [
							^(initialFrame aligned: (initialFrame perform: cornerSel)
								with: putativeCorner) translatedAndSquishedToBeWithin: allowedArea]]].
	"If all else fails..."
	^(self scrollBarSetback @ self screenTopSetback 
		extent: initialFrame extent) translatedAndSquishedToBeWithin: allowedArea! !


!Morph methodsFor: 'macpal' stamp: 'jmv 7/22/2020 23:12:08'!
flash

	self displayBounds ifNotNil: [ :r |
		Display flash: r ]! !

!Morph methodsFor: 'macpal' stamp: 'jmv 7/22/2020 23:12:24'!
flashWith: aColor

	self displayBounds ifNotNil: [ :r | Display flash: r with: aColor ]! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 7/22/2020 23:13:02'!
potentialEmbeddingTargets
	"Return the potential targets for embedding the receiver"
	| myRect myWorld |
	owner ifNil:[^#()].
	myWorld := owner world ifNil:[^#()].
	myRect := self displayBounds.
	^myWorld submorphs select: [ :m |
		m isReallyVisible
		and: [ m isLocked not
			and: [(m displayBounds intersects: myRect)
				and: [(m ~= self)
					and: [(m isKindOf: HaloMorph) not]]]]
		]! !


!SystemWindow methodsFor: 'menu' stamp: 'jmv 7/22/2020 23:15:08'!
fullScreen
	"Zoom Window to Full World size with possible DeskMargins"

	"SystemWindow fullScreen"

	| left right possibleBounds |
	(self hasProperty: #originalBounds)
		ifFalse: [ "Expand"
			self setProperty: #originalBounds toValue: self displayBounds.
			left := right := 0.
			possibleBounds := (RealEstateAgent maximumUsableAreaInWorld: self world) 
				insetBy: (left @ 0 corner: right @ 0).
			possibleBounds := possibleBounds insetBy: Theme current fullScreenDeskMargin
		] 
		ifTrue: [ "Contract"
			possibleBounds := self valueOfProperty: #originalBounds.
			self removeProperty: #originalBounds.
		].
	self morphPosition: possibleBounds topLeft extent: possibleBounds extent! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 7/22/2020 23:15:18'!
resize: boundingBox
	(self hasProperty: #originalBounds) ifFalse: [
		self setProperty: #originalBounds toValue: self displayBounds].
	self morphPosition: boundingBox origin extent: boundingBox extent! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 7/22/2020 23:17:11'!
drawOn: aCanvas
	Transcript
		showOnDisplay: true;
		displayOn: form in: self morphLocalBounds.
	aCanvas image: form at: self morphTopLeft.
	Transcript
		bounds: self displayBounds;
		showOnDisplay: doImmediateUpdates.
	self updateWorkspace! !

