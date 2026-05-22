"
Responsible for real-estate management on the screen, which is to say, controlling where new windows appear, with what sizes, etc.  5/20/96 sw
"
Class {
	#name : 'RealEstateAgent',
	#superclass : 'Object',
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'as yet unclassified' }
RealEstateAgent class >> initialFrameFor: aView initialExtent: initialExtent world: aWorld  [
	"Find a plausible initial screen area for the supplied view, which should be a StandardSystemView, taking into account the 'reverseWindowStagger' Preference, the size needed, and other windows currently on the screen."

	^ self 
		strictlyStaggeredInitialFrameFor: aView
		initialExtent: initialExtent
		world: aWorld
]

{ #category : 'as yet unclassified' }
RealEstateAgent class >> initialFrameFor: aView world: aWorld [
	"Find a plausible initial screen area for the supplied view.  See called method."

	^ self initialFrameFor: aView initialExtent: aView initialExtent world: aWorld
]

{ #category : 'as yet unclassified' }
RealEstateAgent class >> maximumUsableArea [

	| allowedArea |
	allowedArea := Display boundingBox.
	^allowedArea

]

{ #category : 'as yet unclassified' }
RealEstateAgent class >> maximumUsableAreaInWorld: aWorldOrNil [

	| allowedArea |
	allowedArea := Display boundingBox.
	aWorldOrNil ifNotNil: [
		allowedArea := allowedArea intersect: aWorldOrNil viewBox.
		aWorldOrNil taskbar ifNotNil: [ :tb |
			tb displayBounds ifNotNil: [ :r |
				allowedArea := (allowedArea areasOutside: r) first ]]].
	^allowedArea

]

{ #category : 'as yet unclassified' }
RealEstateAgent class >> screenTopSetback [
	^ 0
]

{ #category : 'as yet unclassified' }
RealEstateAgent class >> scrollBarSetback [
	^ 16-3  "width = 16; inset from border by 3"
]

{ #category : 'as yet unclassified' }
RealEstateAgent class >> standardWindowExtent [
	"Answer the standard default extent for new windows.  "

	| grid allowedArea maxLevel |
	"NOTE: following copied from strictlyStaggeredInitialFrameFor:"
	allowedArea := self maximumUsableArea insetBy: (
		self scrollBarSetback @ self screenTopSetback extent: `0@0`
	).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel := allowedArea area > 700000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid := allowedArea area > 700000 ifTrue: [40] ifFalse: [20].
	^ ((allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))) 
		min: `52@40` * (Preferences at: #standardCodeFont) lineSpacing) rounded
]

{ #category : 'as yet unclassified' }
RealEstateAgent class >> strictlyStaggeredInitialFrameFor: aStandardSystemView initialExtent: initialExtent world: aWorld  [
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
	otherFrames := otherFrames reject: [ :f | f isNil ].
	0 to: maxLevel do: [ :level | 
		1 to: 4 do: [ :ci | 
			cornerSel := #(#topLeft #topRight #bottomRight #bottomLeft) at: ci.
			corner := allowedArea perform: cornerSel.
			"The extra grid//2 in delta helps to keep title tabs distinct"
			delta := ((maxLevel - level) * grid + (grid // 2)) @ (level * grid).
			1 to: ci - 1 do: [ :i | delta := delta y negated @ delta x ].	"slow way"
			putativeCorner := corner + delta.
			free := true.
			otherFrames do: [ :w |
				free := free & (((w perform: cornerSel) - putativeCorner)r > 5)].
			free 
				ifTrue: [
					^(initialFrame aligned: (initialFrame perform: cornerSel)
						with: putativeCorner) translatedAndSquishedToBeWithin: allowedArea]]].
	"If all else fails..."
	^(self scrollBarSetback @ self screenTopSetback 
		extent: initialFrame extent) translatedAndSquishedToBeWithin: allowedArea
]
