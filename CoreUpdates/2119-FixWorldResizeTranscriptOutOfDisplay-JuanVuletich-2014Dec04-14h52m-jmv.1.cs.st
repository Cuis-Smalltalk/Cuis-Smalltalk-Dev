'From Cuis 4.2 of 25 July 2013 [latest update: #2111] on 4 December 2014 at 2:58:01.753823 pm'!

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 12/4/2014 14:57'!
strictlyStaggeredInitialFrameFor: aStandardSystemView initialExtent: initialExtent world: aWorld 
	"This method implements a staggered window placement policy that I (di) like.
	Basically it provides for up to 4 windows, staggered from each of the 4 corners.
	The windows are staggered so that there will always be a corner visible."

	| allowedArea grid initialFrame otherFrames cornerSel corner delta putativeCorner free maxLevel |
	allowedArea := (self maximumUsableAreaInWorld: aWorld) 
				insetBy: (self scrollBarSetback @ self screenTopSetback extent: 0 @ 0).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel := allowedArea area > 300000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid := allowedArea area > 500000 ifTrue: [40] ifFalse: [20].
	initialFrame := 0 @ 0 extent: initialExtent.
	"min: (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))))
							min: 600@400"
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [ :w | w visible and: [w isCollapsed not]]) 
						collect: [:w | w morphBoundsInWorld].
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

