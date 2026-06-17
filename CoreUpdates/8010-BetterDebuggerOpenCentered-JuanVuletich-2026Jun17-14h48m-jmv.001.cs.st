'From Cuis7.9 [latest update: #8009] on 17 June 2026 at 3:11:17 pm'!


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 17/Jun/2026, 6:08:48 pm (UTC)'!
initialCenteredFrameFor: aView world: aWorld
	"Centered window placement"

	| allowedArea |
	allowedArea := (self maximumUsableAreaInWorld: aWorld) 
				insetBy: (self scrollBarSetback @ self screenTopSetback extent: `0 @ 0`).

	^(Rectangle center: allowedArea center extent: aView initialExtent)
		translatedAndSquishedToBeWithin: allowedArea! !

!DebuggerWindow methodsFor: 'open/close' stamp: 'jmv 17/Jun/2026, 6:09:04 pm (UTC)'!
initialFrameIn: aWorld
	^RealEstateAgent initialCenteredFrameFor: self world: aWorld! !

!methodRemoval: DebuggerWindow class #open:label: stamp: 'jmv 17/Jun/2026, 6:09:20 pm (UTC)'!
DebuggerWindow class removeSelector: #open:label:!
