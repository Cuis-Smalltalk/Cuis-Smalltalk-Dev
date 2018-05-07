'From Cuis 5.0 of 7 November 2016 [latest update: #3307] on 2 May 2018 at 5:54:32 pm'!

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 5/2/2018 17:48:28'!
initialFrameFor: aView initialExtent: initialExtent world: aWorld 
	"Find a plausible initial screen area for the supplied view, which should be a StandardSystemView, taking into account the 'reverseWindowStagger' Preference, the size needed, and other windows currently on the screen."

	^ self 
		strictlyStaggeredInitialFrameFor: aView
		initialExtent: initialExtent
		world: aWorld! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 5/2/2018 17:49:23'!
standardWindowExtent
	"Answer the standard default extent for new windows.  "

	| grid allowedArea maxLevel |
	"NOTE: following copied from strictlyStaggeredInitialFrameFor:"
	allowedArea _ self maximumUsableArea insetBy: (
		self scrollBarSetback @ self screenTopSetback extent: `0@0`
	).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel _ allowedArea area > 700000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid _ allowedArea area > 700000 ifTrue: [40] ifFalse: [20].
	^ (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))) min: `52@40` * Preferences standardCodeFont height! !

!methodRemoval: TheWorldMenu #staggerPolicyString!
TheWorldMenu removeSelector: #staggerPolicyString!
!methodRemoval: TheWorldMenu #toggleWindowPolicy!
TheWorldMenu removeSelector: #toggleWindowPolicy!
!methodRemoval: RealEstateAgent class #staggerOffset!
RealEstateAgent class removeSelector: #staggerOffset!
!methodRemoval: RealEstateAgent class #standardPositionsInWorld:!
RealEstateAgent class removeSelector: #standardPositionsInWorld:!
!methodRemoval: RealEstateAgent class #windowColumnsDesired!
RealEstateAgent class removeSelector: #windowColumnsDesired!
!methodRemoval: RealEstateAgent class #windowRowsDesired!
RealEstateAgent class removeSelector: #windowRowsDesired!
!methodRemoval: Preferences class #reverseWindowStagger!
Preferences class removeSelector: #reverseWindowStagger!
!methodRemoval: Preferences class #staggerPolicyString!
Preferences class removeSelector: #staggerPolicyString!
!methodRemoval: Preferences class #toggleWindowPolicy!
Preferences class removeSelector: #toggleWindowPolicy!
