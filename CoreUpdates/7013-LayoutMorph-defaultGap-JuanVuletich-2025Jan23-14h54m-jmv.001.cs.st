'From Cuis7.3 [latest update: #7003] on 23 January 2025 at 2:54:14 pm'!

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 1/22/2025 16:37:26'!
defaultGap
	"Answer the default gap between submorphs for the receiver.
	Redefine this method for a different initial gap in your morphs."

	^ 0! !


!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 1/22/2025 16:37:37'!
initialize
	super initialize.
	gap := self defaultGap.
	axisEdgeWeight := 0.0.
	doAdoptWidgetsColor := false.! !

