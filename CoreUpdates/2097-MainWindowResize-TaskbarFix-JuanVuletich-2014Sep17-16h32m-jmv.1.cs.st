'From Cuis 4.2 of 25 July 2013 [latest update: #2096] on 17 September 2014 at 4:32:43.724774 pm'!

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/17/2014 16:28'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| oldBoundsInWorld someChange |

	"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
	oldBoundsInWorld _ self morphBoundsInWorld.
	someChange _ false.
	location position = newPos ifFalse: [
		location setPosition: newPos.
		someChange _ true ].

	extent = newExtent ifFalse: [
		(self privateExtent: newExtent) ifTrue: [
			someChange _ true ]].

	someChange ifTrue: [
		"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
		self invalidateDisplayRect: oldBoundsInWorld from: nil.
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]! !

