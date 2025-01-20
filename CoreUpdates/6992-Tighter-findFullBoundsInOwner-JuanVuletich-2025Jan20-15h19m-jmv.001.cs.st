'From Cuis7.3 [latest update: #6991] on 20 January 2025 at 3:20:43 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 1/20/2025 15:20:18'!
findFullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!!
	Note: Answers an integer rectangle"

	| prevNotVisibleFlag w c answer prevOwner |
	(owner notNil and: [owner isWorldMorph]) ifTrue: [
		w := owner ]
	ifFalse: [
		w := UISupervisor ui ].
	c := w mainBoundsFinderCanvas.
	"Hack owner so #displayBoundsSetFrom: will update privateDisplayBounds"
	prevOwner := owner.
	owner := w.
	prevNotVisibleFlag := self privateFlagAt: 3.
	self privateFlagAt: 3 put: false.
	c fullUpdateProtrudingBounds: self.
	answer := self displayFullBounds.
	"Reset owner and privateDisplayBounds (if needed) so no one finds out what we've just done!!"
	prevOwner == owner ifFalse: [
		owner := prevOwner.
		self world = w ifTrue: [
			self privateFlagAt: 3 put: false.
			self allOwnersReverseDo: [ :m | c into: m ].
			c fullUpdateProtrudingBounds: self.
			self allOwnersDo: [ :m | c outOfMorph ]]].
	self privateFlagAt: 3 put: prevNotVisibleFlag.
	^answer origin corner: answer corner -(1@0).! !

