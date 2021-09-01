'From Cuis 5.0 [latest update: #4822] on 1 September 2021 at 11:41:53 am'!

!Morph methodsFor: 'updating' stamp: 'jmv 9/1/2021 11:41:29'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way.
	Notes:
		Area used before any change is reported here.
		Area used after any changes is reported when displayBounds are updated.
			See updateBoundsIn:addDamageTo: and senders.
	"

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self basicDisplayBounds ifNotNil: [ :b |
		"outsetBy: 1 due to anti aliasing in VectorCanvas"
		self invalidateDisplayRect: (b outsetBy: 1) for: self ].
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].
	self removeProperty: #bitMask.
	self removeProperty: #fullBoundsInOwner.! !

!methodRemoval: Morph #displayBoundsOrBogus stamp: 'jmv 9/1/2021 11:41:45'!
Morph removeSelector: #displayBoundsOrBogus!
