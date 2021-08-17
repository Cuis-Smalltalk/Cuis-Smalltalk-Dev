'From Cuis 5.0 [latest update: #4762] on 17 August 2021 at 4:09:44 pm'!

!Morph methodsFor: 'updating' stamp: 'jmv 8/17/2021 16:08:53'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way."

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	"outsetBy: 1 due to anti aliasing in VectorCanvas"
	self invalidateDisplayRect: (self displayBoundsOrBogus outsetBy: 1) for: self.
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].
	self removeProperty: #shadow.! !

