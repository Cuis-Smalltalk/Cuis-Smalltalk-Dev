'From Cuis 6.0 [latest update: #5754] on 5 April 2023 at 9:20:42 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 4/5/2023 21:18:44'!
haloRedrawMayBeNeeded
	self world ifNotNil: [ :w |
		w haloMorphsDo: [ :h |
			h target ifNotNil: [ :t |
				t haloShowsCoordinateSystem ifTrue: [
					self withAllOwnersDo: [ :m |
						m == t ifTrue: [ h redrawNeeded ]]]]]]! !


!Morph methodsFor: 'updating' stamp: 'jmv 4/5/2023 21:18:59'!
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
		self invalidateDisplayRect: b for: self ].
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].
	self haloRedrawMayBeNeeded.
	self removeProperty: #bitMask.
	self removeProperty: #fullBoundsInOwner.! !

