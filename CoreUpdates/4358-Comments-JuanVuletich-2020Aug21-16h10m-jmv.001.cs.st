'From Cuis 5.0 [latest update: #4357] on 21 August 2020 at 4:34:48 pm'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 8/21/2020 16:14:59'!
invalidateDisplayRect: damageRect fromSubmorph: submorphOrNil for: aMorph
	"
	If we clip submorphOrNil, then we clip damageRect.
	When calling from self, submorphOrNil should be nil, i.e. we are not reporting damage for some submorph.
	
	aMorph is the morph that changed and therefore should be redrawn. In some cases, damage reporting is done by no longer occupying some area, and requesting whatever appropriate morph to be drawn there. In such cases, aMorph should be nil. See senders."

	| clippedRect b |

	"warning. Senders are using global coordinates. Redesign!!"
	"local now!!!!!!!!!!"
	self flag: #jmvVer2.	"ok?"

	self visible ifFalse: [ ^self].

	clippedRect _ damageRect.
	submorphOrNil ifNotNil: [
	 	submorphOrNil == self clippedSubmorph
			ifTrue: [
				b _ self displayBounds.
				b ifNil: [ ^self ].
				clippedRect _ damageRect intersect: b ]].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect fromSubmorph: self for: aMorph ]! !

!Morph methodsFor: 'updating' stamp: 'jmv 8/21/2020 16:15:38'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way."

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self invalidateDisplayRect: self displayBoundsOrBogus fromSubmorph: nil for: self.
	self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]! !

