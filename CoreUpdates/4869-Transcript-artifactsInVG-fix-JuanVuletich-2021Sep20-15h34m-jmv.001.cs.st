'From Cuis 5.0 [latest update: #4868] on 20 September 2021 at 3:34:52 pm'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 9/20/2021 12:32:14'!
invalidateDisplayRect: damageRect for: aMorph
	"
	If we clip submorphs, then we clip damageRect.
	
	aMorph is the morph that changed and therefore should be redrawn. In some cases, damage reporting is done by no longer occupying some area, and requesting whatever appropriate morph to be drawn there. In such cases, aMorph should be nil. See senders."

	| clippedRect b |
	self visible ifFalse: [ ^self].

	clippedRect _ damageRect.
	aMorph == self ifFalse: [
		self clipsSubmorphsReally ifTrue: [
			b _ self displayBounds.
			b ifNil: [ ^self ].
			clippedRect _ damageRect intersect: b ]].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect for: aMorph ].! !

