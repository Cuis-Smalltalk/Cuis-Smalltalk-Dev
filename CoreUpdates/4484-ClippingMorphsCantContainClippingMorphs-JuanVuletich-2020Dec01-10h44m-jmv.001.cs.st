'From Cuis 5.0 [latest update: #4483] on 1 December 2020 at 10:45:05 am'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 12/1/2020 10:39:59'!
clipsSubmorphsReally
	"Currently only one level of clipping is supported.
	This means that if a morph clipsSubmorphs, then no submorph in its tree can't do it.
	This is a current limitation of VectorCanvas"

	self clipsSubmorphs ifFalse: [ ^false ].
	self allOwnersDo: [ :o | o clipsSubmorphs ifTrue: [ ^false ]].
	^true! !


!Morph methodsFor: 'change reporting' stamp: 'jmv 12/1/2020 10:40:16'!
invalidateDisplayRect: damageRect for: aMorph
	"
	If we clip submorphs, then we clip damageRect.
	
	aMorph is the morph that changed and therefore should be redrawn. In some cases, damage reporting is done by no longer occupying some area, and requesting whatever appropriate morph to be drawn there. In such cases, aMorph should be nil. See senders."

	| clippedRect b |

	"warning. Senders are using global coordinates. Redesign!!"
	"local now!!!!!!!!!!"
	self flag: #jmvVer2.	"ok?"

	self visible ifFalse: [ ^self].

	clippedRect _ damageRect.
	self clipsSubmorphsReally ifTrue: [
		b _ self displayBounds.
		b ifNil: [ ^self ].
		clippedRect _ damageRect intersect: b ].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect for: aMorph ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/1/2020 10:40:08'!
displayFullBounds
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world)."

	| answer |
	answer _ self displayBounds.
	answer ifNil: [ ^nil ].
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m |
			answer _ answer quickMerge: m displayFullBounds ]].
	^answer! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 12/1/2020 10:42:27'!
clipsSubmorphs
	"Answer true if we clip the shape of our submorphs to our own.
	Answer true only when clipping by the canvas is needed.
	Morphs that can guarantee that submorphs don't protrude and answer false to 
	#submorphsMightProtrude should answer false for better performance.
	Note: Currently we are limited to only one clipping morph in an owner / submorph chain.
	See #clipsSubmorphsReally"

	^ false! !

!Morph methodsFor: 'updating' stamp: 'jmv 12/1/2020 10:40:19'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way."

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self invalidateDisplayRect: self displayBoundsOrBogus for: self.
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].! !


!MorphicCanvas methodsFor: 'private' stamp: 'jmv 12/1/2020 10:40:22'!
clippingByCurrentMorphDo: aBlock
	"Do clipping only if currentMorph says so"

	| prevClipRect prevClippingMorphRect |

	currentMorph clipsSubmorphsReally ifFalse: [ ^aBlock value ].

	prevClipRect _ self clipRect.
	prevClippingMorphRect _ clippingMorphDisplayBounds.
	"Might use the fact that currentMorph has just been drawn."
	clippingMorphDisplayBounds _ self boundingRectOfCurrentMorphAfterDraw.
	self setClipRect: (prevClipRect intersect: clippingMorphDisplayBounds).
	self clipCurrentMorph: true.
	aBlock ensure: [
		self clipCurrentMorph: false.
		self setClipRect: prevClipRect.
		clippingMorphDisplayBounds _ prevClippingMorphRect ]! !

