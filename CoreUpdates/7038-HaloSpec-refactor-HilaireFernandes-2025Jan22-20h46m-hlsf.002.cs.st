'From Cuis7.3 [latest update: #6992] on 22 January 2025 at 9:47:12 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'hlsf 1/22/2025 21:44:34'!
haloSpecifications

	^ Preferences at: #haloSpecifications! !


!Morph methodsFor: 'halos and balloon help' stamp: 'hlsf 1/22/2025 21:44:42'!
addHandlesTo: aHaloMorph box: box
	"Add halo handles to the halo.  Apply the halo filter if appropriate"

	self haloSpecifications do: [ :aSpec |
		(self
			wantsHaloHandleWithSelector: aSpec addHandleSelector
			inHalo: aHaloMorph) ifTrue: [
		aHaloMorph
			perform: aSpec addHandleSelector
			with: aSpec ]].
	aHaloMorph target
		addOptionalHandlesTo: aHaloMorph
		box: box! !

