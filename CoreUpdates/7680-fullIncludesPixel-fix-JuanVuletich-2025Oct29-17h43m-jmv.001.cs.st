'From Cuis7.5 [latest update: #7679] on 29 October 2025 at 6:18:18 pm'!

!BoxMorph methodsFor: 'geometry services' stamp: 'jmv 10/29/2025 18:11:20'!
fullIncludesPixel: worldPoint
	"Answer true if worldPoint is in some submorph, even if not inside our shape."

	(self includesPixel: worldPoint) ifTrue: [ ^ true ].
	(self submorphsMightProtrude and: [self clipsSubmorphs not]) ifTrue: [
		self submorphsDo: [ :m |
			(m fullIncludesPixel: worldPoint) ifTrue: [ ^ true ]]].
	^ false.! !

!BoxMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 10/29/2025 18:12:55'!
topMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	| doWeIncludePixel |
	doWeIncludePixel := self includesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [self clipsSubmorphs not]]) ifTrue: [
		submorphs do: [ :m |
			(m topMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]]].
	doWeIncludePixel ifTrue: [ ^self ].
	^nil! !

!BoxMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 10/29/2025 17:14:41'!
topUnlockedMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	| doWeIncludePixel |
	self isLocked ifTrue: [ ^nil ].
	doWeIncludePixel := self includesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [self clipsSubmorphs not]]) ifTrue: [
		submorphs do: [ :m |
			(m topUnlockedMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]]].
	doWeIncludePixel ifTrue: [ ^self ].
	^nil! !


!Morph methodsFor: 'geometry services' stamp: 'jmv 10/29/2025 18:12:04'!
fullIncludesPixel: worldPoint
	"Answer true if worldPoint is in some submorph, even if not inside our shape."

	(self includesPixel: worldPoint) ifTrue: [ ^ true ].
	self submorphsDo: [ :m |
		(m fullIncludesPixel: worldPoint) ifTrue: [ ^ true ]].
	^ false.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/29/2025 18:17:28'!
includesPixel: worldPoint
	"Answer true if the grabbing from the pixel argument pixel with the mouse should grab us:

	- Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.

	- Some implementations (BoxMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.

	- For other morphs, not BoxMorph, (i.e. they run this implementation), if VectorGraphics is not active,
	answer true for any point that lies within our bounds. If this is the case, consider using VectorGraphics: In addition to
	many other advantages, this method will always answer a strictly correct answer.

	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph
	covers us. A common case is to find the topmost morph at some position. In that case, iterating front to back ensures
	that if any other morph covers us, it is found first.

	Note: This method is redefined by some subclasses. This implementation is only used for morphs drawn by
	VectorCanvas, either because the main canvas is a VectorCanvas, or because it is a HybridCanvas and we answer
	true to #requiresVectorCanvas.
	(See other implementors)

	Also see #ownsPixel:, #coversPixel:
	Also see #fullIncludesPixel:, #topMorphAt:, #topUnlockedMorphAt:"

	"Check if #ownsPixel:, if possible."
	self mainCanvas ifNotNil: [ :mainCanvas |
		mainCanvas canDoVectorGraphics ifTrue: [
			^ (mainCanvas morphIdAt: worldPoint) = self morphId ]].

	^ false.! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 10/29/2025 17:28:05'!
topMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	(self includesPixel: worldPoint) ifTrue: [ ^ self ].
	submorphs do: [ :m |
		(m topMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]].
	^nil! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 10/29/2025 17:28:25'!
topUnlockedMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	self isLocked ifTrue: [ ^nil ].
	(self includesPixel: worldPoint) ifTrue: [ ^ self ].
	submorphs do: [ :m |
		(m topUnlockedMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]].
	^nil! !


!BoxMorph methodsFor: 'geometry services' stamp: 'jmv 10/29/2025 18:17:04'!
includesPixel: worldPoint
	"Answer true if the grabbing from the pixel argument pixel with the mouse should grab us:
	(See comment and implementation at Morph).
	Note: This implementation also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.
	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph covers us.
	Note: This implementation is only used for morphs with a cheap #coversPixel:.
	(See other implementors)
	Also see #ownsPixel:, #coversPixel:
	Also see #fullIncludesPixel:, #topMorphAt:, #topUnlockedMorphAt:"

	^ self visible and: [self coversPixel: worldPoint].! !

