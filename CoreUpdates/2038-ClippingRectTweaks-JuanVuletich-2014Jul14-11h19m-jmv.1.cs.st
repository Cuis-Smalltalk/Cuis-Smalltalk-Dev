'From Cuis 4.2 of 25 July 2013 [latest update: #2038] on 14 July 2014 at 11:41:55.163584 am'!

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/14/2014 11:26'!
clippingRectForCurrentMorph
	^currentTransformation displayBoundsOfTransformOf: currentMorph clippingRect! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/14/2014 11:36'!
externalizeDisplayBounds: r

	| inOwners |
	self flag: #jmvVer2.

	inOwners _ self location displayBoundsOfTransformOf: r.
	^owner
		ifNotNil: [ owner externalizeDisplayBounds: inOwners ]
		ifNil: [ inOwners ]! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 7/14/2014 11:35'!
externalizeDisplayBounds: r

	^self isWorldMorph
		ifTrue: [ r ]
		ifFalse: [ super externalizeDisplayBounds: r ]! !


!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/14/2014 11:26'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	"To replace #fullDrawOn:"
	| oldClipRect |
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	aMorph layoutSubmorphsIfNeeded.	"preferiria ni preguntar... para M3 al menos"
	(self isVisible: aMorph morphFullBoundsInWorld) ifFalse: [
		self outOfMorph.
		^ self].

	aMorph isKnownFailing ifTrue: [
		self drawAsError: aMorph.
		self outOfMorph.
		^ self].

	(aMorph isOwnedByHand and: [ Preferences cheapWindowReframe ]) ifTrue: [
		self drawAsOutline: aMorph.
		self outOfMorph.
		^ self].
	
	" --------------------------------------------------" 
	"Aca es donde hay que dibujar primero el morph, y despues los submorphs, sabiendo los bounds hallados al dibujar el morph!!!!!!!!!! (en M3)
	este es el mismo metodo que hace into: y outOfMorph    :D  "
	" --------------------------------------------------"
	"preferiria ni preguntar... para M3 al menos"
	(self isVisible: aMorph morphBoundsInWorld) ifTrue: [
		aMorph drawOn: self ].

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')"
	aMorph hasSubmorphs ifTrue: [
		aMorph clipsSubmorphs ifTrue: [
			"Draw with additional clipping done by aMorph."
			"Ojo, en M3 es un rect, no un quad. el clipping final se hace a los pixels usados por aMorph... (hacerlo!!)"
			"En Morphic 3, lo que hay que hacer es que en vez de pedirle el #clippingRect a aMorph, 
			hay que usar los bounds hallados al dibujar aMorph. (y hacer que en M3Canvas, #additionalClipRect:while: para que no traduzca rectangulos...)
			En realidad no es imprescindible hacerlo ya, porque en todo caso, contamos con #clippingRect para M2..."
			oldClipRect _ clipRect.
			self setClipRect: (clipRect intersect: self clippingRectForCurrentMorph) ].
		aMorph privateSubmorphs reverseDo: [ :m |
			self fullDraw: m ].
		oldClipRect ifNotNil: [
			self setClipRect: oldClipRect ].
		].
	self outOfMorph! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/14/2014 11:37'!
morphBoundsInWorld
	"Return the bounds of this morph."
	
	"WORLD absolute bounds :("

	"remove senders and implementors"
	self flag: #jmvVer2.
	^ self externalizeDisplayBounds: (0@0 extent: self morphExtent)! !

!methodRemoval: BorderedRectMorph #clippingRect!
BorderedRectMorph removeSelector: #clippingRect!
!methodRemoval: FormCanvas #clippingRectCurrentMorph!
FormCanvas removeSelector: #clippingRectCurrentMorph!
