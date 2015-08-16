'From Cuis 4.2 of 25 July 2013 [latest update: #2459] on 16 August 2015 at 11:37:04.955928 am'!

!BitBltCanvas methodsFor: 'testing' stamp: 'jmv 8/15/2015 22:58'!
isCurrentMorphVisible

	| o aRectangle |
	currentMorph visible ifFalse: [ ^false ].
	aRectangle _ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.
	o _ self canvasOrigin.
	"Optimization"
	(aRectangle right + o x) < clipLeft			ifTrue: [^ false].
	(aRectangle left + o x) > (clipRight+1)		ifTrue: [^ false].
	(aRectangle bottom + o y) < clipTop		ifTrue: [^ false].
	(aRectangle top + o y) > (clipBottom+1)	ifTrue: [^ false].
	^ true
! !


!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 8/15/2015 21:08'!
unclippedSubmorphsReverseDo: aBlock
	| lastClippedIndex |
	lastClippedIndex _ submorphs size.
	self clipsLastSubmorph ifTrue: [
		lastClippedIndex _ lastClippedIndex - 1 ].
	lastClippedIndex to: 1 by: -1 do: [ :index |
		aBlock value: (submorphs at: index) ]! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/15/2015 21:21'!
drawCurrentAsOutline

	self isCurrentMorphVisible ifTrue: [
		self
			reverseRectangleBorder: currentMorph morphLocalBounds
			borderWidth: 2 ]! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/15/2015 23:04'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	| oldClipRect |
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [ Preferences cheapWindowReframe ]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].
	
	" --------------------------------------------------" 
	"Aca es donde hay que dibujar primero el morph, y despues los submorphs, sabiendo los bounds hallados al dibujar el morph!!!!!!!!!! (en M3)
	este es el mismo metodo que hace into: y outOfMorph    :D  "
	" --------------------------------------------------"
	"preferiria ni preguntar... para M3 al menos"
	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self ].

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')"
	currentMorph hasSubmorphs ifTrue: [
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self currentOwnerIfClips: currentMorph.
			oldClipRect _ self clipRect.
			self setClipRect: (oldClipRect intersect: self clippingRectForCurrentMorph).
			self fullDraw: clipped.
			self setClipRect: oldClipRect.
			self currentOwnerIfClips: nil ].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullDraw: m ].
		].
	self outOfMorph! !

!methodRemoval: BitBltCanvas #isFullMorphVisible:!
BitBltCanvas removeSelector: #isFullMorphVisible:!
!methodRemoval: BitBltCanvas #isMorphVisible:!
BitBltCanvas removeSelector: #isMorphVisible:!
