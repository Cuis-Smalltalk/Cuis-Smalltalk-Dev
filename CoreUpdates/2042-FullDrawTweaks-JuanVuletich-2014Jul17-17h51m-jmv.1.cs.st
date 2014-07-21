'From Cuis 4.2 of 25 July 2013 [latest update: #2040] on 17 July 2014 at 6:03:15.770499 pm'!

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/17/2014 17:53'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| br w |
	br _ currentMorph morphExtent.
	w _ br > (20@20)
		ifTrue: [ 10 ]
		ifFalse: [ 1 ].
	self
		frameAndFillRectangle: (0@0 corner: br)
		fillColor: Color red
		borderWidth: w
		borderColor: Color yellow.
	self line: 0@0 to: (br-w) width: w color: Color yellow.
	self line: (br x-w) @ 0 to: 0 @ (br y - w) width: w color: Color yellow.! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/17/2014 17:54'!
drawCurrentAsOutline

	(self isVisible: currentMorph morphBoundsInWorld) ifTrue: [
		self
			reverseRectangleBorder: (0 @ 0 extent: currentMorph morphExtent)
			borderWidth: 2 ]! !


!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/17/2014 17:57'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	"To replace #fullDrawOn:"
	| oldClipRect |
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.	"preferiria ni preguntar... para M3 al menos"
	(self isVisible: currentMorph morphFullBoundsInWorld) ifFalse: [
		self outOfMorph.
		^ self].

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
	(self isVisible: currentMorph morphBoundsInWorld) ifTrue: [
		currentMorph drawOn: self ].

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')"
	currentMorph hasSubmorphs ifTrue: [
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			currentOwnerIfClips _ currentMorph.
			oldClipRect _ clipRect.
			self setClipRect: (clipRect intersect: self clippingRectForCurrentMorph).
			self fullDraw: clipped.
			self setClipRect: oldClipRect.
			currentOwnerIfClips _ nil ].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullDraw: m ].
		].
	self outOfMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/17/2014 17:54'!
fullDrawHand: aHandMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	"
	This is starting to work:
		| c |
		c _ Display getCanvas into: self runningWorld.
		self runningWorld submorphsDo: [ :m | c fullDraw: m ].
	"

	"To replace #fullDrawOn:"
	self flag: #jmvVer3.
	aHandMorph visible ifFalse: [^ self].
	self into: aHandMorph.

	aHandMorph layoutSubmorphsIfNeeded.
	(self isVisible: aHandMorph morphFullBoundsInWorld) ifFalse: [
		self outOfMorph.
		^ self].

	"mnhhhh. raro. creo que mejor sacar..."
	aHandMorph isKnownFailing
		ifTrue: [ self drawCurrentAsError ]
		ifFalse: [ aHandMorph fullDrawHandOn: self ].

	self outOfMorph! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/17/2014 18:03'!
clippingRect
	"In own coordinates!!
	Return the bounds to which any submorphs should be clipped if the property is set"
	"Should be a region, like our shadow"
	self flag: #jmvVer2.
	^ 0@0 extent: self morphExtent
	
	"Quiero eliminar #clippingRect.
	Creo que hay que pedirle al canvas, pasando por param el morph, para que un BoundsFinder pueda usarse, etc.
	Ojo. podria usarse un boundsFinder tambien en M2"
	"
	morphBoundsInWorld
	morphFullBoundsInWorld
	senders de invalidateRect:
	es el mismo rect
	"! !

!methodRemoval: FormCanvas #drawAsError:!
FormCanvas removeSelector: #drawAsError:!
!methodRemoval: FormCanvas #drawAsOutline:!
FormCanvas removeSelector: #drawAsOutline:!
