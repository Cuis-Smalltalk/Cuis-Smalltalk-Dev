'From Cuis 4.2 of 25 July 2013 [latest update: #2033] on 14 July 2014 at 9:43:58.230092 am'!
!classDefinition: #FormCanvas category: #'Morphic-Support'!
Object subclass: #FormCanvas
	instanceVariableNames: 'clipRect form port shadowColor transformations currentTransformation cti currentOwnerIfClips currentMorph '
	classVariableNames: 'AccessProtect AuxBlitter AuxForm CachedForms '
	poolDictionaries: ''
	category: 'Morphic-Support'!

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/10/2014 14:19'!
drawAsError: aMorph
	"The morph (or one of its submorphs) had an error in its drawing method."
	| br w |
	br _ aMorph morphExtent.
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

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/10/2014 22:13'!
drawAsOutline: aMorph

	(self isVisible: aMorph morphBoundsInWorld) ifTrue: [
		self
			reverseRectangleBorder: (0 @ 0 extent: aMorph morphExtent)
			borderWidth: 2 ]! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/11/2014 11:21'!
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
		ifTrue: [ self drawAsError: aHandMorph ]
		ifFalse: [ aHandMorph fullDrawHandOn: self ].

	self outOfMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/11/2014 11:16'!
outOfLocation

	cti _ cti - 1.
	currentTransformation _ transformations at: cti! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/14/2014 09:30'!
outOfMorph

	self outOfLocation.
	currentMorph _ currentMorph owner.
	currentOwnerIfClips _ currentMorph ifNotNil: [ :o |
		currentMorph owner ifNotNil: [ :oo |
			oo clipsSubmorphs ifTrue: [ oo ]]]! !


!Morph methodsFor: 'testing' stamp: 'jmv 7/10/2014 13:33'!
isOwnedByHand
	^owner is: #HandMorph! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:29'!
fullDrawHandOn: aCanvas
	| shadowForm |
	"A HandMorph has unusual drawing requirements:
		1. the hand itself (i.e., the cursor) appears in front of its submorphs
		2. morphs being held by the hand cast a shadow on the world/morphs below
	The illusion is that the hand plucks up morphs and carries them above the world."

	submorphs isEmpty ifTrue: [^ self drawOn: aCanvas].  "just draw the hand itself"

	Preferences cheapWindowReframe ifFalse: [
		"Note: We use a shadow form here to prevent drawing
		overlapping morphs multiple times using the transparent
		shadow color."
		shadowForm _ self submorphsShadowForm.

		"draw shadows"
		aCanvas stencil: shadowForm at: shadowForm offset  + self shadowOffset color: (Color black alpha: 0.5) ].
	
	"draw morphs in front of shadows"
	submorphs reverseDo: [ :m | aCanvas fullDraw: m ].
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"! !


!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/11/2014 11:21'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	"To replace #fullDrawOn:"
	| boundingRect oldClipRect |
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	aMorph layoutSubmorphsIfNeeded.
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
			boundingRect _ currentTransformation displayBoundsOfTransformOf: aMorph clippingRect.
			self setClipRect: (clipRect intersect: boundingRect) ].
		aMorph privateSubmorphs reverseDo: [ :m |
			self fullDraw: m ].
		oldClipRect ifNotNil: [
			self setClipRect: oldClipRect ].
		].
	self outOfMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/14/2014 09:30'!
into: aMorph

	self intoLocation: aMorph location.
	currentOwnerIfClips _ currentMorph ifNotNil: [
		currentMorph clipsSubmorphs ifTrue: [ currentMorph ]].
	currentMorph _ aMorph! !


!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 7/10/2014 22:19'!
hasSubmorphs
	^submorphs size ~= 0! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 14:30'!
submorphsShadowForm
	"Return a 1-bit shadow of my submorphs.  Assumes submorphs is not empty"
	| bnds canvas |
	bnds _ Rectangle merging: (submorphs collect: [:m | m morphFullBoundsInWorld]).
	canvas _ FormCanvas forShadowOver: bnds.
	canvas into: self.
	submorphs reverseDo: [ :m | canvas fullDraw: m ].
	^ canvas form offset: bnds topLeft - self morphPositionInWorld! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 7/10/2014 14:04'!
displayWorld: aWorld submorphs: submorphs
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvasFor: aWorld.

	"repair world's damage on canvas"
	worldDamageRects _ self drawInvalidAreasWorld: aWorld submorphs: submorphs.

	"Check which hands need to be drawn (they are not the hardware mouse pointer)"
	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"Draw hands (usually carying morphs) onto world canvas"
	canvas newClipRect: nil.
	handsToDraw reverseDo: [ :h | canvas fullDrawHand: h ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode
		ifTrue: [
			"Drawing was done on Display object. Just show it"
			Display forceDamageToScreen: allDamage.
			]
		ifFalse: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			canvas showAt: aWorld viewBox origin invalidRects: allDamage.
			].

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].
	Display deferUpdates: false; forceDisplayUpdate! !

!methodRemoval: HandMorph #fullDrawOn:!
HandMorph removeSelector: #fullDrawOn:!
!methodRemoval: Morph #drawErrorOn:!
Morph removeSelector: #drawErrorOn:!
!methodRemoval: Morph #fullDrawOn:!
Morph removeSelector: #fullDrawOn:!
!methodRemoval: FormCanvas #additionalClipRect:while:!
FormCanvas removeSelector: #additionalClipRect:while:!
!methodRemoval: FormCanvas #drawSubmorphsOf:!
FormCanvas removeSelector: #drawSubmorphsOf:!
!methodRemoval: FormCanvas #outOf:!
FormCanvas removeSelector: #outOf:!
!classDefinition: #FormCanvas category: #'Morphic-Support'!
Object subclass: #FormCanvas
	instanceVariableNames: 'clipRect form port shadowColor transformations currentTransformation cti currentMorph currentOwnerIfClips'
	classVariableNames: 'AccessProtect AuxBlitter AuxForm CachedForms'
	poolDictionaries: ''
	category: 'Morphic-Support'!
