'From Cuis 4.2 of 25 July 2013 [latest update: #2027] on 8 July 2014 at 12:18:52.53242 pm'!

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/3/2014 16:53'!
drawSubmorphsOf: aMorph
	"Display submorphs back to front"

	aMorph hasSubmorphs ifFalse: [ ^ self ].

	"No Clipping? Just draw and we're done."
	aMorph clipsSubmorphs ifFalse: [
		aMorph privateSubmorphs reverseDo: [ :m |
			self fullDraw: m ].
		^self ].

	"Draw with additional clipping done by aMorph."
	"Ojo, en M3 es un rect, no un quad. el clipping final se hace a los pixels usados por aMorph... (hacerlo!!)"
	self additionalClipRect: aMorph clippingRect while: [
		aMorph privateSubmorphs reverseDo: [ :m |
			self fullDraw: m ]]! !


!Morph methodsFor: 'private' stamp: 'jmv 7/3/2014 10:12'!
privateSubmorphs
	^submorphs! !


!Morph methodsFor: 'drawing' stamp: 'jmv 7/3/2014 10:13'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"
	"Draw receiver itself"
	| h |
	h _ self world ifNotNil: [ :w | w activeHand ].
	(owner == h and: [ Preferences cheapWindowReframe ])
		ifTrue: [
			(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [
				aCanvas
					reverseRectangleBorder: (0 @ 0 extent: self morphExtent)
					borderWidth: 2 ]]
		ifFalse: [
			(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [ self drawOn: aCanvas ].
			aCanvas drawSubmorphsOf: self ]! !


!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 7/3/2014 10:14'!
magnifiedForm
	| srcRect form neededExtent |
	lastPos _ self sourcePoint.
	srcRect _ self sourceRectFrom: lastPos.
	((srcRect intersects: self morphBoundsInWorld) and: [ RecursionLock == nil ])
		ifTrue: [
			RecursionLock _ self.
			"try to reuse form if appropriate"
			auxCanvas _ (auxCanvas notNil and: [ auxCanvas extent = srcExtent ])
				ifTrue: [
					"Just in case we go out of the Display"
					srcRect origin > (0@0) ifFalse: [
						auxCanvas form fillBlack ].
					FormCanvas on: auxCanvas form over: srcRect ]
				ifFalse: [ FormCanvas depth: 32 over: srcRect ].
			self world drawOn: auxCanvas.
			auxCanvas drawSubmorphsOf: self world.
			form _ auxCanvas form.
			RecursionLock _ nil]
		ifFalse: [
			"cheaper method if the source is not occluded"
			form _ Display copy: srcRect].
	"smooth if non-integer scale"
	neededExtent _ (srcExtent * magnification ) truncated.
	(magnifiedForm isNil or: [ magnifiedForm extent ~=  neededExtent ])
		ifTrue: [ magnifiedForm _ Form extent: neededExtent depth: 32 ].
	(WarpBlt toForm: magnifiedForm)
		sourceForm: form;
		colorMap: (form colormapIfNeededFor: magnifiedForm);
		cellSize: (magnification isInteger ifTrue: [1] ifFalse: [2]);  "installs a new colormap if cellSize > 1"
		combinationRule: 3;
		copyQuad: form boundingBox innerCorners toRect: magnifiedForm boundingBox.
	^magnifiedForm.! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/3/2014 10:14'!
fullDrawOn: aCanvas
	| shadowForm |
	"A HandMorph has unusual drawing requirements:
		1. the hand itself (i.e., the cursor) appears in front of its submorphs
		2. morphs being held by the hand cast a shadow on the world/morphs below
	The illusion is that the hand plucks up morphs and carries them above the world."
	"Note: This version does not cache an image of the morphs being held by the hand.
	 Thus, it is slower for complex morphs, but consumes less space."

	submorphs isEmpty ifTrue: [^ self drawOn: aCanvas].  "just draw the hand itself"

	Preferences cheapWindowReframe ifFalse: [
		"Note: We use a shadow form here to prevent drawing
		overlapping morphs multiple times using the transparent
		shadow color."
		shadowForm _ self submorphsShadowForm.

		"draw shadows"
		aCanvas stencil: shadowForm at: shadowForm offset  + self shadowOffset color: (Color black alpha: 0.5) ].
	
	"draw morphs in front of shadows"
	aCanvas drawSubmorphsOf: self.
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/3/2014 10:14'!
submorphsShadowForm
	"Return a 1-bit shadow of my submorphs.  Assumes submorphs is not empty"
	| bnds canvas |
	bnds _ Rectangle merging: (submorphs collect: [:m | m morphFullBoundsInWorld]).
	canvas _ FormCanvas forShadowOver: bnds.
	canvas into: self.
	canvas drawSubmorphsOf: self.
	^ canvas form offset: bnds topLeft - self morphPositionInWorld! !

!methodRemoval: Morph #drawSubmorphsOn:!
Morph removeSelector: #drawSubmorphsOn:!
