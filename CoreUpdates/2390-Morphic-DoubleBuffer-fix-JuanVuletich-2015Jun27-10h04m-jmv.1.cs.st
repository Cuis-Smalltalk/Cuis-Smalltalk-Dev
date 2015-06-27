'From Cuis 4.2 of 25 July 2013 [latest update: #2389] on 27 June 2015 at 10:08:12.379418 am'!

!DisplayScreen methodsFor: 'other' stamp: 'jmv 6/27/2015 00:42'!
forceDisplayUpdate
"We stopped using it because it seems that with #deferUpdates: true (as supported by Linux / Windows / Mac OS)
what we need is #forceToScreen instead. You might check code containing 'forceDisplayUpdate'"

	"On platforms that buffer screen updates, force the screen to be updated immediately. On other platforms, or if the primitive is not implemented, do nothing."

	<primitive: 231>
	"do nothing if primitive fails"! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 6/27/2015 00:44'!
fillRects: rectangleList color: aColor
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."


	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: 0@0;
		clipRect: self viewBox;
		fillColor: aColor;
		combinationRule: Form over.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect"; forceDisplayUpdate" ].

	(Delay forMilliseconds: 150) wait! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 6/27/2015 00:55'!
tryDeferredUpdatingAndSetCanvasFor: aWorld
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, set a non-Disply canvas and answer false.."
	| properDisplay |
	
	"As this is the only sender of #deferUpdates: , this could be done in Morphic or image startup, and never efterwards..."
	(Display deferUpdates: true) ifNil: [
		"deferred updates not supported by the VM, do them in the image!!"
		self ensureNonDisplayCanvas.
		^ false].
	
	"if no canvas, or canvas was offscreen, from a platform that didnt support defers, then fix it"
	properDisplay _ canvas notNil and: [canvas drawsOnDisplay].
	properDisplay ifFalse: [
		aWorld viewBox: Display boundingBox.    "do first since it may clear canvas"
		self setCanvas: Display getCanvas.
	].
	^ true! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 6/27/2015 10:06'!
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
	deferredUpdateVMMode ifFalse: [
		"Drawing was done to off-Display canvas. Copy content to Display"
		canvas showAt: aWorld viewBox origin invalidRects: allDamage ].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	Display forceDamageToScreen: allDamage.

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].! !

