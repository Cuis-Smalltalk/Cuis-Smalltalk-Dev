'From Cuis 4.2 of 25 July 2013 [latest update: #2437] on 28 July 2015 at 8:54:03.545717 am'!

!WorldState methodsFor: 'drawing' stamp: 'jmv 7/28/2015 08:35'!
displayWorld: aWorld submorphs: submorphs
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time."
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

