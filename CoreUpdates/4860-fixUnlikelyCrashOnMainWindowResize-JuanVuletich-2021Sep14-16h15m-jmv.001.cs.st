'From Cuis 5.0 [latest update: #4859] on 14 September 2021 at 4:21:17 pm'!

!DisplayScreen methodsFor: 'other' stamp: 'jmv 9/14/2021 16:20:04'!
forceToScreen: aRectangle
	"Force the given rectangular section of the Display to be copied to the screen. The primitive call does nothing if the primitive is not implemented. Used when the deferUpdates flag in the virtual machine is on; see #deferUpdates:.
	
	If for whatever reason, actual OS or hardware Display is smaller than us, don't go outside its bounds.
	This can sometimes happen, at least on MacOS, when frantically resizing the main OS Cuis window,
	while Morphic is busy drawing many heavy morphs: it could be observed than apparently in #displayWorld,
	after we were updated in #checkForNewScreenSize, MacOS window could be made smaller than aRectangle,
	and a hard crash due to an invalid memory access happened in this primitive."

	| platformDisplayExtent |
	platformDisplayExtent _ DisplayScreen actualScreenSize.
	self primShowRectLeft: (aRectangle left max: 0)
		right: (aRectangle right min: platformDisplayExtent x)
		top: (aRectangle top max: 0)
		bottom: (aRectangle bottom min: platformDisplayExtent y).
! !


!WorldMorph methodsFor: 'drawing' stamp: 'jmv 9/14/2021 16:20:41'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"repair world's damage on canvas"
	worldDamageRects _ canvas drawWorld: self repair: damageRecorder.

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
		canvas showAt: self viewBox origin invalidRects: allDamage ].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	DisplayScreen isDisplayExtentOk ifTrue: [
		Display forceDamageToScreen: allDamage ].

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].! !

