'From Cuis 5.0 [latest update: #4896] on 4 October 2021 at 9:41:13 am'!

!Preferences class methodsFor: 'misc' stamp: 'jmv 10/4/2021 09:35:45'!
cacheDisplayContentWhenMovingMorphs
	"Set to false to save buffer memory, at the cost of redrawing morphs belo hand each time."

	^ self
		valueOfFlag: #cacheDisplayContentWhenMovingMorphs
		ifAbsent: [ true ].! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/4/2021 09:27:57'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	| prevTranslation |
	prevTranslation _ location translation.
	location _ location withTranslation: aPoint.
	"Ask if translation effectively changed, after possible conversion to 32 bit Float in AffineTransformation. "
	location translation = prevTranslation ifFalse: [
		self isDrawnBySoftware ifTrue: [
			Preferences cacheDisplayContentWhenMovingMorphs
				ifTrue: [ self needsRedraw: true ]
				ifFalse:  [self redrawNeeded ]]].! !


!WorldMorph methodsFor: 'drawing' stamp: 'jmv 10/4/2021 09:40:33'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time. ?? revisar."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"Restore world canvas under hands and their carried morphs"
	Preferences cacheDisplayContentWhenMovingMorphs ifTrue: [
		hands do: [ :h | h restoreSavedPatchOn: canvas ]].

	"repair world's damage on canvas"
	worldDamageRects _ canvas drawWorld: self repair: damageRecorder.
	"worldDamageRects do: [ :r | Display border: r width: 3 fillColor: Color random ].	 'Debugging Aid'."
	allDamage _ Rectangle merging: worldDamageRects.
	canvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: worldDamageRects do: [ :h |
		Preferences cacheDisplayContentWhenMovingMorphs
			ifTrue: [ allDamage _ (h savePatchFrom: canvas) quickMerge: allDamage ]
			ifFalse: [ allDamage _ h displayFullBoundsForPatch quickMerge: allDamage ].
		canvas fullDrawHand: h .
		h needsRedraw: false ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		"Drawing was done to off-Display canvas. Copy content to Display"
		canvas showAt: self viewBox origin invalidRect: allDamage ].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	allDamage ifNotNil: [
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: allDamage ]].! !

