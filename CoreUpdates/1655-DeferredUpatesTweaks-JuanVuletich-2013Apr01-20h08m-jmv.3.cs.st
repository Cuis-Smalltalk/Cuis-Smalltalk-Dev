'From Cuis 4.1 of 12 December 2012 [latest update: #1654] on 1 April 2013 at 8:55:46 pm'!
!classDefinition: #DisplayScreen category: #'Graphics-Display Objects'!
Form subclass: #DisplayScreen
	instanceVariableNames: ''
	classVariableNames: 'DeferringUpdates '
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!WorldState methodsFor: 'canvas' stamp: 'jmv 4/1/2013 20:31'!
clearCanvas
	canvas _ nil.
	damageRecorder _ nil! !

!WorldState methodsFor: 'canvas' stamp: 'jmv 4/1/2013 20:32'!
ensureNonDisplayCanvas
	(canvas isNil or: [
		canvas drawsOnDisplay or: [
		(canvas extent ~= viewBox extent) or: [
		canvas form depth ~= Display depth]]]) ifTrue: [
			"allocate a new offscreen canvas the size of the window"
			self setCanvas: (FormCanvas withExtent: viewBox extent depth: Display depth)]! !

!WorldState methodsFor: 'canvas' stamp: 'jmv 4/1/2013 20:30'!
setCanvas: aFormCanvas
	canvas _ aFormCanvas.
	aFormCanvas ifNotNil: [
		aFormCanvas into: world ].
	damageRecorder
		ifNil: [ damageRecorder _ DamageRecorder new]
		ifNotNil: [ damageRecorder doFullRepaint]! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 4/1/2013 20:54'!
tryDeferredUpdatingAndSetCanvasFor: aWorld
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, set a non-Disply canvas and answer false.."
	| properDisplay |
	
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


!DisplayScreen methodsFor: 'other' stamp: 'jmv 4/1/2013 20:12'!
deferUpdates: aBoolean
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen.  To actually make them visible, call #forceToScreen: .
	If this underlying platform does not support deferred updates, this primitive will fail. Answer  nil if it fails.
	Most platforms do support this functionality. You can turn it off for playing with Display directly."

	"Enable this to act as if the VM didn't support defer updates, even when it does"
	"true ifTrue: [ ^nil ]."

	"Note: If we disable VM defer updates (with this &false), but answer notNil, the the Morphic workaround is not used,
	and you get a lot of flicking."
	^self primitiveDeferUpdates: aBoolean "& false"! !

!DisplayScreen methodsFor: 'other' stamp: 'jmv 4/1/2013 20:12'!
forceToScreen: aRectangle
	"Force the given rectangular section of the Display to be copied to the screen. The primitive call does nothing if the primitive is not implemented. Used when the deferUpdates flag in the virtual machine is on; see #deferUpdates:."

	self primShowRectLeft: aRectangle left
		right: aRectangle right
		top: aRectangle top
		bottom: aRectangle bottom.
! !


!PasteUpMorph methodsFor: 'caching' stamp: 'jmv 4/1/2013 20:31'!
releaseCachedState
	super releaseCachedState.
	backgroundImage _ nil.
	self isWorldMorph ifTrue: [
		self cleanseStepList.
		worldState clearCanvas ]! !

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 4/1/2013 20:31'!
morphExtent: aPoint
	"In our own coordinates"

	self flag: #jmvVer2.
	extent = aPoint ifFalse: [
		self redrawNeeded.
		extent _ aPoint.
		self buildMagnifiedBackgroundImage.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = self morphBoundsInWorld ifFalse: [
				worldState clearCanvas.
				worldState viewBox: self morphBoundsInWorld ]]]! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 4/1/2013 20:32'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState clearCanvas ].
		worldState viewBox: newViewBox ].
	location setPosition: (owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ]).
	extent _ newViewBox extent! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 4/1/2013 20:54'!
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
	handsToDraw reverseDo: [ :h | canvas fullDraw: h ].

	"*make this true to flash damaged areas for testing*"
	Preferences debugShowDamage ifTrue: [ aWorld flashRects: allDamage ].

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

!methodRemoval: WorldState #assuredNonDisplayCanvas!
WorldState removeSelector: #assuredNonDisplayCanvas!
!methodRemoval: WorldState #canvas:!
WorldState removeSelector: #canvas:!
!methodRemoval: WorldState #doDeferredUpdatingFor:!
WorldState removeSelector: #doDeferredUpdatingFor:!
!classDefinition: #DisplayScreen category: #'Graphics-Display Objects'!
Form subclass: #DisplayScreen
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!
