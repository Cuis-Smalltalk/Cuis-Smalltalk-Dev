'From Cuis 5.0 of 7 November 2016 [latest update: #3036] on 17 January 2017 at 11:13:18 am'!

!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 1/17/2017 10:51:52'!
initialExtent
	^ RealEstateAgent standardWindowExtent * 3 // 2! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 1/17/2017 11:12:27'!
maximumUsableAreaInWorld: aWorldOrNil

	| allowedArea |
	allowedArea _ Display boundingBox.
	aWorldOrNil ifNotNil: [
		allowedArea _ allowedArea intersect: aWorldOrNil viewBox.
		aWorldOrNil taskbar ifNotNil: [ :tb |
			allowedArea _ (allowedArea areasOutside: tb morphBoundsInWorld) first ]].
	^allowedArea
! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 1/17/2017 11:00:35'!
privateExtent: newExtent

	^ (super privateExtent: newExtent)
		ifTrue: [
			self buildMagnifiedBackgroundImage.
			worldState clearCanvas ]; yourself! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 1/17/2017 10:56:23'!
viewBox

	^ worldState
		ifNotNil: [
			 0@0 extent: extent ]
		ifNil: [
			self world viewBox ]! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 1/17/2017 11:05:24'!
restoreMorphicDisplay
	DisplayScreen startUp.
	self
		morphExtent: Display extent;
		handsDo: [ :h | h visible: true ];
		fullRepaintNeeded.
	WorldState addDeferredUIMessage: [ Cursor normal activateCursor ]! !


!PasteUpMorph class methodsFor: 'instance creation' stamp: 'jmv 1/17/2017 11:04:44'!
newWorld
	"
[
	ProjectX stopUIProcess.
	ProjectX spawnNewMorphicProcessFor: PasteUpMorph newWorld
] fork.
	"
	| w ws |
	w _ self new.
	ws _ WorldState new.
	w worldState: ws.
	w morphPosition: 0@0 extent: Display extent.
	ws setCanvas: Display getCanvas.
	w borderWidth: 0.
	ws handsDo: [ :h |
		h privateOwner: w ].
	^w! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 1/17/2017 10:57:47'!
ensureNonDisplayCanvas
	(canvas isNil or: [
		canvas drawsOnDisplay or: [
		(canvas extent ~= world morphExtent) or: [
		canvas form depth ~= Display depth]]]) ifTrue: [
			"allocate a new offscreen canvas the size of the window"
			self setCanvas: (BitBltCanvas withExtent: world morphExtent depth: Display depth)]! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 1/17/2017 11:05:18'!
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
		aWorld morphPosition: 0@0 extent: Display extent.
		self setCanvas: Display getCanvas.
	].
	^ true! !

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

!methodRemoval: WorldState #viewBox!
WorldState removeSelector: #viewBox!
!methodRemoval: WorldState #viewBox:!
WorldState removeSelector: #viewBox:!
!methodRemoval: DebuggerWindow #initialFrameIn:!
DebuggerWindow removeSelector: #initialFrameIn:!
!methodRemoval: PasteUpMorph #viewBox:!
PasteUpMorph removeSelector: #viewBox:!
