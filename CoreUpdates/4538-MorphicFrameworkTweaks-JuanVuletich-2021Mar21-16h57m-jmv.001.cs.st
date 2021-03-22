'From Cuis 5.0 [latest update: #4537] on 21 March 2021 at 5:16:51 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 3/21/2021 13:05:45'!
restorePatch: savedPatch bounds: savedBounds
	"Argument must previously be obtained by calling #savePatch:bounds:
	Senders should not assume anything about the returned object, except that it will not be nil."

self flag: #jmvHacks.
"Podriamos usar un BitBlt guardado. Este metodo termina haciendo demasiadas cosas... (innecesarias)"
	self
		image: savedPatch
		at: savedBounds origin
		sourceRect: (0@0 extent: savedBounds extent)! !


!Morph methodsFor: 'drawing' stamp: 'jmv 3/21/2021 12:44:53'!
imageForm: extent depth: depth
	| canvas |
	canvas _ MorphicCanvas depth: depth over: (self morphPosition extent: (self morphExtent min: extent)) encompassingIntegerRectangle.
	canvas fullDraw: self.
	^ canvas form divideByAlpha! !

!Morph methodsFor: 'geometry' stamp: 'jmv 3/21/2021 07:20:22'!
displayFullBounds
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world)."

	| answer |
	answer _ self displayBounds.
	answer ifNil: [ ^nil ].
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m |
			answer _ answer quickMerge: m displayFullBounds ]].
	^answer encompassingIntegerRectangle! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 3/21/2021 17:07:13'!
needsToBeDrawn
	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor. This is the case if it (a) it is a remote hand, (b) it is showing a temporary cursor, or (c) it is not empty and there are any visible submorphs. If using the software cursor, ensure that the hardware cursor is hidden."
	"Details:  Return true if this hand has a saved patch to ensure that is is processed by the world. This saved patch will be deleted after one final display pass when it becomes possible to start using the hardware cursor again. This trick gives us one last display cycle to allow us to remove the software cursor from the display."
	"Note. We draw the hand as a regular morph (using #drawOn:), disabling the hardware cursor, when we carry submorphs. The reason is to lock the mouse pointer and the carried morph together. Otherwhise the carried morph would lag behind the mouse pointer.
	This method answers whether the regular #drawOn: drawing mechanism is used for us.
	
	Check senders. Hand drawing is handled explicitly by the world, because the Hand is not a submorph of the world!!"
	| blankCursor |
	(prevFullBounds notNil or: [
		submorphs anySatisfy: [ :ea |
			ea visible ]]) ifTrue: [
		"using the software cursor; hide the hardware one"
		blankCursor _ Cursor cursorAt: #blankCursor.
		Cursor currentCursor == blankCursor ifFalse: [ blankCursor activateCursor ].
		^ true ].
	^ false.! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 3/21/2021 17:06:17'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."
	hasChanged _ false.
	prevFullBounds ifNotNil: [
		aCanvas restorePatch: savedPatch bounds: prevFullBounds.
		submorphs notEmpty ifTrue: [ ^ self ].
		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self invalidateDisplayRect: (self morphPosition extent: extent) for: nil.
		Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
		"show hardware cursor"
		prevFullBounds _ nil ].! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 3/21/2021 07:20:15'!
displayFullBounds	
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around)."
	| fullBounds |
	fullBounds _ self displayBounds.
	self submorphsDo: [ :m |
		fullBounds _ fullBounds quickMerge: m displayFullBounds].
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Include also the rectangle updated to current hand position."
			fullBounds _ fullBounds quickMerge: (fullBounds translatedBy: self morphPosition-lastPos) ].
	^fullBounds encompassingIntegerRectangle! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 3/21/2021 07:27:48'!
mainLoop

	
	self clearWaitDelay.
	[
		self doOneCycle.
		true ]
			whileTrue: []! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 3/21/2021 07:28:10'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[self displayWorld] ifError: [ :err :rcvr |
		"Handle a drawing error"
		canvas currentMorphDrawingFails.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	]! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 3/21/2021 17:16:08'!
savePatch: prevSavedPatch bounds: aRectangle
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."
	| savedPatch bb prevExtent extent |
	savedPatch _ prevSavedPatch.
	prevExtent _ 0@0.
	((savedPatch is: #Form) and: [
		prevExtent _ savedPatch extent.
		prevExtent >= aRectangle extent]) 
			ifTrue: [
				savedPatch offset: 0@0 ]
			ifFalse: [
				"allocate new patch form if needed"
				extent _ aRectangle extent max: prevExtent.
				extent _ (extent x // 64 + 2 * 64) @ (extent y + 64).						"Make it slightly larger, and even width."
				savedPatch _ Form extent: extent depth: form depth ].
	bb _ BitBlt toForm: savedPatch.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height;
		copyBits.
	^savedPatch! !

!methodRemoval: MorphicCanvas #restorePatch: stamp: 'jmv 3/21/2021 16:58:02'!
MorphicCanvas removeSelector: #restorePatch:!
