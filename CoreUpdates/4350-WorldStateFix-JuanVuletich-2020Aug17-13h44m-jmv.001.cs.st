'From Cuis 5.0 [latest update: #4349] on 17 August 2020 at 1:45:32 pm'!

!PasteUpMorph methodsFor: 'initialization' stamp: 'jmv 8/17/2020 13:41:01'!
setCanvas
	worldState setCanvas! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 8/17/2020 13:41:08'!
setCanvas
	self setCanvas: Display getCanvas.
	damageRecorder _ DamageRecorder new.! !


!StrikeFont class methodsFor: 'class cached access' stamp: 'jmv 8/17/2020 12:48:37'!
releaseClassState
	"Do not remove them in every image save (#releaseClassCachedState) to save startup time."
	"
	StrikeFont releaseClassState
	"
	"Deallocate synthetically derived copies of base fonts to save space"
	self allInstancesDo: [ :sf | sf reset ]! !


!Morph class methodsFor: 'initialize-release' stamp: 'jmv 8/17/2020 13:22:58'!
releaseClassState

	self allInstancesDo: [ :each |
		each releaseCachedState.
		each clearId ].
	LastMorphId _ nil.! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 8/17/2020 13:41:41'!
privateExtent: newExtent

	^ (super privateExtent: newExtent)
		ifTrue: [
			self buildMagnifiedBackgroundImage.
			worldState ifNotNil: [
				worldState setCanvas ]];
		yourself! !

!PasteUpMorph methodsFor: 'interaction loop' stamp: 'jmv 8/17/2020 13:41:37'!
mainLoop

	
	self clearWaitDelay.
	self setCanvas.
	[
		self doOneCycle.
		Processor yield.
		true ]
			whileTrue: []! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 8/17/2020 13:42:46'!
clearCanvas
	canvas _ nil.
	damageRecorder _ nil.! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 8/17/2020 13:41:43'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[world displayWorld] ifError: [:err :rcvr |
		"Handle a drawing error"
		| errCtx errMorph |
		errCtx _ thisContext.
		[
			errCtx _ errCtx sender.
			"Search the sender chain to find the morph causing the problem"
			[errCtx notNil and: [ (errCtx receiver is: #Morph) not ]] 
				whileTrue:[errCtx _ errCtx sender].
			"If we're at the root of the context chain then we have a fatal drawing problem"
			errCtx ifNil:[^self handleFatalDrawingError: err].
			errMorph _ errCtx receiver.
			"If the morph causing the problem has already the #drawError flag set,
			then search for the next morph above in the caller chain."
			errMorph isKnownFailing
		] whileTrue.
		errMorph drawingFails.
		self setCanvas.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	]! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/17/2020 13:41:33'!
activeSubclass: aMorphicCanvasSubclass
	ActiveSubclass _ aMorphicCanvasSubclass.
	PasteUpMorph allInstancesDo: [ :w | w setCanvas ]! !

