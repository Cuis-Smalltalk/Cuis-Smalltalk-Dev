'From Cuis 5.0 [latest update: #4810] on 27 August 2021 at 5:41:38 pm'!

!DisplayScreen methodsFor: 'accessing' stamp: 'jmv 8/27/2021 17:20:52'!
getMainCanvas
	"Return a Canvas that can be used to draw onto the receiver"
	^MorphicCanvas withVectorEnginePluginOnForm: self! !


!WorldMorph methodsFor: 'canvas' stamp: 'jmv 8/27/2021 17:26:47'!
setMainCanvas
	self clearCanvas.						"Deallocate before allocating could mean less memory stress."
	self setCanvas: Display getMainCanvas.
	damageRecorder _ DamageRecorder new.
	self redrawNeeded! !


!MorphicCanvas methodsFor: 'testing' stamp: 'jmv 8/27/2021 16:04:07'!
usesVectorEnginePlugin
	^false! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/27/2021 17:20:46'!
withVectorEnginePluginOnForm: aForm
	"Note: Only one instance using VectorEnginePlugin should be acive at a time:
	the plugin holds numeric parameters that are not passed again on every call."

	^ self subclassToUse basicNew
		setPluginAndForm: aForm;
		initializeWithTranslation: `0@0`.! !


!BitBltCanvas methodsFor: 'private' stamp: 'jmv 8/27/2021 17:17:56'!
setPluginAndForm: aForm
	"nil means use default kind of anti aliasing"

	form _ aForm.
	engine _ BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	self newClipRect: nil.! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/27/2021 16:06:32'!
okayToRotateEasily
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver."

	^ self hasOwnLocation and: [self world canvas usesVectorEnginePlugin]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/27/2021 16:06:44'!
okayToScaleEasily
	"Answer whether it is appropriate for a scale handle to be shown for the receiver."

	^ self hasOwnLocation and: [self world canvas usesVectorEnginePlugin]! !


!WorldMorph methodsFor: 'geometry' stamp: 'jmv 8/27/2021 17:27:32'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	self setMainCanvas! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 8/27/2021 17:23:17'!
mainLoop

	
	self clearWaitDelay.
	canvas isNil ifTrue: [
		self setCanvas: Display getMainCanvas.
		damageRecorder _ DamageRecorder new ].
	self redrawNeeded.
	[
		self doOneCycle.
		true ]
			whileTrue: []! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 8/27/2021 17:27:28'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[self displayWorld] ifErrorOrHalt: [ :err :rcvr |
		"Handle a drawing error"
		canvas currentMorphDrawingFails.
		self setMainCanvas.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	]! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 8/27/2021 17:25:41'!
tryDeferredUpdatingAndSetCanvas
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
		self morphPosition: `0@0` extent: Display extent.
		self setCanvas: Display getMainCanvas.
	].
	^ true! !


!WorldMorph class methodsFor: 'instance creation' stamp: 'jmv 8/27/2021 17:23:29'!
newWorld
	"
[
	UISupervisor stopUIProcess.
	UISupervisor spawnNewMorphicProcessFor: WorldMorph newWorld
] fork.
	"
	| w |
	w _ self new.
	w morphPosition: `0@0` extent: Display extent.
	w setCanvas: Display getMainCanvas.
	w handsDo: [ :h |
		h privateOwner: w ].
	^w! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/27/2021 17:38:53'!
activeSubclass: aMorphicCanvasSubclass
	ActiveSubclass _ aMorphicCanvasSubclass.
	UISupervisor ui setMainCanvas.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/27/2021 17:07:00'!
onForm: aForm over: aRectangle
	"Warning: aForm extent must equal aRectangle extent"

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: false;
		initializeWithTranslation: aRectangle topLeft negated.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/27/2021 17:06:54'!
onForm: aForm translation: aPoint

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: false;
		initializeWithTranslation: aPoint.! !

!methodRemoval: MorphicCanvas class #isVectorGraphicsUsedForAllRendering stamp: 'jmv 8/27/2021 17:32:16'!
MorphicCanvas class removeSelector: #isVectorGraphicsUsedForAllRendering!
!methodRemoval: MorphicCanvas class #onFormWithWholePixelAntiAliasing: stamp: 'jmv 8/27/2021 17:32:16'!
MorphicCanvas class removeSelector: #onFormWithWholePixelAntiAliasing:!
!methodRemoval: MorphicCanvas class #isVectorGraphicsActive stamp: 'jmv 8/27/2021 17:32:23'!
MorphicCanvas class removeSelector: #isVectorGraphicsActive!
!methodRemoval: MorphicCanvas class #isVectorGraphicsPluginActive stamp: 'jmv 8/27/2021 17:32:16'!
MorphicCanvas class removeSelector: #isVectorGraphicsPluginActive!
!methodRemoval: MorphicCanvas class #onFormWithWholePixelAntiAliasing:translation: stamp: 'jmv 8/27/2021 17:32:23'!
MorphicCanvas class removeSelector: #onFormWithWholePixelAntiAliasing:translation:!
!methodRemoval: WorldMorph #setCanvas stamp: 'jmv 8/27/2021 17:40:20'!
WorldMorph removeSelector: #setCanvas!
