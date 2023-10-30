'From Cuis 6.0 [latest update: #6062] on 30 October 2023 at 12:22:19 pm'!
!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'world form clipRect transformations currentTransformation cti currentMorph drawingMorphStack engine clippingMorphDisplayBounds ownerProcess '
	classVariableNames: 'ActiveSubclass '
	poolDictionaries: ''
	category: 'Morphic-Support'!

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 10/30/2023 11:32:31'!
ownerProcess
	^ownerProcess! !

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 10/30/2023 11:32:23'!
ownerProcess: aProcess
	ownerProcess := aProcess! !


!HybridCanvas methodsFor: 'accessing' stamp: 'jmv 10/30/2023 12:21:05'!
ownerProcess: aProcess
	ownerProcess := aProcess.
	vectorCanvas ownerProcess: aProcess.! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 10/30/2023 11:33:20'!
mainLoop

	| morphicCanvas morphicDamageRecorder delay |
        "If this platform supports deferred updates
		- Set Display to deferred update mode
		- Use a Canvas on Display
	otherwise
		- Set a non-Display canvas"
	self morphPosition: `0@0` extent: Display extent.
	morphicCanvas := (Display deferUpdates: true)
		ifNotNil: [ Display getMainCanvas ]
		ifNil: [ (Form extent: extent depth: Display depth) getCanvas ].
	morphicCanvas ownerProcess: Processor activeProcess.
	morphicCanvas world: self.
	morphicDamageRecorder := DamageRecorder new.
	"Set this as the main canvas"
	canvas := morphicCanvas.
	damageRecorder := morphicDamageRecorder.
	damageRecorder recordInvalidRect: self morphLocalBounds for: nil.
	self restoreDisplay.

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime := Time localMillisecondClock.
	lastAlarmTime := 0.

	delay := Delay forMilliseconds: 50.
	[
		self displayWorldOn: morphicCanvas.
		[
			"Set the main canvas on every iteration.
			This is needed if UI processes are interrupted, resumed, etc."
			canvas := morphicCanvas.
			damageRecorder := morphicDamageRecorder.
			self doOneCycleOn: morphicCanvas delay: delay.
			UISupervisor isUIProcessRunning ]
				whileTrue: []
	] on: Error, Halt do: [ :ex |
		"Handle a drawing error"
		(ex signalerContext findContextSuchThat: [ :context |
				(context receiver is: #Morph) and: [ context  selector = #drawOn: ]])
			ifNotNil: [ :context |
				context receiver drawingFails.
				"Install the old error handler, so we can re-raise the error.
				This means, open a Debugger.
				This will also create a new Morphic process. if the user closes the debugger."
				ex receiver error: ex description.
				"Create a new UI process, if the user proceeds in the debugger."
				UISupervisor newUIProcess ]
			ifNil: [ ex pass ]
	].! !


!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 10/30/2023 11:39:01'!
image: srcForm at: aPoint sourceRect: sourceRect
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resamples image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!!

	If No scaling and no rotation, use BitBlt instead (way faster). This is useful, for instance, for halo handles.
	We expect to replace them with vector graphics icons, anyway.

	But if drawing the world background, finding bounds is not needed. Use two BitBlts then:
	One to paint pixels, and a second one to set morphIds.
	In this way, performance is same as in HybridCanvas, and not too different from BitBltCanvas.
	This is especially important, the default World background is a Form!!"

	| p bits |

	"Special case for our world:
	- No bounds needed
	- currentTransformation is identity: No translation, no rotation, no scaling.
	BitBlt is enough, but we need two of them!!"
	currentMorph == world ifTrue: [
		p := (currentTransformation transform: aPoint) roundedHAFZ.
		auxBitBltEngine
			sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
			colorMap: (srcForm colormapIfNeededFor: form);
			clipRect: clipRect;
			sourceX: sourceRect left; sourceY: sourceRect top;
			destX: p x destY: p y width: sourceRect width height: sourceRect height;
			copyBits.
		bits := form bits.
		[
			form bits: engine morphIds.
			auxBitBltEngine
				sourceForm: nil;
				fillBitmap: (Bitmap with: (currentMorph morphId << 8 + 255));
				combinationRule: `Form over`;
				colorMap: nil;
				copyBits.
		] ensure: [ form bits: bits ].
		^self ].

	"Otherwise, we need to compute bounds. While we are at it, set morphID for affected pixels."
	self fillRectangle: (sourceRect translatedBy: aPoint) color: `Color gray alpha: 0.01`.
	currentTransformation isPureTranslation
		ifTrue: [
			p := (currentTransformation transform: aPoint) roundedHAFZ.
			auxBitBltEngine
				sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				sourceX: sourceRect left; sourceY: sourceRect top;
				destX: p x destY: p y width: sourceRect width height: sourceRect height;
				copyBits ]
		ifFalse: [
			| bb cellSize dstBox srcBox srcQuad |
			cellSize := 4.
			srcBox := sourceRect.
			dstBox := (currentTransformation externalBoundingRectOf: srcBox)
				encompassingIntegerRectangle.
			srcQuad := dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) roundedHAFZ ].
			dstBox := (currentTransformation externalBoundingRectOf: (srcBox translatedBy: aPoint-0.5))
				encompassingIntegerRectangle.
			bb := WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].! !

!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'ownerProcess world form clipRect transformations currentTransformation cti currentMorph drawingMorphStack engine clippingMorphDisplayBounds'
	classVariableNames: 'ActiveSubclass'
	poolDictionaries: ''
	category: 'Morphic-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
UISupervisor newUIProcess!

