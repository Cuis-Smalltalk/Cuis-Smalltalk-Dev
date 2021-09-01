'From Cuis 5.0 [latest update: #4822] on 31 August 2021 at 5:33:26 pm'!

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 8/31/2021 17:05:43'!
resetCanvas
	"To be called in case of possible inconsistency due to an exception during drawing.
	See #displayWorldSafely"

	canvas resetCanvas.
	damageRecorder _ DamageRecorder new.
	self redrawNeeded! !


!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 8/31/2021 17:05:32'!
resetCanvas
	"To be called in case of possible inconsistency due to an exception during drawing.
	See #displayWorldSafely"

	cti _ 1.
	currentTransformation _ transformations at: 1.! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 8/31/2021 17:06:38'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[self displayWorld] on: Error, Halt do: [ :err :rcvr |
		"Handle a drawing error"
		canvas currentMorphDrawingFails.
		"Creating a new canvas here could be dangerous, as code signaling the exception will be resumed."
		self resetCanvas.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	]! !


!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 8/31/2021 17:04:10'!
initializeWithTranslation: aPoint
	"Set up these only on initialization."

	currentTransformation _ MorphicTranslation withTranslation: aPoint.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ].
	drawingMorphStack
		ifNil: [ drawingMorphStack _ OrderedCollection with: nil ]
		ifNotNil: [ drawingMorphStack at: cti put: nil ].! !

!methodRemoval: BlockClosure #ifErrorOrHalt: stamp: 'jmv 8/31/2021 17:32:19'!
BlockClosure removeSelector: #ifErrorOrHalt:!
