'From Cuis 5.0 [latest update: #4749] on 11 August 2021 at 8:02:30 pm'!

!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 8/11/2021 19:55:39'!
initializeWithOrigin: aPoint

	"Set up these only on initialization.
	This is safe (wrt walkbacks during world redraw) because a new instance is created
	on draw error. See #displayWorldSafely"
	currentTransformation _ MorphicTranslation withTranslation: aPoint.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ].
	drawingMorphStack
		ifNil: [ drawingMorphStack _ OrderedCollection with: nil ]
		ifNotNil: [ drawingMorphStack at: cti put: nil ].! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/11/2021 19:56:00'!
on: aForm over: aRectangle
	"Warning: aForm extent must equal aRectangle extent"

	^ self subclassToUse basicNew
		setForm: aForm preferSubPixelAntiAliasing: false;
		initializeWithOrigin: aRectangle topLeft negated.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/11/2021 19:56:06'!
onForm: aForm

	^ self subclassToUse basicNew
		setForm: aForm preferSubPixelAntiAliasing: false;
		initializeWithOrigin: `0@0`.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/11/2021 19:56:12'!
onFormWithSubPixelAntiAliasing: aForm

	^ self subclassToUse basicNew
		setForm: aForm preferSubPixelAntiAliasing: true;
		initializeWithOrigin: `0@0`.! !


!BitBltCanvas methodsFor: 'private' stamp: 'jmv 8/11/2021 19:58:00'!
setForm: aForm preferSubPixelAntiAliasing: aBoolean
	form _ aForm.
	engine _ BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	self newClipRect: nil.! !

!methodRemoval: MorphicCanvas #initialize stamp: 'jmv 8/11/2021 19:50:28'!
MorphicCanvas removeSelector: #initialize!
!methodRemoval: MorphicCanvas #setForm:preferSubPixelAntiAliasing: stamp: 'jmv 8/11/2021 19:58:10'!
MorphicCanvas removeSelector: #setForm:preferSubPixelAntiAliasing:!
!methodRemoval: MorphicCanvas #initializeWith:origin:preferSubPixelAntiAliasing: stamp: 'jmv 8/11/2021 19:54:52'!
MorphicCanvas removeSelector: #initializeWith:origin:preferSubPixelAntiAliasing:!
!methodRemoval: MorphicCanvas #initializeWith:origin: stamp: 'jmv 8/11/2021 19:56:26'!
MorphicCanvas removeSelector: #initializeWith:origin:!
