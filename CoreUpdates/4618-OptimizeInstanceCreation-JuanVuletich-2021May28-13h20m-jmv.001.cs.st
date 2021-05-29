'From Cuis 5.0 [latest update: #4613] on 28 May 2021 at 2:14:09 pm'!

!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 5/28/2021 10:08:06'!
initializeWith: aForm origin: aPoint preferSubPixelAntiAliasing: aBoolean
	self initialize.
	self setForm: aForm preferSubPixelAntiAliasing: aBoolean.

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

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 5/28/2021 10:03:45'!
setForm: aForm preferSubPixelAntiAliasing: aBoolean
	form _ aForm.! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 5/28/2021 10:06:37'!
onFormWithSubPixelAntiAliasing: aForm

	^ self subclassToUse basicNew
		initializeWith: aForm origin: `0@0` preferSubPixelAntiAliasing: true! !


!BitBltCanvas methodsFor: 'private' stamp: 'jmv 5/28/2021 10:08:10'!
setForm: aForm preferSubPixelAntiAliasing: aBoolean
	super setForm: aForm preferSubPixelAntiAliasing: aBoolean.
	engine _ BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	self newClipRect: nil.! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 5/28/2021 10:05:53'!
on: aForm over: aRectangle
	"Warning: aForm extent must equal aRectangle extent"

	^ self subclassToUse basicNew
		initializeWith: aForm origin: aRectangle topLeft negated preferSubPixelAntiAliasing: false! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 5/28/2021 10:06:00'!
onForm: aForm

	^ self subclassToUse basicNew
		initializeWith: aForm origin: `0@0` preferSubPixelAntiAliasing: false! !

!methodRemoval: BitBltCanvas #setForm: stamp: 'jmv 5/28/2021 14:13:27'!
BitBltCanvas removeSelector: #setForm:!
!methodRemoval: MorphicCanvas #setForm: stamp: 'jmv 5/28/2021 14:13:31'!
MorphicCanvas removeSelector: #setForm:!
!methodRemoval: MorphicCanvas #initializeWith:origin: stamp: 'jmv 5/28/2021 14:10:34'!
MorphicCanvas removeSelector: #initializeWith:origin:!
