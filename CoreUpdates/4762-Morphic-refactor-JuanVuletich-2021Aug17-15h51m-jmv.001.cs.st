'From Cuis 5.0 [latest update: #4761] on 17 August 2021 at 3:53:26 pm'!

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/17/2021 14:44:25'!
onFormWithWholePixelAntiAliasing: aForm

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: false;
		initializeWithOrigin: `0@0`.! !


!BitBltCanvas methodsFor: 'private' stamp: 'jmv 8/17/2021 14:43:35'!
setForm: aForm subPixelAntiAliasing: aBooleanOrNil
	"nil means use default kind of anti aliasing"

	form _ aForm.
	engine _ BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	self newClipRect: nil.! !


!Morph methodsFor: 'geometry' stamp: 'jmv 8/17/2021 14:31:13'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Clipped appropiately if some owner clips us.
	See also #displayBoundsUpdateFrom:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			(aCanvas isBoundsFinderCanvas or: [c canvasToUse == aCanvas]) ifTrue: [
				self knowsOwnLocalBounds
					ifTrue: [
						"A good optimization in many cases.
						But for morphs where #drawOn: might not be able to find bounds (InnerTextMorph and LayoutMorph,
						for example) this is essential."
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphFromLocalBounds]
					ifFalse: [
						"Use the bounds bound by #drawOn. This method must therefore be called after calling #drawOn:."
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphAfterDraw.
						self wantsContour
							ifTrue: [
								 (aCanvas
									contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) 
									into: [ :contourArray :contourY0 :contourY1 |
										self setProperty: #contour toValue: contourArray.
										self setProperty: #contourY0 toValue: contourY0.
										self setProperty: #contourY1 toValue: contourY1.
								])]
							ifFalse: [
								self setProperty: #contour toValue: nil.
								self setProperty: #contourY0 toValue: nil.
								self setProperty: #contourY1 toValue: nil ]]]]].! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 8/17/2021 15:03:24'!
containsGlobalPoint: worldPoint
	"Answer true if pixel worldPoint is covered by us, and we are visible a it.
	No other morph above us also covers it."

	self visible ifTrue: [
		self topmostWorld ifNotNil: [ :w |
			w canvas ifNotNil: [ :canvas |
				^ canvas morph: self isAtPoint: worldPoint ]]].
	^ false! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/17/2021 14:43:50'!
on: aForm over: aRectangle
	"Warning: aForm extent must equal aRectangle extent"

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: nil;
		initializeWithOrigin: aRectangle topLeft negated.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/17/2021 14:43:57'!
onForm: aForm

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: nil;
		initializeWithOrigin: `0@0`.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 8/17/2021 14:44:02'!
onFormWithSubPixelAntiAliasing: aForm

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: true;
		initializeWithOrigin: `0@0`.! !

!methodRemoval: BitBltCanvas #setForm:preferSubPixelAntiAliasing: stamp: 'jmv 8/17/2021 15:52:00'!
BitBltCanvas removeSelector: #setForm:preferSubPixelAntiAliasing:!
