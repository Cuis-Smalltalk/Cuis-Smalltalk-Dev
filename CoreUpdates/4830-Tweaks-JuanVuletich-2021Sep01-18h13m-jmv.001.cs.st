'From Cuis 5.0 [latest update: #4829] on 1 September 2021 at 6:18:08 pm'!

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 9/1/2021 18:14:16'!
setForm: aForm subPixelAntiAliasing: aBooleanOrNil
	"nil means use default kind of anti aliasing. Ignored in BitBltCanvas."

	form _ aForm.
	engine _ BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	self newClipRect: nil.! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 9/1/2021 18:15:37'!
setPluginAndForm: aForm
	"No VectorEnginePlugin in BitBltCanvas."

	self setForm: aForm subPixelAntiAliasing: nil.! !

