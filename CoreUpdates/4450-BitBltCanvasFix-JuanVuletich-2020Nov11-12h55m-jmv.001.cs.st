'From Cuis 5.0 [latest update: #4449] on 11 November 2020 at 1:03:24 pm'!

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 11/11/2020 12:58:43'!
setForm: aForm
	super setForm: aForm.
	engine _ BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	self newClipRect: nil.! !

!methodRemoval: BitBltCanvas #initializeWith:origin: stamp: 'jmv 11/11/2020 12:55:42'!
BitBltCanvas removeSelector: #initializeWith:origin:!
!methodRemoval: BitBltCanvas #resetEngine stamp: 'jmv 11/11/2020 12:59:36'!
BitBltCanvas removeSelector: #resetEngine!
