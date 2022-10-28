'From Cuis 6.0 [latest update: #5540] on 28 October 2022 at 5:50:28 pm'!

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 10/28/2022 17:50:13'!
setForm: aForm subPixelAntiAliasing: aBoolean
	"nil means use default kind of anti aliasing"
	"
	self runningWorld setCanvas
	"
	"aForm depth = 32 ifFalse: [ self error: 'VectorCanvas can only work on 32 bit target' ]."
	"For the benefit of headless systems, like, for example, our CI on GitHub."
	form _ aForm depth = 32 ifTrue: [aForm] ifFalse: [Form extent: aForm extent depth: 32].
	form _ aForm.
	engine _ VectorEngine onForm: form subPixelAntiAliasing: aBoolean.
	engine canvas: self.
	auxBitBltEngine _ BitBltCanvasEngine toForm: form.
	self newClipRect: nil.! !

