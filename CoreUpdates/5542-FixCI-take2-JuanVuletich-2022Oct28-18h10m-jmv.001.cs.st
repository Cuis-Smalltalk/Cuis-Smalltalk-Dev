'From Cuis 6.0 [latest update: #5541] on 28 October 2022 at 6:11:09 pm'!

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 10/28/2022 18:09:39'!
activeSubclass
	ActiveSubclass ifNil: [
		ActiveSubclass _ BitBltCanvas ].
	Display depth = 32 ifFalse: [
		"VectorCanvas can only work on 32 bit target"
		ActiveSubclass _ BitBltCanvas ].
	^ActiveSubclass! !


!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 1/17/2022 12:32:19'!
setForm: aForm subPixelAntiAliasing: aBoolean
	"nil means use default kind of anti aliasing"
	"
	self runningWorld setCanvas
	"
	aForm depth = 32 ifFalse: [ self error: 'VectorCanvas can only work on 32 bit target' ].
	form _ aForm.
	engine _ VectorEngine onForm: form subPixelAntiAliasing: aBoolean.
	engine canvas: self.
	auxBitBltEngine _ BitBltCanvasEngine toForm: form.
	self newClipRect: nil.! !

