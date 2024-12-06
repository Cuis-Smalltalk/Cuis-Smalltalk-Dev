'From Cuis7.3 [latest update: #6895] on 6 December 2024 at 4:45:00 pm'!

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/6/2024 16:24:28'!
drawSquareCloseIcon
	"For Window control buttons"

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/6/2024 16:34:36'!
drawSquareExpandIcon
	"For Window control buttons"

	self subclassResponsibility.! !


!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 12/6/2024 16:33:50'!
drawSquareCloseIcon
"make all colors be parameters!!!!!!!!!!"

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.996 g: 0.329 b: 0.329`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.996 g: 0.329 b: 0.329) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/20.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowSquareButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonClosePathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 12/6/2024 16:38:27'!
drawSquareExpandIcon

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.337 g: 0.604 b: 0.161`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.337 g: 0.604 b: 0.161) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/20.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowSquareButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonExpandPathData: size) ].! !


!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 12/6/2024 16:33:29'!
windowSquareButtonPathData: aSize
	| scale |
	(IconPathData at: #windowButtonSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowSquareButton ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowSquareButton put: (
		self scalePathData: `#[
			0.0 0 0
			1    0 20
			1    20 20
			1    20 0
			1    0 0
		] asFloat32Array` scale: scale)! !


!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/6/2024 16:25:24'!
drawSquareCloseIcon
	| icon |
	icon := self class windowButtonIcon: #squareCloseIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/6/2024 16:34:48'!
drawSquareExpandIcon
	| icon |
	icon := self class windowButtonIcon: #squareExpandIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.! !


!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/6/2024 16:24:35'!
drawSquareCloseIcon
	vectorCanvas  drawSquareCloseIcon! !

!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/6/2024 16:34:54'!
drawSquareExpandIcon
	vectorCanvas  drawSquareExpandIcon! !

