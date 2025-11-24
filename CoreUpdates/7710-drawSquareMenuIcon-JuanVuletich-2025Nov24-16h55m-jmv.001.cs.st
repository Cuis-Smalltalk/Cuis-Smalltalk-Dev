'From Cuis7.5 [latest update: #7709] on 24 November 2025 at 5:01:58 pm'!

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 11/24/2025 16:55:03'!
drawSquareMenuIcon
	"For Window control buttons"

	self subclassResponsibility.! !


!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 11/24/2025 17:00:47'!
drawSquareMenuIcon

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.212 g: 0.439 b: 0.784`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.212 g: 0.439 b: 0.784) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/20.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowSquareButtonPathData: size) ].

	self fillColor: s do: [
		self pathSequence: (self class windowButtonMenuPathData: size) ].! !


!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 11/24/2025 16:55:28'!
drawSquareMenuIcon
	| icon |
	icon := self class windowButtonIcon: #squareMenuIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.! !


!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 11/24/2025 16:55:12'!
drawSquareMenuIcon
	vectorCanvas  drawSquareMenuIcon! !

