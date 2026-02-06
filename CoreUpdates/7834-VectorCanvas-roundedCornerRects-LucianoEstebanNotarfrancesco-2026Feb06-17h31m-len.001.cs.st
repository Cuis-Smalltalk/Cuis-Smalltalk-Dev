'From Cuis7.7 [latest update: #7833] on 6 February 2026 at 5:32:12 pm'!

!AbstractVectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'len 6/Feb/2026 17:54:34'!
fillRectangle: aRectangle color: aColor cornerRadius: r
	r > 0.0 ifFalse: [^ self fillRectangle: aRectangle color: aColor].
	self fillColor: aColor do: [
		self
			roundRectangleMinX: aRectangle left maxX: aRectangle right
			minY: aRectangle top maxY: aRectangle bottom rx: r ry: r ]! !

!AbstractVectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'len 6/Feb/2026 17:55:00'!
frameRectangle: aRectangle borderWidth: borderWidth color: borderColor cornerRadius: r
	| d |
	r > 0.0 ifFalse: [^ self frameRectangle: aRectangle borderWidth: borderWidth color: borderColor].
	d := borderWidth*0.5.
	self strokeWidth: borderWidth color: borderColor do: [
		self
			roundRectangleMinX: aRectangle left+d maxX: aRectangle right-d
			minY: aRectangle top+d maxY: aRectangle bottom-d rx: r ry: r]! !

