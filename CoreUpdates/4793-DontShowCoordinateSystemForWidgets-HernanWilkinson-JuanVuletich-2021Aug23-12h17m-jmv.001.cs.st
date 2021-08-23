'From Cuis 5.0 [latest update: #4789] on 23 August 2021 at 12:19:38 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/23/2021 10:31:06'!
haloShowsCoordinateSystem

	^Preferences halosShowCoordinateSystem! !


!WidgetMorph methodsFor: 'halos and balloon help' stamp: 'jmv 8/23/2021 10:33:53'!
haloShowsCoordinateSystem
	"We are usually not concerned with this level of detail for Widgets, as they prefer using automatic Layout."

	^false! !


!HaloMorph methodsFor: 'drawing' stamp: 'jmv 8/23/2021 10:34:32'!
drawOn: aCanvas 

	| b |
	target ifNil: [^super drawOn: aCanvas].
	"Debugging aid"
	"aCanvas
		fillRectangle: haloBox
		color: (Color red alpha: 0.2).
	aCanvas
		fillRectangle: self morphLocalBounds
		color: (Color gray alpha: 0.4)."
	target haloShowsCoordinateSystem ifTrue: [
		target displayFullBounds ifNotNil: [ :fb |
			aCanvas frameGlobalRect: fb borderWidth: 3 color: `Color black alpha: 0.1`.
			b _ target displayBounds.
			b = fb ifFalse: [
				aCanvas frameGlobalRect: b borderWidth: 3 color: `Color black alpha: 0.1` ].
			self drawCoordinateSystemOn: aCanvas ]].! !

