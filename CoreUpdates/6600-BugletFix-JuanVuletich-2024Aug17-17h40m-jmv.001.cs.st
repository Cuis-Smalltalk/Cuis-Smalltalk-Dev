'From Cuis7.1 [latest update: #6598] on 17 August 2024 at 5:42:46 pm'!

!VectorEngine class methodsFor: 'instance creation' stamp: 'jmv 8/17/2024 17:42:28'!
onForm: aForm subPixelAntiAliasing: aBoolean
	"nil means use default kind of anti aliasing
	Use the VectorEnginePlugin if available."

	| engineClass doSubPixelAntiAliasing |
	self hasSubclasses
		ifFalse: [
			engineClass := self ]
		ifTrue: [
			doSubPixelAntiAliasing := aBoolean ifNil: [
				"Assume high resolution implies high DPI"
				Display width <= 1920 ].
			engineClass := VectorEngineWithPlugin isPluginAvailable
				ifTrue: [ doSubPixelAntiAliasing
					ifTrue: [VectorEngineWithPluginSubPixel]
					ifFalse: [VectorEngineWithPluginWholePixel]]
				ifFalse: [ doSubPixelAntiAliasing
					ifTrue: [VectorEngineSubPixel]
					ifFalse: [VectorEngineWholePixel]]].
		^engineClass 	new initializeOnForm: aForm.! !

