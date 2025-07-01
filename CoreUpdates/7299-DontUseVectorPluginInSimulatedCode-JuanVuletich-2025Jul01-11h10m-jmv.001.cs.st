'From Cuis7.5 [latest update: #7296] on 1 July 2025 at 11:14:01 am'!

!VectorEngine class methodsFor: 'instance creation' stamp: 'jmv 7/1/2025 11:13:53'!
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
			engineClass := (VectorEngineWithPlugin isPluginAvailable and: [
					"VectorEnginePlugin calls can not be reliably simulated in the Debugger because the plugin holds static state."
					Processor activeProcess isSimulatingAnotherProcess not ])
				ifTrue: [ doSubPixelAntiAliasing
					ifTrue: [VectorEngineWithPluginSubPixel]
					ifFalse: [VectorEngineWithPluginWholePixel]]
				ifFalse: [ doSubPixelAntiAliasing
					ifTrue: [VectorEngineSubPixel]
					ifFalse: [VectorEngineWholePixel]]].
		^engineClass new initializeOnForm: aForm.! !

