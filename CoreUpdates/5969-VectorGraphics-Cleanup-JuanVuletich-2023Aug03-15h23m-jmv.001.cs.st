'From Cuis 6.0 [latest update: #5968] on 3 August 2023 at 3:25:15 pm'!

!VectorCanvas class methodsFor: 'class initialization' stamp: 'jmv 8/3/2023 15:24:05'!
initialize
	UISupervisor whenUIinSafeState: [
		(VectorEngineWithPlugin isPluginAvailable
			ifTrue: [
				"VectorEnginePlugin is still slow to make fills of rectangles (world background, window background)
				For instance, these methods
				#blendFillOnlyWPOTAt:antiAliasAlphaByte:
				#blendFillOnlyWPOTLeft:top:right:bottom:targetBits:morphIds:edgeCounts:alphaMask:
				could keep computed values from previous pixel, and reuse them most times.
				"
				MorphicCanvas activeSubclass: HybridCanvas
				"MorphicCanvas activeSubclass: VectorCanvas"
				]
			ifFalse: [
				'VectorEnginePlugin (VM plugin) is not available. HybridCanvas will be activated.' print.
				MorphicCanvas activeSubclass: HybridCanvas ]) ].! !


!VectorEngine class methodsFor: 'instance creation' stamp: 'jmv 8/3/2023 15:23:41'!
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
				Display width > 1920 ].
			engineClass := VectorEngineWithPlugin isPluginAvailable
				ifTrue: [ doSubPixelAntiAliasing
					ifTrue: [VectorEngineWithPluginSubPixel]
					ifFalse: [VectorEngineWithPluginWholePixel]]
				ifFalse: [ doSubPixelAntiAliasing
					ifTrue: [VectorEngineSubPixel]
					ifFalse: [VectorEngineWholePixel]]].
		^engineClass 	new initializeOnForm: aForm.! !

VectorCanvas initialize!
