'From Cuis 6.0 [latest update: #5517] on 26 October 2022 at 10:14:26 am'!

!VectorCanvas class methodsFor: 'class initialization' stamp: 'jmv 10/25/2022 14:55:40'!
initialize
	VectorEngineWithPlugin isPluginAvailable ifFalse: [
		Feature require: 'VectorEngineInSmalltalk' ].
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

!methodRemoval: FontFamily class #enableTrueTypeFontsOnly stamp: 'jmv 10/26/2022 10:14:00'!
FontFamily class removeSelector: #enableTrueTypeFontsOnly!
VectorCanvas initialize!
