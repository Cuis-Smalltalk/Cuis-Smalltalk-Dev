'From Cuis7.5 [latest update: #7366] on 23 July 2025 at 2:24:18 pm'!

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 7/23/2025 14:23:44'!
pvtStrokeWidth: aNumber hop: aFloat

	strokeWidth := aNumber.
	hop := aFloat.
	"Set AntiAliasingWidth in the plugin, as #primStrokeWidth:hop: needs it."
	self
		primAntiAliasingWidth: antiAliasingWidth asFloat
		subPixelDelta: self subPixelDelta asFloat.
	"This call in the plugin uses AntiAliasingWith."
	self primStrokeWidth: strokeWidth hop: hop.! !

