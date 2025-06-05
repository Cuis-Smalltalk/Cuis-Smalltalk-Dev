'From Cuis7.5 [latest update: #7259] on 5 June 2025 at 11:06:45 am'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 6/5/2025 11:06:02'!
comeToFront

	"Restore default #layerNumber for us.
	We may still end behind other 'more topMost' morphs."
	self removeProperty: #layerNumber.
	self atFront ifFalse: [owner privateMoveFrontMorph: self]! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 6/5/2025 11:06:36'!
goBehind
	| regularLayerNumber |
	"Set our #layerNumber to the default for most morphs, allowing us
	to go behind them."
	regularLayerNumber  := 100.
	self layerNumber = regularLayerNumber ifFalse: [
		self setProperty: #layerNumber toValue: regularLayerNumber ].
	owner privateMoveBackMorph: self.
! !

