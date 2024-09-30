'From Cuis7.1 [latest update: #6752] on 30 September 2024 at 9:42:45 am'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 9/30/2024 09:40:39'!
beMainCanvasOfWorld
	world mainCanvas: self.
	self doOwnEngine.
	world displayWorldOn: self.! !

