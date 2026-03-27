'From Cuis7.7 [latest update: #7849] on 26 March 2026 at 9:54:21 pm'!

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 26/Mar/2026 21:52:10'!
resetMorphBounds

	"Clear any possible contour."
	morphBoundsTop ifNotNil: [
		morphBoundsTop <= morphBoundsBottom ifTrue: [
			self
				primResetContourTop: (morphBoundsTop max: 0)
				bottom: (morphBoundsBottom min: targetHeight -1) ]].
	super resetMorphBounds.! !

