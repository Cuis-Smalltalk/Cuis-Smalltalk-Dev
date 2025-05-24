'From Cuis7.3 [latest update: #7106] on 1 April 2025 at 9:52:08 am'!

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 4/1/2025 09:50:18'!
clipByCurrentMorphStop
	| prevClipSpec |
	csi := csi-1.
	prevClipSpec := csi = 1
		"Using nil instead of the initial clipping spec when no clipping is an optimization."
		ifFalse: [ clippingSpecStack at: csi ].
	engine clippingSpec: prevClipSpec.! !

