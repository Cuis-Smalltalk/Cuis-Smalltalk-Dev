'From Cuis7.5 [latest update: #7703] on 20 November 2025 at 8:10:37 am'!

!FloatArray methodsFor: 'private' stamp: 'jmv 11/20/2025 08:09:59'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !

!methodRemoval: Float32Array #replaceFrom:to:with:startingAt: stamp: 'jmv 11/20/2025 08:09:59'!
Float32Array removeSelector: #replaceFrom:to:with:startingAt:!
