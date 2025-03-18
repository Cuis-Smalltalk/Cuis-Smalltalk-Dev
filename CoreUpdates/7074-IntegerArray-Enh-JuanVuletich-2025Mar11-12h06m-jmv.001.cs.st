'From Cuis7.3 [latest update: #7067] on 11 March 2025 at 12:07:11 pm'!

!IntegerArray methodsFor: 'private' stamp: 'jmv 3/6/2025 19:07:06'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !


!IntegerArray methodsFor: 'private' stamp: 'jmv 3/11/2025 12:03:53'!
primFill: aPositiveInteger
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays.
	Note: We store signed integers (see class comment). Senders may hack negative integers into positive integers for this primitive to work. See senders. Use with care."

	<primitive: 145>
	self errorImproperStore.! !

