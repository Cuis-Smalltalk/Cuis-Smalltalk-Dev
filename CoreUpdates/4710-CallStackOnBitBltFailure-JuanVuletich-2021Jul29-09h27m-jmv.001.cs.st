'From Cuis 5.0 [latest update: #4709] on 29 July 2021 at 9:28:28 am'!

!BitBlt methodsFor: 'private' stamp: 'jmv 7/29/2021 09:27:58'!
copyBitsAgain
	"Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>
	'#copyBitsAgain failed. Requested BitBlt operation not performed. Call stack follows:' print.
	thisContext printStack: 10.! !

