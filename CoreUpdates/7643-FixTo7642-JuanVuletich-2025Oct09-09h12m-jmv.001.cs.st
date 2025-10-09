'From Cuis7.5 [latest update: #7642] on 9 October 2025 at 9:12:43 am'!

!BitBlt methodsFor: 'system primitives' stamp: 'jmv 10/9/2025 09:12:11'!
primitiveCopyBits: factor
	"This entry point to BitBlt supplies an extra argument to specify translucency
	for operations 30 and 31.  The argument must be an integer between 0 and 255."
	"Separated in it's own method so it can be protected with a class-wide MutEx.
	nil answer means primitive failure"
	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>
	^nil! !


!BitBlt methodsFor: 'copying' stamp: 'jmv 10/9/2025 09:12:15'!
copyBitsTranslucent: factor
	"This entry point to BitBlt supplies an extra argument to specify translucency
	for operations 30 and 31.  The argument must be an integer between 0 and 255."

	| primAnswer |
	self class accessProtect critical: [
		primAnswer := self primitiveCopyBits: factor ].
	primAnswer ifNotNil: [ ^primAnswer ].

	self primitiveFailed  "Later do nicer error recovery -- share copyBits recovery"! !

