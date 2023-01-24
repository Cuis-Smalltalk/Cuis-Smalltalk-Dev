'From Cuis 6.0 [latest update: #5652] on 24 January 2023 at 3:41:12 pm'!

!ContextPart class methodsFor: 'special context creation' stamp: 'jar 1/24/2023 16:48:41'!
contextEnsure: block
	"Create an #ensure: context that is ready to return from executing its receiver.
	 Note: push a fake return value to the ctxt stack before making the jump as
	 #jump would remove it before returning (see #jump method and comments).
	See https://lists.cuis.st/mailman/archives/cuis-dev/2023-January/006841.html"

	| ctxt chain |
	ctxt _ thisContext.
	[chain _ thisContext sender cut: ctxt. ctxt push: nil; jump] ensure: block.
	"jump above will resume here without unwinding chain"
	^ chain! !

