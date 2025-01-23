'From Cuis7.3 [latest update: #7002] on 23 January 2025 at 8:38:17 am'!

!Debugger methodsFor: 'method creation' stamp: 'jmv 1/23/2025 08:37:41'!
wasInterruptedOnOverridableMethod
	| methodClass receiverClass |

	self interruptedContext sender ifNotNil:
		[ :senderContext |
			senderContext selector == #shouldBeImplemented ifTrue: 
				[ ^false ]].

	methodClass := self interruptedContext method methodClass.
	receiverClass := self interruptedContext receiver class.

	^ methodClass ~= receiverClass! !

