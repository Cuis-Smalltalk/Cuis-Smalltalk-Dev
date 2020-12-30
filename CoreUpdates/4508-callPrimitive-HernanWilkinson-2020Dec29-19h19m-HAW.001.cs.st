'From Cuis 5.0 [latest update: #4506] on 29 December 2020 at 7:19:51 pm'!

!MethodContext methodsFor: 'instruction decoding (closures)' stamp: 'HAW 12/29/2020 19:19:31'!
callPrimitive: primNumber
	"Evaluate the primitive, either normal or inlined, and answer the new context resulting from that
	 (either the sender if a successful non-inlined primitive, or the current context, if not)."
	"Copied from Squeak, Context>>#callPrimitive:
	The message callInlinedPrimitive: is not implemented in Squeak also - Hernan"
	
	| maybePrimFailToken |
	primNumber >= (1 << 15) ifTrue: "Inlined primitive, cannot fail"
		[^self callInlinedPrimitive: primNumber].
	maybePrimFailToken := self doPrimitive: primNumber
								method: method
								receiver: receiver
								args: self arguments.
	"Normal primitive. Always at the beginning of methods."
	(self isPrimFailToken: maybePrimFailToken) ifFalse: "On success return the result"
		[^self methodReturnTop].
	"On failure, store the error code if appropriate and keep interpreting the method"
	(method encoderClass isStoreAt: pc in: method) ifTrue:
		[self at: stackp put: maybePrimFailToken last].
	^self! !

