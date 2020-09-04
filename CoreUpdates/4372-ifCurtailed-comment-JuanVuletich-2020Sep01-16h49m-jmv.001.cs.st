'From Cuis 5.0 [latest update: #4365] on 1 September 2020 at 4:50:43 pm'!

!BlockClosure methodsFor: 'exceptions' stamp: 'ar 12/4/2009 18:41'!
ifCurtailed: aBlock
	"Evaluate the receiver with an abnormal termination action.
	 Evaluate aBlock only if execution is unwound during execution
	 of the receiver.  If execution of the receiver finishes normally do
	 not evaluate aBlock.  N.B.  This method is *not* implemented as a
	 primitive.  Primitive 198 always fails.  The VM uses prim 198 in a
	 context's method as the mark for an ensure:/ifCurtailed: activation."
	"The abnormal termination is detected by the vm and signalled as an #aboutToReturn:to: message
	sent to a reified current context, with the context of the #ifCurtailed: invocation as an argument.
	The current context then walks the stack to unwind and execute any unwind blocks (including the
	one protected by the #ifCurtailed: invocation) - see Context>>#resume:through"
	| complete result |
	<primitive: 198>
	result := self valueNoContextSwitch.
	complete := true.
	^result! !

