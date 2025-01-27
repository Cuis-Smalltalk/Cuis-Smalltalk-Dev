'From Cuis7.3 [latest update: #7009] on 27 January 2025 at 9:50:44 am'!

!ContextPart methodsFor: 'controlling' stamp: 'eem 4/13/2017 13:02'!
quickSend: selector to: rcvr with: arguments lookupIn: lookupClass
	"Send the given selector with arguments in an environment which closely resembles
	 the non-simulating environment, with an interjected unwind-protected block to catch
	 nonlocal returns.  Attention: don't get lost!!  This beautiful method is originally due to
	 Hans-Martin Mosner."
	| oldSender contextToReturnTo result |
	contextToReturnTo := self.
	[oldSender := thisContext sender swapSender: self.
	 result := self object: rcvr perform: selector withArguments: arguments inClass: lookupClass.
	 thisContext sender swapSender: oldSender] ifCurtailed:
		[contextToReturnTo := thisContext sender receiver.	"The block context returning nonlocally"
		contextToReturnTo pc: contextToReturnTo previousPc.	"skip to front of return bytecode causing this unwind"
		contextToReturnTo willReturnTopFromMethod ifTrue:
			"If it was a returnTop, push the value to be returned.
			Otherwise the value is implicit in the bytecode"
			[contextToReturnTo push: (thisContext sender tempAt: 1)].
		thisContext swapSender: thisContext home sender.	"Make this block return to the method's sender"
		contextToReturnTo].
	contextToReturnTo push: result.
	^contextToReturnTo! !

