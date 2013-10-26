'From Cuis 4.0 of 21 April 2012 [latest update: #1495] on 4 December 2012 at 10:52:08 pm'!

!CompiledMethod methodsFor: 'literals' stamp: 'eem 11/5/2012 09:42'!
sendsSelector: aSelector 
	| scanner |
	scanner := InstructionStream on: self.
	scanner scanFor: 
		[:x | 
		 scanner selectorToSendOrSelf == aSelector ifTrue:
			[^true].
		 false	"keep scanning"].
	^false! !

