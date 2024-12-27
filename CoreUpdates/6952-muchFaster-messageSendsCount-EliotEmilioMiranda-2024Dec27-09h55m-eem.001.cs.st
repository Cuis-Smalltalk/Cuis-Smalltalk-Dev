'From Cuis7.3 [latest update: #6951] on 27 December 2024 at 9:59:33 am'!

!CompiledMethod methodsFor: 'source code management' stamp: 'eem 12/27/2024 09:55:57'!
messageSendsCount
	"Number of message sends in code.
	A measure of the complexity of code. Alternative to #linesOfCode"

	| count scanner |
	count := 0.
	scanner := InstructionStream on: self.
	scanner scanFor:
		[:x | | selectorOrSelf |
		(selectorOrSelf := scanner selectorToSendOrSelf) == scanner ifFalse:
			[count := count + 1].
		false	"keep scanning"].
	^count! !

