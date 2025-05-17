'From Cuis7.3 [latest update: #7158] on 15 May 2025 at 7:20:19 pm'!

!SimulationSideEffectWarning methodsFor: 'printing' stamp: 'HAW 5/15/2025 19:19:55'!
messageText

	^ messageText ifNil: [
		'The code being simulated is trying to control a process ({1}). {2}' translated format: {
			self context method asMethodReference.
			self isSimulationGuard
				ifTrue: ['If you proceed, your image may be locked. Continue at own risk, and better save your image before.' translated]
				ifFalse: ['Process controlling cannot be simulated. If you proceed, side effects may occur outside the observable area of the simulator.' translated]}]! !

