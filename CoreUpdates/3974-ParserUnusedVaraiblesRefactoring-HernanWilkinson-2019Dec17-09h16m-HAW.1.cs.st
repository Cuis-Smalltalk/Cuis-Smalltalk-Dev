'From Cuis 5.0 [latest update: #3972] on 17 December 2019 at 9:21:05 am'!

!Parser methodsFor: 'expression types' stamp: 'HAW 12/17/2019 09:18:00'!
performInteractiveChecks: aMethodNode

	self
		declareUndeclaredTemps: aMethodNode;
		removeUnusedTempsOf: aMethodNode ! !

