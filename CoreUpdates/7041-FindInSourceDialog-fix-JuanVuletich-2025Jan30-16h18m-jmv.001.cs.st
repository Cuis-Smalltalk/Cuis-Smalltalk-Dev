'From Cuis7.3 [latest update: #7040] on 30 January 2025 at 4:20:30 pm'!

!SmalltalkEditor methodsFor: 'accessing' stamp: 'jmv 1/30/2025 16:19:30'!
selectedMethod
	^self codeProvider isEditingExistingMethod ifTrue: [
		self codeProvider currentCompiledMethod asMethodReference ].! !

