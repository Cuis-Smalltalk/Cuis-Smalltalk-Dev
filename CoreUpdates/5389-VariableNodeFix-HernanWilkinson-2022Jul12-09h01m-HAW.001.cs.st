'From Cuis 6.0 [latest update: #5388] on 12 July 2022 at 9:03:58 am'!

!VariableNode methodsFor: 'testing' stamp: 'HAW 7/12/2022 09:01:26'!
isSelfPseudoVariable
	"Answer if this ParseNode represents the 'self' pseudo-variable."

	^ key = 'self' or: [ name = 'self'  or: [name = '{{self}}']]! !

