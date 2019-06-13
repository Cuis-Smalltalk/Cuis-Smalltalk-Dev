'From Cuis 5.0 of 7 November 2016 [latest update: #3801] on 12 June 2019 at 5:27:40 pm'!

!ParseNode methodsFor: 'testing' stamp: 'HAW 6/12/2019 17:23:21'!
isFalsePseudoVariable	
	"Overridden in VariableNode."
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 6/12/2019 17:23:26'!
isNilPseudoVariable	
	"Overridden in VariableNode."
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 6/12/2019 17:23:34'!
isThisContextPseudoVariable	
	"Overridden in VariableNode."
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 6/12/2019 17:23:42'!
isTruePseudoVariable	
	"Overridden in VariableNode."
	^false! !


!VariableNode methodsFor: 'testing' stamp: 'HAW 6/12/2019 17:25:37'!
isFalsePseudoVariable
	
	^key = 'false' or: [name = '{{false}}']! !

!VariableNode methodsFor: 'testing' stamp: 'HAW 6/12/2019 17:25:44'!
isNilPseudoVariable
	
	^ key = 'nil' or: [name = '{{nil}}']! !

!VariableNode methodsFor: 'testing' stamp: 'HAW 6/12/2019 17:26:06'!
isThisContextPseudoVariable
	
	^key = 'thisContext' or: [name = '{{thisContext}}']! !

!VariableNode methodsFor: 'testing' stamp: 'HAW 6/12/2019 17:25:56'!
isTruePseudoVariable
	
	^ key = 'true' or: [name = '{{true}}']! !

