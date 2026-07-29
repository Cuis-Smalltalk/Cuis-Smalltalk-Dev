'From Cuis7.9 [latest update: #8100] on 29 July 2026 at 2:54:08 pm'!

!Debugger methodsFor: 'method creation' stamp: 'FJG 12/Oct/2018, 3:22:13 pm (UTC)'!
createMethodOnDoesNotUnderstand

	| message chosenClass interruptedContext |
	
	"The doesNotUndertand context must be selected - Hernan"
	contextStackIndex = 1 ifFalse: [ self contextStackIndex: 1 oldContextWas: self selectedContext ].
		
	interruptedContext := self interruptedContext.
	message := interruptedContext tempAt: 1.
	
	chosenClass := self
		askForSuperclassOf: interruptedContext receiver class
		toImplement: message selector
		ifCancel: [^self].
	
	self implement: message inClass: chosenClass context: self selectedContext

! !


!Debugger methodsFor: 'method creation' stamp: 'FJG 12/Oct/2018, 3:22:26 pm (UTC)'!
createMethod

	self wasInterrupedOnDoesNotUnderstand ifTrue: [ ^self createMethodOnDoesNotUnderstand ].
	self wasInterruptedOnSubclassResponsibility ifTrue: [ ^self createMethodOnSubclassResponsibility ].
	self wasInterruptedOnOverridableMethod ifTrue: [ ^self overrideMethodOnSubclass ].
	
	self inform: 'Only available to override methods or for #doesNotUnderstand: and #subclassResponsibility' ! !

!methodRemoval: Debugger #createMethodWhenDoesNotUnderstand stamp: 'jmv 29/Jul/2026, 5:53:43 pm (UTC)'!
Debugger removeSelector: #createMethodWhenDoesNotUnderstand!
