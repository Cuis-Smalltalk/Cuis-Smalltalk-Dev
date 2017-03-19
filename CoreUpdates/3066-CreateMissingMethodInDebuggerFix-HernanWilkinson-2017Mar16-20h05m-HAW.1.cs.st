'From Cuis 5.0 of 7 November 2016 [latest update: #3064] on 17 March 2017 at 10:25:22 am'!

!Debugger methodsFor: 'method creation' stamp: 'HAW 3/17/2017 10:24:51'!
createMethodWhenDoesNotUndertand

	| message chosenClass interruptedContext |
	
	"The doesNotUndertand context must be selected - Hernan"
	contextStackIndex = 1 ifFalse: [ self contextStackIndex: 1 oldContextWas: self selectedContext ].
		
	interruptedContext _ self interruptedContext.
	message _ interruptedContext tempAt: 1.
	
	chosenClass _ self
		askForSuperclassOf: interruptedContext receiver class
		toImplement: message selector
		ifCancel: [^self].
	
	self implement: message inClass: chosenClass context: self selectedContext

! !

