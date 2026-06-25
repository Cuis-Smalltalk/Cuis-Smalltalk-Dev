'From Cuis7.9 [latest update: #8014] on 24 June 2026 at 5:13:43 pm'!

!Debugger methodsFor: 'method creation' stamp: 'HAW 24/Jun/2026, 8:11:05 pm (UTC)'!
createMethodOnSubclassResponsibility

	| chosenClass lastPossibleClass message methodCategory methodClass receiverClass subclassResponsibilityContext |
	
	subclassResponsibilityContext := self interruptedContext sender sender.
	methodCategory := subclassResponsibilityContext method category.
	methodClass := subclassResponsibilityContext method methodClass.
	receiverClass := subclassResponsibilityContext receiver class.
	lastPossibleClass := subclassResponsibilityContext method methodClass.
	message := subclassResponsibilityContext messageForYourself.

	chosenClass := self
		askForSuperclassOf: receiverClass
		upTo: lastPossibleClass
		toImplement: message selector
		ifCancel: [^self].
		
	^ self implement: message inCategory: methodCategory fromClass: chosenClass context: subclassResponsibilityContext! !

