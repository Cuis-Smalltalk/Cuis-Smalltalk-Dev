'From Cuis7.1 [latest update: #6591] on 15 August 2024 at 5:53:53 pm'!

!Debugger methodsFor: 'method creation' stamp: 'jmv 8/15/2024 17:10:25'!
overrideMethodOnSubclass

	| chosenClass lastPossibleClass message methodCategory methodClass receiverClass |

	methodCategory := self interruptedContext method category.
	methodClass := self interruptedContext method methodClass.
	receiverClass := self interruptedContext receiver class.
	lastPossibleClass := (receiverClass withAllSuperclassesUpTo: methodClass) penultimate.
	message := self interruptedContext messageForYourself.

	chosenClass := self
		askForSuperclassOf: receiverClass
		upTo: lastPossibleClass
		toImplement: message selector
		withCaption: 'Override #', message selector, ' in which class?'
		ifCancel: [^self].
	
	^ self implement: message inCategory: methodCategory fromClass: chosenClass context: self interruptedContext! !

!methodRemoval: Behavior #withAllSuperclassesPreviousTo: stamp: 'jmv 8/15/2024 17:10:31'!
Behavior removeSelector: #withAllSuperclassesPreviousTo:!
!methodRemoval: Behavior #allSuperclassesPreviousTo: stamp: 'jmv 8/15/2024 17:07:12'!
Behavior removeSelector: #allSuperclassesPreviousTo:!
