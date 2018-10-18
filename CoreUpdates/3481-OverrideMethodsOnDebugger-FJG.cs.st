'From Cuis 5.0 of 7 November 2016 [latest update: #3470] on 12 October 2018 at 12:23:20 pm'!

!Debugger methodsFor: 'method creation' stamp: 'FJG 10/8/2018 12:49:55'!
askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector withCaption: aCaptionText ifCancel: cancelBlock

	| classes chosenClassIndex |

	classes _ aClass = aSuperclass ifTrue: [ { aClass } ] ifFalse: [ aClass withAllSuperclassesUpTo: aSuperclass ].
	chosenClassIndex _ PopUpMenu
		withCaption: aCaptionText
		chooseFrom: (classes collect: [:c | c name]).
	chosenClassIndex = 0 ifTrue: [^ cancelBlock value].

	^ classes at: chosenClassIndex! !

!Debugger methodsFor: 'method creation' stamp: 'FJG 10/8/2018 12:38:20'!
implement: aMessage inCategory: aCategory fromClass: aClass context: aContext

	aClass
		compile: (aMessage createStubMethodFor: aClass)
		classified: aCategory.
		
	aContext privRefreshWith: (aClass lookupSelector: aMessage selector).
	aMessage arguments withIndexDo: [ :arg :index | aContext tempAt: index put: arg ].
	self resetContext: aContext! !


!Debugger methodsFor: 'method creation' stamp: 'FJG 10/8/2018 12:50:42'!
askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector ifCancel: cancelBlock

	^ self askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector withCaption: 'Define #', aSelector, ' in which class?' ifCancel: cancelBlock! !

!Debugger methodsFor: 'method creation' stamp: 'FJG 10/12/2018 12:22:13'!
createMethodWhenDoesNotUnderstand

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

!Debugger methodsFor: 'method creation' stamp: 'FJG 10/12/2018 12:22:26'!
createMethod

	self wasInterrupedOnDoesNotUnderstand ifTrue: [ ^self createMethodWhenDoesNotUnderstand ].
	self wasInterruptedOnSubclassResponsibility ifTrue: [ ^self createMethodOnSubclassResponsibility ].
	self wasInterruptedOnOverridableMethod ifTrue: [ ^self overrideMethodOnSubclass ].
	
	self inform: 'Only available to override methods or for #doesNotUnderstand: and #subclassResponsibility' ! !

!Debugger methodsFor: 'method creation' stamp: 'FJG 10/8/2018 12:37:52'!
implement: aMessage inClass: aClass context: aContext 
	
	self implement: aMessage inCategory: (self askForCategoryIn: aClass default: 'as yet unclassified') fromClass: aClass context: aContext! !

!Debugger methodsFor: 'method creation' stamp: 'FJG 10/8/2018 13:20:36'!
overrideMethodOnSubclass

	| chosenClass lastPossibleClass message methodCategory methodClass receiverClass |

	methodCategory _ self interruptedContext method category.
	methodClass _ self interruptedContext method methodClass.
	receiverClass _ self interruptedContext receiver class.
	lastPossibleClass _ (receiverClass withAllSuperclassesPreviousTo: methodClass) last.
	message _ self interruptedContext messageForYourself.

	chosenClass _ self
		askForSuperclassOf: receiverClass
		upTo: lastPossibleClass
		toImplement: message selector
		withCaption: 'Override #', message selector, ' in which class?'
		ifCancel: [^self].
	
	^ self implement: message inCategory: methodCategory fromClass: chosenClass context: self interruptedContext! !

!Debugger methodsFor: 'method creation' stamp: 'FJG 10/8/2018 13:20:43'!
wasInterruptedOnOverridableMethod
	| methodClass receiverClass |
	
	methodClass _ self interruptedContext method methodClass.
	receiverClass _ self interruptedContext receiver class.
	
	^ methodClass ~= receiverClass! !


!Behavior methodsFor: 'accessing class hierarchy' stamp: 'FJG 10/8/2018 13:21:29'!
allSuperclassesPreviousTo: aSuperclass
	
	| superclasses |
	
	superclasses _ self allSuperclassesUpTo: aSuperclass.
	
	^ superclasses allButLast! !

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'FJG 10/8/2018 13:21:39'!
withAllSuperclassesPreviousTo: aSuperclass

	| classes |
	
	classes _ self withAllSuperclassesUpTo: aSuperclass.
	
	^ classes allButLast! !

!methodRemoval: Debugger #createMethodWhenDoesNotUndertand!
Debugger removeSelector: #createMethodWhenDoesNotUndertand!
