'From Cuis7.9 [latest update: #8184] on 16 September 2026 at 10:24:02 am'!
!classDefinition: #MoveToInstanceOrClassMethod category: #'Tools-Refactoring'!
ScopedRefactoring subclass: #MoveToInstanceOrClassMethod
	instanceVariableNames: 'senders selector implementors changes sendsToRedirect '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!MoveToInstanceOrClassMethod commentStamp: '<historical>' prior: 0!
I move the implementors of a selector on one side of their classes to the other side, instance methods to the class side or class methods to the instance side, and redirect the senders I am given so that they keep reaching them.

The implementors and the senders come from a scope, the way they do for a selector change; the implementors on the other side of the one selected are left alone, and for: alone takes the whole system. My subclasses know the two directions and for: answers the one the method needs.

A sender is redirected by rewriting the receiver of each send I am told to redirect: an instance method sending it to self sends it to self class once it is a class method; one sending it to self class, or to the class by name, sends it to self once it is an instance method, or to a new instance of the class when it is not one itself; and a new instance sent it by name is sent it as a class. Which sends those are is answered by the closure I am created with: the ones whose receiver is written that way and reaches an implementor, or, with LiveTyping, the ones it saw reach one.!

!classDefinition: #MoveToClassMethod category: #'Tools-Refactoring'!
MoveToInstanceOrClassMethod subclass: #MoveToClassMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!MoveToClassMethod commentStamp: '<historical>' prior: 0!
I move instance methods to the class side: a sender reaching them through self, or any receiver LiveTyping saw hold an instance, sends the message to its class, and one through a new instance of the class by name sends it to the class.!

!classDefinition: #MoveToInstanceMethod category: #'Tools-Refactoring'!
MoveToInstanceOrClassMethod subclass: #MoveToInstanceMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!MoveToInstanceMethod commentStamp: '<historical>' prior: 0!
I move class methods to the instance side: a sender reaching them through self class sends the message to self, one through the class by name to self when it is an instance of that class and to a new instance otherwise, and any other receiver LiveTyping saw hold the class to a new instance.!

!classDefinition: #TransferMethod category: #'Tools-Refactoring'!
Refactoring subclass: #TransferMethod
	instanceVariableNames: 'method changes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #MoveToInstanceOrClassMethodApplier category: #'Tools-Refactoring'!
ChangeSelectorApplier subclass: #MoveToInstanceOrClassMethodApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!MoveToInstanceOrClassMethodApplier commentStamp: '<historical>' prior: 0!
I apply MoveToInstanceOrClassMethod from a browser the way a selector change is applied: I ask the scope, then show the implementors and the senders it takes in for the user to add to or remove from, and apply over what is left.!


!ParseNode methodsFor: 'testing' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
isReferencingBehavior

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 15/Sep/2026, 11:54:04 pm (UTC)'!
isSendOfClassToSelf

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
isSendOfNewToLiteralClass

	^false! !


!MessageNode methodsFor: 'testing' stamp: 'HAW 15/Sep/2026, 11:54:04 pm (UTC)'!
isSendOfClassToSelf

	^self receiverOrCascadeReceiver isSelfPseudoVariable and: [ self selectorSymbol == #class ]! !

!MessageNode methodsFor: 'testing' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
isSendOfNewToLiteralClass

	^self receiverOrCascadeReceiver isReferencingBehavior
		and: [ #(new basicNew) includes: self selectorSymbol ]! !


!ScopedRefactoring class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 11:20:52 am (UTC)'!
assertAllImplementors: implementors haveSame: aSelector

	| invalidImplementors |
	
	invalidImplementors := implementors reject: [ :anImplementor | anImplementor selector = aSelector ].
	invalidImplementors notEmpty ifTrue: [ self signalInvalidImplementors: invalidImplementors ].! !

!ScopedRefactoring class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 11:20:54 am (UTC)'!
assertAllSenders: senders send: aSelector

	| invalidSenders |
	
	invalidSenders := senders reject: [ :aSender | aSender hasLiteral: aSelector ].
	invalidSenders notEmpty ifTrue: [ self signalInvalidSenders: invalidSenders of: aSelector ].! !

!ScopedRefactoring class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 11:23:08 am (UTC)'!
assertIsNotEmpty: aCollection signalMessageText: errorMessageText

	aCollection isEmpty ifTrue: [ self refactoringError: errorMessageText ].! !

!ScopedRefactoring class methodsFor: 'exceptions' stamp: 'HAW 16/Sep/2026, 11:20:52 am (UTC)'!
errorMessageForInvalidImplementors: aCollectionOfInvalidImplementors

	^String streamContents: [ :stream |
		stream 
			nextPutAll: (aCollectionOfInvalidImplementors size = 1 ifTrue: [ 'This method' ] ifFalse: [ 'These methods']);
		 	nextPutAll: ' do not have same the same selector to rename: '.
		aCollectionOfInvalidImplementors asCommaSeparated: [:anImplementor | anImplementor printClassAndSelectorOn: stream ] on: stream ] ! !

!ScopedRefactoring class methodsFor: 'exceptions' stamp: 'HAW 16/Sep/2026, 11:20:54 am (UTC)'!
errorMessageForInvalidSenders: aCollectionOfInvalidSenders of: anOldSelector

	^String streamContents: [ :stream |
		stream 
			nextPutAll: 'Message #';
			nextPutAll: anOldSelector;
			nextPutAll: ' is not send by: '.
		aCollectionOfInvalidSenders asCommaSeparated: [:aSender | aSender printClassAndSelectorOn: stream ] on: stream ] ! !

!ScopedRefactoring class methodsFor: 'exceptions' stamp: 'HAW 16/Sep/2026, 11:20:52 am (UTC)'!
signalInvalidImplementors: invalidImplementors

	self refactoringError: (self errorMessageForInvalidImplementors: invalidImplementors).! !

!ScopedRefactoring class methodsFor: 'exceptions' stamp: 'HAW 16/Sep/2026, 11:20:54 am (UTC)'!
signalInvalidSenders: invalidSender of: aSelector

	self refactoringError: (self errorMessageForInvalidSenders: invalidSender of: aSelector).! !


!MoveToInstanceOrClassMethod methodsFor: 'applying - private' stamp: 'HAW 16/Sep/2026, 11:28:38 am (UTC)'!
move: anImplementor

	| targetClass |

	targetClass := self class targetClassOf: anImplementor.
	self
		noteChangeIn: targetClass
		at: (targetClass compile: anImplementor sourceCode classified: anImplementor category).
	anImplementor methodClass removeSelector: anImplementor selector.
	self noteChangeIn: anImplementor methodClass at: anImplementor selector! !

!MoveToInstanceOrClassMethod methodsFor: 'applying - private' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
moveImplementors

	implementors do: [ :anImplementor | self move: anImplementor ]! !

!MoveToInstanceOrClassMethod methodsFor: 'applying - private' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
noteChangeIn: aClass at: aSelector

	changes add: (MethodReference class: aClass selector: aSelector)! !

!MoveToInstanceOrClassMethod methodsFor: 'initialization' stamp: 'HAW 16/Sep/2026, 12:51:21 pm (UTC)'!
initializeMoving: someImplementors of: aSelector redirecting: someSenders with: aSendsToRedirectBlock

	selector := aSelector.
	implementors := someImplementors asArray.
	senders := someSenders.
	sendsToRedirect := aSendsToRedirectBlock.
	changes := Set new! !

!MoveToInstanceOrClassMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
behaviorReceiving: aReceiverNode in: aSender ifNone: aNoneBlock

	^self subclassResponsibility! !

!MoveToInstanceOrClassMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:28:38 am (UTC)'!
isRedirectableSend: aNode in: aSender

	| receivingBehavior |

	((aNode isMessageNamed: selector) and: [ aNode isCascade not ]) ifFalse: [ ^false ].
	receivingBehavior := self behaviorReceiving: aNode receiver in: aSender ifNone: [ ^false ].

	^self reaches: receivingBehavior! !

!MoveToInstanceOrClassMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
reaches: aBehavior

	^implementors identityIncludes: (aBehavior lookupSelector: selector)! !

!MoveToInstanceOrClassMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
redirect: aReceiverNode written: aText in: aSender

	^self subclassResponsibility! !

!MoveToInstanceOrClassMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
redirectSenders

	(senders reject: [ :aSender | implementors identityIncludes: aSender ])
		do: [ :aSender | self redirectSendsIn: aSender ]! !

!MoveToInstanceOrClassMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
redirectSendsIn: aSender

	| redirections |

	redirections := self redirectionsIn: aSender.
	redirections ifEmpty: [ ^self ].

	aSender methodClass compile: (aSender sourceCode copyReplacing: redirections).
	self noteChangeIn: aSender methodClass at: aSender selector! !

!MoveToInstanceOrClassMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:28:38 am (UTC)'!
redirectionOf: aSend in: aMethodNode of: aSender

	| receiverRange receiverText |

	receiverRange := (aSend receiverNodeSourceRangesIn: aMethodNode) value.
	receiverText := aMethodNode sourceText
		copyFrom: receiverRange first
		to: receiverRange last.

	^receiverRange -> (self redirect: aSend receiver written: receiverText in: aSender)! !

!MoveToInstanceOrClassMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 12:50:09 pm (UTC)'!
redirectionsIn: aSender

	"The sends to redirect are answered by a closure because LiveTyping has to override which
	they are: the ones it saw reach the implementors, whatever their receiver is written as,
	instead of the ones whose receiver names the current side."

	| methodNode redirections |

	methodNode := aSender notOptimizedMethodNode.
	redirections := SortedCollection sortBlock: [ :aRedirection :anotherRedirection |
		aRedirection key first <= anotherRedirection key first ].
	(sendsToRedirect value: methodNode value: aSender value: self) do: [ :aSend |
		redirections add: (self
			redirectionOf: aSend
			in: methodNode
			of: aSender) ].

	^redirections! !

!MoveToInstanceOrClassMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 12:50:09 pm (UTC)'!
sendsByReceiverIn: aMethodNode of: aSender

	^aMethodNode completeSourceRanges keys
		select: [ :aNode | self isRedirectableSend: aNode in: aSender ]! !

!MoveToInstanceOrClassMethod methodsFor: 'accessing' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
implementors

	^implementors! !

!MoveToInstanceOrClassMethod methodsFor: 'accessing' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
selector

	^selector! !

!MoveToInstanceOrClassMethod methodsFor: 'applying' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
apply

	self
		redirectSenders;
		moveImplementors.

	^changes! !


!MoveToInstanceOrClassMethod class methodsFor: 'instance creation' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
classMovingFrom: aBehavior

	^aBehavior isMeta
		ifTrue: [ MoveToInstanceMethod ]
		ifFalse: [ MoveToClassMethod ]! !

!MoveToInstanceOrClassMethod class methodsFor: 'instance creation' stamp: 'HAW 16/Sep/2026, 12:50:09 pm (UTC)'!
for: aMethodToMove implementors: someImplementors senders: someSenders

	^self
		for: aMethodToMove
		implementors: someImplementors
		senders: someSenders
		redirecting: [ :aMethodNode :aSender :aMove | aMove sendsByReceiverIn: aMethodNode of: aSender ]! !

!MoveToInstanceOrClassMethod class methodsFor: 'instance creation' stamp: 'HAW 16/Sep/2026, 12:51:21 pm (UTC)'!
for: aMethodToMove implementors: someImplementors senders: someSenders redirecting: aSendsToRedirectBlock

	| refactoringClass implementorsToMove |

	refactoringClass := self classMovingFrom: aMethodToMove methodClass.
	implementorsToMove := refactoringClass implementorsOnCurrentSideAmong: someImplementors.
	refactoringClass assertCanMoveAll: implementorsToMove of: aMethodToMove selector.
	self assertAllSenders: someSenders send: aMethodToMove selector.

	^refactoringClass new
		initializeMoving: implementorsToMove
		of: aMethodToMove selector
		redirecting: someSenders
		with: aSendsToRedirectBlock! !

!MoveToInstanceOrClassMethod class methodsFor: 'instance creation' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
for: aMethodToMove inSystem: aSystem

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.
	(self classMovingFrom: aMethodToMove methodClass)
		addImplementorsOf: aMethodToMove selector
		to: implementors
		andSendersTo: senders
		inSystem: aSystem.

	^self
		for: aMethodToMove
		implementors: implementors
		senders: (senders collect: [ :aSender | aSender compiledMethod ])! !

!MoveToInstanceOrClassMethod class methodsFor: 'exceptions' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
noImplementorToMoveErrorMessage

	^'There is no implementor to move on the side of the method'! !

!MoveToInstanceOrClassMethod class methodsFor: 'exceptions' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
signalTargetAlreadyImplements: anImplementor

	self refactoringError: (self targetAlreadyImplementsErrorMessageFor: anImplementor)! !

!MoveToInstanceOrClassMethod class methodsFor: 'exceptions' stamp: 'HAW 16/Sep/2026, 11:26:56 am (UTC)'!
targetAlreadyImplementsErrorMessageFor: anImplementor

	^'Can not move ' , anImplementor classAndSelector , ' because its other side already implements it'! !

!MoveToInstanceOrClassMethod class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 11:28:38 am (UTC)'!
assertCanMove: anImplementor

	self
		assertIsNotAccessingInstanceVariable: anImplementor;
		assertLocalVariableDoesNotConflictWithInstanceVariable: anImplementor;
		assertTargetDoesNotImplement: anImplementor! !

!MoveToInstanceOrClassMethod class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 11:28:38 am (UTC)'!
assertCanMoveAll: someImplementors of: aSelector

	self
		assertIsNotEmpty: someImplementors
		signalMessageText: self noImplementorToMoveErrorMessage.
	self assertAllImplementors: someImplementors haveSame: aSelector.
	someImplementors do: [ :anImplementor | self assertCanMove: anImplementor ]! !

!MoveToInstanceOrClassMethod class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
assertTargetDoesNotImplement: anImplementor

	((self targetClassOf: anImplementor) includesSelector: anImplementor selector)
		ifTrue: [ self signalTargetAlreadyImplements: anImplementor ]! !

!MoveToInstanceOrClassMethod class methodsFor: 'sides' stamp: 'HAW 16/Sep/2026, 12:47:48 pm (UTC)'!
isCurrentSide: aBehavior

	^self subclassResponsibility! !

!MoveToInstanceOrClassMethod class methodsFor: 'sides' stamp: 'HAW 15/Sep/2026, 11:54:04 pm (UTC)'!
targetClassOf: aMethodToMove

	^self subclassResponsibility! !

!MoveToInstanceOrClassMethod class methodsFor: 'implementors and senders' stamp: 'HAW 16/Sep/2026, 12:47:48 pm (UTC)'!
addImplementorsOf: aSelector to: implementors andSendersTo: senders of: aBehavior

	| implementorsToKeep |

	implementorsToKeep := (self isCurrentSide: aBehavior)
		ifTrue: [ implementors ]
		ifFalse: [ OrderedCollection new ].

	super
		addImplementorsOf: aSelector
		to: implementorsToKeep
		andSendersTo: senders
		of: aBehavior! !

!MoveToInstanceOrClassMethod class methodsFor: 'implementors and senders' stamp: 'HAW 16/Sep/2026, 12:47:50 pm (UTC)'!
implementorsOnCurrentSideAmong: someImplementors

	^someImplementors select: [ :anImplementor |
		self isCurrentSide: anImplementor methodClass ]! !


!MoveToClassMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:28:38 am (UTC)'!
behaviorReceiving: aReceiverNode in: aSender ifNone: aNoneBlock

	aReceiverNode isSelfPseudoVariable ifTrue: [ ^aSender methodClass ].
	aReceiverNode isSendOfNewToLiteralClass
		ifTrue: [ ^aReceiverNode receiver variableBinding value ].

	^aNoneBlock value! !

!MoveToClassMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:28:38 am (UTC)'!
redirect: aReceiverNode written: aText in: aSender

	aReceiverNode isSendOfNewToLiteralClass ifTrue: [ ^aReceiverNode receiver name ].

	^aText , ' class'! !


!MoveToClassMethod class methodsFor: 'sides' stamp: 'HAW 16/Sep/2026, 12:47:48 pm (UTC)'!
isCurrentSide: aBehavior

	^aBehavior isMeta not! !

!MoveToClassMethod class methodsFor: 'sides' stamp: 'HAW 15/Sep/2026, 11:54:04 pm (UTC)'!
targetClassOf: aMethodToMove

	^aMethodToMove methodClass class! !


!MoveToInstanceMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:28:38 am (UTC)'!
behaviorReceiving: aReceiverNode in: aSender ifNone: aNoneBlock

	aReceiverNode isSendOfClassToSelf ifTrue: [ ^aSender methodClass class ].
	aReceiverNode isReferencingBehavior
		ifTrue: [ ^aReceiverNode variableBinding value class ].

	^aNoneBlock value! !

!MoveToInstanceMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:28:38 am (UTC)'!
redirect: aReceiverNode written: aText in: aSender

	aReceiverNode isSendOfClassToSelf ifTrue: [ ^'self' ].
	aReceiverNode isReferencingBehavior
		ifTrue: [ ^self redirectLiteralClass: aReceiverNode written: aText in: aSender ].

	^aText , ' new'! !

!MoveToInstanceMethod methodsFor: 'redirecting senders - private' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
redirectLiteralClass: aReceiverNode written: aText in: aSender

	^(aSender methodClass includesBehavior: aReceiverNode variableBinding value)
		ifTrue: [ 'self' ]
		ifFalse: [ aText , ' new' ]! !


!MoveToInstanceMethod class methodsFor: 'sides' stamp: 'HAW 16/Sep/2026, 12:47:48 pm (UTC)'!
isCurrentSide: aBehavior

	^aBehavior isMeta! !

!MoveToInstanceMethod class methodsFor: 'sides' stamp: 'HAW 15/Sep/2026, 11:54:04 pm (UTC)'!
targetClassOf: aMethodToMove

	^aMethodToMove methodClass soleInstance! !


!RefactoringApplier class methodsFor: 'appliers - registering' stamp: 'HAW 16/Sep/2026, 12:02:22 am (UTC)'!
registerMoveToInstanceOrClassMethodApplier: aMoveToInstanceOrClassMethodApplierClass

	self
		registerApplierAt: self moveToInstanceOrClassMethodApplierId
		with: aMoveToInstanceOrClassMethodApplierClass! !

!RefactoringApplier class methodsFor: 'appliers - getting' stamp: 'HAW 16/Sep/2026, 12:02:22 am (UTC)'!
moveToInstanceOrClassMethodApplier

	^self
		applierAt: self moveToInstanceOrClassMethodApplierId
		ifAbsent: [ MoveToInstanceOrClassMethodApplier ]! !

!RefactoringApplier class methodsFor: 'appliers - id' stamp: 'HAW 16/Sep/2026, 12:02:22 am (UTC)'!
moveToInstanceOrClassMethodApplierId

	^#moveToInstanceOrClassMethodApplier! !

!RefactoringApplier class methodsFor: 'appliers - resetting' stamp: 'HAW 16/Sep/2026, 12:02:22 am (UTC)'!
resetMoveToInstanceOrClassMethodApplier

	self resetApplierAt: self moveToInstanceOrClassMethodApplierId! !


!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 16/Sep/2026, 11:28:38 am (UTC)'!
createRefactoring

	^self refactoringClass
		for: self methodToMove
		implementors: implementors
		senders: senders! !

!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
methodToMove

	^selectedClass >> oldSelector! !

!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - changes' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
informChangesToBrowser

	browser selector: oldSelector refactoredTo: nil! !


!MoveToInstanceOrClassMethodApplier class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
assertCanApplyRefactoringFor: aSelector in: aSelectedClass
! !


!Browser methodsFor: 'refactorings' stamp: 'HAW 16/Sep/2026, 12:03:24 am (UTC)'!
moveToInstanceOrClassMethod

	RefactoringApplier moveToInstanceOrClassMethodApplier applyFrom: self! !


!MethodSet methodsFor: 'refactorings' stamp: 'HAW 16/Sep/2026, 12:03:24 am (UTC)'!
moveToInstanceOrClassMethod

	RefactoringApplier moveToInstanceOrClassMethodApplier applyFrom: self! !


!MoveToInstanceOrClassMethod class methodsFor: 'instance creation' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
for: aMethodToMove

	^self for: aMethodToMove inSystem: Smalltalk! !

!MoveToInstanceOrClassMethod class methodsFor: 'pre-conditions' stamp: 'HAW 15/Sep/2026, 11:54:04 pm (UTC)'!
assertLocalVariableDoesNotConflictWithInstanceVariable: aMethodToMove

	(self targetClassOf: aMethodToMove) instVarNames do: [ :instVarName |
		(aMethodToMove notOptimizedMethodNode hasLocalNamed: instVarName)
			ifTrue: [ self signalLocalVariableConflictsWithInstanceVariable ] ]! !


!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
refactoringClass

	^MoveToInstanceOrClassMethod classMovingFrom: selectedClass! !

!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 16/Sep/2026, 11:25:12 am (UTC)'!
requestRefactoringParameters
! !

!methodRemoval: MoveToInstanceOrClassMethodApplier class #applyFrom: stamp: 'HAW 16/Sep/2026, 11:25:32 am (UTC)'!
MoveToInstanceOrClassMethodApplier class removeSelector: #applyFrom:!
!methodRemoval: MoveToInstanceOrClassMethodApplier class #on:for: stamp: 'HAW 16/Sep/2026, 11:25:32 am (UTC)'!
MoveToInstanceOrClassMethodApplier class removeSelector: #on:for:!
!methodRemoval: MoveToInstanceOrClassMethodApplier #collectionToAddimplementorsTo stamp: 'HAW 16/Sep/2026, 11:25:29 am (UTC)'!
MoveToInstanceOrClassMethodApplier removeSelector: #collectionToAddimplementorsTo!
!methodRemoval: MoveToInstanceOrClassMethodApplier #selectorToLookImplementorsFor stamp: 'HAW 16/Sep/2026, 11:25:29 am (UTC)'!
MoveToInstanceOrClassMethodApplier removeSelector: #selectorToLookImplementorsFor!
!methodRemoval: MoveToInstanceOrClassMethodApplier #collectionToAddSendersTo stamp: 'HAW 16/Sep/2026, 11:25:29 am (UTC)'!
MoveToInstanceOrClassMethodApplier removeSelector: #collectionToAddSendersTo!
!methodRemoval: MoveToInstanceOrClassMethodApplier #initializeOn:for: stamp: 'HAW 16/Sep/2026, 11:25:29 am (UTC)'!
MoveToInstanceOrClassMethodApplier removeSelector: #initializeOn:for:!
!methodRemoval: MoveToInstanceOrClassMethodApplier #sendersToRedirect stamp: 'HAW 16/Sep/2026, 11:25:29 am (UTC)'!
MoveToInstanceOrClassMethodApplier removeSelector: #sendersToRedirect!
!methodRemoval: MoveToInstanceOrClassMethodApplier #showChanges stamp: 'HAW 16/Sep/2026, 11:25:29 am (UTC)'!
MoveToInstanceOrClassMethodApplier removeSelector: #showChanges!
!methodRemoval: MoveToInstanceOrClassMethodApplier #methodHasSenders stamp: 'HAW 16/Sep/2026, 11:25:29 am (UTC)'!
MoveToInstanceOrClassMethodApplier removeSelector: #methodHasSenders!
!methodRemoval: MoveToInstanceMethod class #sideLeftOf: stamp: 'HAW 16/Sep/2026, 11:25:27 am (UTC)'!
MoveToInstanceMethod class removeSelector: #sideLeftOf:!
!methodRemoval: MoveToInstanceMethod class #isSideLeft: stamp: 'HAW 16/Sep/2026, 12:47:48 pm (UTC)'!
MoveToInstanceMethod class removeSelector: #isSideLeft:!
!methodRemoval: MoveToInstanceMethod #redirectedReceiver stamp: 'HAW 16/Sep/2026, 11:25:22 am (UTC)'!
MoveToInstanceMethod removeSelector: #redirectedReceiver!
!methodRemoval: MoveToInstanceMethod #isRedirectableReceiver: stamp: 'HAW 16/Sep/2026, 11:25:22 am (UTC)'!
MoveToInstanceMethod removeSelector: #isRedirectableReceiver:!
!methodRemoval: MoveToClassMethod class #sideLeftOf: stamp: 'HAW 16/Sep/2026, 11:25:24 am (UTC)'!
MoveToClassMethod class removeSelector: #sideLeftOf:!
!methodRemoval: MoveToClassMethod class #isSideLeft: stamp: 'HAW 16/Sep/2026, 12:47:48 pm (UTC)'!
MoveToClassMethod class removeSelector: #isSideLeft:!
!methodRemoval: MoveToClassMethod #redirectedReceiver stamp: 'HAW 16/Sep/2026, 11:25:20 am (UTC)'!
MoveToClassMethod removeSelector: #redirectedReceiver!
!methodRemoval: MoveToClassMethod #isRedirectableReceiver: stamp: 'HAW 16/Sep/2026, 11:25:19 am (UTC)'!
MoveToClassMethod removeSelector: #isRedirectableReceiver:!
!methodRemoval: MoveToInstanceOrClassMethod class #isSideLeft: stamp: 'HAW 16/Sep/2026, 12:47:48 pm (UTC)'!
MoveToInstanceOrClassMethod class removeSelector: #isSideLeft:!
!methodRemoval: MoveToInstanceOrClassMethod class #sideLeftOf: stamp: 'HAW 16/Sep/2026, 11:25:17 am (UTC)'!
MoveToInstanceOrClassMethod class removeSelector: #sideLeftOf:!
!methodRemoval: MoveToInstanceOrClassMethod class #for:senders: stamp: 'HAW 16/Sep/2026, 11:25:17 am (UTC)'!
MoveToInstanceOrClassMethod class removeSelector: #for:senders:!
!methodRemoval: MoveToInstanceOrClassMethod class #implementorsOnSideLeftAmong: stamp: 'HAW 16/Sep/2026, 12:47:50 pm (UTC)'!
MoveToInstanceOrClassMethod class removeSelector: #implementorsOnSideLeftAmong:!
!methodRemoval: MoveToInstanceOrClassMethod class #classMoving: stamp: 'HAW 16/Sep/2026, 11:25:17 am (UTC)'!
MoveToInstanceOrClassMethod class removeSelector: #classMoving:!
!methodRemoval: MoveToInstanceOrClassMethod #initializeFor:senders: stamp: 'HAW 16/Sep/2026, 11:25:15 am (UTC)'!
MoveToInstanceOrClassMethod removeSelector: #initializeFor:senders:!
!methodRemoval: MoveToInstanceOrClassMethod #redirectedReceiver stamp: 'HAW 16/Sep/2026, 11:25:15 am (UTC)'!
MoveToInstanceOrClassMethod removeSelector: #redirectedReceiver!
!methodRemoval: MoveToInstanceOrClassMethod #parenthesized:for: stamp: 'HAW 16/Sep/2026, 11:28:41 am (UTC)'!
MoveToInstanceOrClassMethod removeSelector: #parenthesized:for:!
!methodRemoval: MoveToInstanceOrClassMethod #categoryOf: stamp: 'HAW 16/Sep/2026, 11:28:41 am (UTC)'!
MoveToInstanceOrClassMethod removeSelector: #categoryOf:!
!methodRemoval: MoveToInstanceOrClassMethod #isRedirectableReceiver: stamp: 'HAW 16/Sep/2026, 11:25:15 am (UTC)'!
MoveToInstanceOrClassMethod removeSelector: #isRedirectableReceiver:!
!methodRemoval: MoveToInstanceOrClassMethod #compileInTargetClass stamp: 'HAW 16/Sep/2026, 11:25:15 am (UTC)'!
MoveToInstanceOrClassMethod removeSelector: #compileInTargetClass!
!methodRemoval: MoveToInstanceOrClassMethod #reachesMethodFrom: stamp: 'HAW 16/Sep/2026, 11:25:15 am (UTC)'!
MoveToInstanceOrClassMethod removeSelector: #reachesMethodFrom:!
!methodRemoval: MoveToInstanceOrClassMethod #sortedByPosition: stamp: 'HAW 16/Sep/2026, 12:47:20 am (UTC)'!
MoveToInstanceOrClassMethod removeSelector: #sortedByPosition:!
!methodRemoval: MoveToInstanceOrClassMethod #redirectionOf:in: stamp: 'HAW 16/Sep/2026, 11:25:15 am (UTC)'!
MoveToInstanceOrClassMethod removeSelector: #redirectionOf:in:!
!methodRemoval: MoveToInstanceOrClassMethod #isRedirectableSend: stamp: 'HAW 16/Sep/2026, 11:25:15 am (UTC)'!
MoveToInstanceOrClassMethod removeSelector: #isRedirectableSend:!
!methodRemoval: MoveToInstanceOrClassMethod #moveMethod stamp: 'HAW 16/Sep/2026, 11:25:15 am (UTC)'!
MoveToInstanceOrClassMethod removeSelector: #moveMethod!
!methodRemoval: ChangeSelector class #errorMessageForInvalidSenders:of: stamp: 'HAW 16/Sep/2026, 11:20:54 am (UTC)'!
ChangeSelector class removeSelector: #errorMessageForInvalidSenders:of:!
!methodRemoval: ChangeSelector class #signalInvalidSenders:of: stamp: 'HAW 16/Sep/2026, 11:20:54 am (UTC)'!
ChangeSelector class removeSelector: #signalInvalidSenders:of:!
!methodRemoval: ChangeSelector class #assertAllImplementors:haveSame: stamp: 'HAW 16/Sep/2026, 11:20:52 am (UTC)'!
ChangeSelector class removeSelector: #assertAllImplementors:haveSame:!
!methodRemoval: ChangeSelector class #signalInvalidImplementors: stamp: 'HAW 16/Sep/2026, 11:20:52 am (UTC)'!
ChangeSelector class removeSelector: #signalInvalidImplementors:!
!methodRemoval: ChangeSelector class #assertAllSenders:send: stamp: 'HAW 16/Sep/2026, 11:20:54 am (UTC)'!
ChangeSelector class removeSelector: #assertAllSenders:send:!
!methodRemoval: ChangeSelector class #errorMessageForInvalidImplementors: stamp: 'HAW 16/Sep/2026, 11:20:52 am (UTC)'!
ChangeSelector class removeSelector: #errorMessageForInvalidImplementors:!
!methodRemoval: ChangeSelector class #assertIsNotEmpty:signalMessageText: stamp: 'HAW 16/Sep/2026, 11:23:08 am (UTC)'!
ChangeSelector class removeSelector: #assertIsNotEmpty:signalMessageText:!
!classDefinition: #MoveToInstanceOrClassMethod category: #'Tools-Refactoring'!
ScopedRefactoring subclass: #MoveToInstanceOrClassMethod
	instanceVariableNames: 'selector implementors senders sendsToRedirect changes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!MoveToInstanceOrClassMethod class reorganize!
('instance creation' classMovingFrom: for: for:implementors:senders: for:implementors:senders:redirecting: for:inSystem:)
('exceptions' localVariableConflictsWithInstanceVariableErrorMessage noImplementorToMoveErrorMessage referencingInstanceVariablesErrorMessage signalLocalVariableConflictsWithInstanceVariable signalMethodCannotAccessInstanceVariable signalTargetAlreadyImplements: targetAlreadyImplementsErrorMessageFor:)
('pre-conditions' assertCanMove: assertCanMoveAll:of: assertIsNotAccessingInstanceVariable: assertLocalVariableDoesNotConflictWithInstanceVariable: assertTargetDoesNotImplement:)
('sides' isCurrentSide: targetClassOf:)
('implementors and senders' addImplementorsOf:to:andSendersTo:of: implementorsOnCurrentSideAmong:)
!

