'From Cuis7.9 [latest update: #8184] on 16 September 2026 at 7:43:13 pm'!

!MoveToInstanceOrClassMethod commentStamp: '<historical>' prior: 0!
I move the implementors of a selector on one side of their classes to the other side, instance methods to the class side or class methods to the instance side, and redirect the senders I am given so that they keep reaching them.

The implementors and the senders come from a scope, the way they do for a selector change; the implementors on the other side of the one selected are left alone, and for: alone takes the whole system. My subclasses know the two directions and for: answers the one the method needs.

A sender is redirected by rewriting the receiver of each send I am told to redirect: an instance method sending it to self sends it to self class once it is a class method; one sending it to self class, or to the class by name, sends it to self once it is an instance method, or to a new instance of the class when it is not one itself; and a new instance sent it by name is sent it as a class. Which sends those are is answered by the closure I am created with: the ones whose receiver is written that way and reaches an implementor, or, with LiveTyping, the ones it saw reach one.

The body of each implementor is retargeted too, so that every send to self keeps reaching the method it reached on the side it was written for: going up, self class bar becomes self bar; going down, self bar becomes self class bar whenever the two sides resolve bar to different methods; and a body going up that sends to itself something the class side resolves differently is refused, since no instance is at hand there.!


!MoveToInstanceOrClassMethod methodsFor: 'retargeting the body - private' stamp: 'HAW 16/Sep/2026, 7:21:00 pm (UTC)'!
retargetSelfSend: aNode in: aMethodNode of: anImplementor addingTo: someRetargets

	| receiverText receiverRange |

	(aNode isMessageNode and: [ aNode isCascade not ]) ifFalse: [ ^self ].
	receiverText := self
		retargetedReceiverOf: aNode
		in: anImplementor
		ifNone: [ ^self ].
	receiverRange := (aNode receiverNodeSourceRangesIn: aMethodNode) value.

	someRetargets add: receiverRange -> receiverText! !

!MoveToInstanceOrClassMethod methodsFor: 'retargeting the body - private' stamp: 'HAW 16/Sep/2026, 7:19:59 pm (UTC)'!
retargetedReceiverOf: aSend in: anImplementor ifNone: aNoneBlock

	^self subclassResponsibility! !

!MoveToInstanceOrClassMethod methodsFor: 'retargeting the body - private' stamp: 'HAW 16/Sep/2026, 7:21:00 pm (UTC)'!
sourceToMoveOf: anImplementor

	| methodNode retargets |

	methodNode := anImplementor notOptimizedMethodNode.
	retargets := SortedCollection sortBlock: [ :aRetarget :anotherRetarget |
		aRetarget key first <= anotherRetarget key first ].
	methodNode completeSourceRanges keys do: [ :aNode |
		self
			retargetSelfSend: aNode
			in: methodNode
			of: anImplementor
			addingTo: retargets ].

	^anImplementor sourceCode copyReplacing: retargets! !


!MoveToInstanceOrClassMethod class methodsFor: 'exceptions' stamp: 'HAW 16/Sep/2026, 7:19:59 pm (UTC)'!
selfSendNotUnderstoodErrorMessageFor: anImplementor sending: aSelector

	^'Can not move ' , anImplementor classAndSelector , ' because it sends #' , aSelector ,
		' to itself and the other side does not understand it'! !

!MoveToInstanceOrClassMethod class methodsFor: 'exceptions' stamp: 'HAW 16/Sep/2026, 7:21:00 pm (UTC)'!
signalSelfSendNotUnderstoodIn: anImplementor sending: aSelector

	self refactoringError: (self
		selfSendNotUnderstoodErrorMessageFor: anImplementor
		sending: aSelector)! !

!MoveToInstanceOrClassMethod class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 7:19:59 pm (UTC)'!
assertSelfSendsAreUnderstoodAfterMoving: anImplementor

	^self subclassResponsibility! !

!MoveToInstanceOrClassMethod class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 7:38:07 pm (UTC)'!
isSelfSendToRetarget: aSelector of: anImplementor

	| resolvedOnCurrentSide resolvedOnTarget |

	aSelector = anImplementor selector ifTrue: [ ^false ].
	resolvedOnCurrentSide := anImplementor methodClass lookupSelector: aSelector.
	resolvedOnCurrentSide ifNil: [ ^false ].
	resolvedOnTarget := (self targetClassOf: anImplementor) lookupSelector: aSelector.

	^resolvedOnCurrentSide ~~ resolvedOnTarget! !


!MoveToClassMethod methodsFor: 'retargeting the body - private' stamp: 'HAW 16/Sep/2026, 7:38:07 pm (UTC)'!
retargetedReceiverOf: aSend in: anImplementor ifNone: aNoneBlock

	^aSend receiver isSendOfClassToSelf
		ifTrue: [ 'self' ]
		ifFalse: aNoneBlock! !


!MoveToClassMethod class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 7:21:00 pm (UTC)'!
assertSelfSendsAreUnderstoodAfterMoving: anImplementor

	anImplementor notOptimizedMethodNode completeSourceRanges keys do: [ :aNode |
		(self isSelfSendLeftBehind: aNode movingUp: anImplementor) ifTrue: [
			self signalSelfSendNotUnderstoodIn: anImplementor sending: aNode selectorSymbol ] ]! !

!MoveToClassMethod class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 7:38:04 pm (UTC)'!
isSelfSendLeftBehind: aNode movingUp: anImplementor

	aNode isMessageNode ifFalse: [ ^false ].
	aNode receiverOrCascadeReceiver isSelfPseudoVariable ifFalse: [ ^false ].

	^self isSelfSendToRetarget: aNode selectorSymbol of: anImplementor! !


!MoveToInstanceMethod methodsFor: 'retargeting the body - private' stamp: 'HAW 16/Sep/2026, 7:38:04 pm (UTC)'!
retargetedReceiverOf: aSend in: anImplementor ifNone: aNoneBlock

	aSend receiver isSelfPseudoVariable ifFalse: [ ^aNoneBlock value ].

	^(self class isSelfSendToRetarget: aSend selectorSymbol of: anImplementor)
		ifTrue: [ 'self class' ]
		ifFalse: aNoneBlock! !


!MoveToInstanceMethod class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 7:19:59 pm (UTC)'!
assertSelfSendsAreUnderstoodAfterMoving: anImplementor
! !


!MoveToInstanceOrClassMethod methodsFor: 'applying - private' stamp: 'HAW 16/Sep/2026, 7:21:00 pm (UTC)'!
move: anImplementor

	| targetClass movedSelector |

	targetClass := self class targetClassOf: anImplementor.
	movedSelector := targetClass
		compile: (self sourceToMoveOf: anImplementor)
		classified: anImplementor category.
	self noteChangeIn: targetClass at: movedSelector.
	anImplementor methodClass removeSelector: anImplementor selector.
	self noteChangeIn: anImplementor methodClass at: anImplementor selector! !


!MoveToInstanceOrClassMethod class methodsFor: 'pre-conditions' stamp: 'HAW 16/Sep/2026, 7:19:59 pm (UTC)'!
assertCanMove: anImplementor

	self
		assertIsNotAccessingInstanceVariable: anImplementor;
		assertLocalVariableDoesNotConflictWithInstanceVariable: anImplementor;
		assertTargetDoesNotImplement: anImplementor;
		assertSelfSendsAreUnderstoodAfterMoving: anImplementor! !

!methodRemoval: MoveToInstanceOrClassMethod class #isUnderstoodOnlyOnCurrentSide:of: stamp: 'HAW 16/Sep/2026, 7:38:04 pm (UTC)'!
MoveToInstanceOrClassMethod class removeSelector: #isUnderstoodOnlyOnCurrentSide:of:!

!MoveToClassMethod class reorganize!
('sides' isCurrentSide: targetClassOf:)
('pre-conditions' assertSelfSendsAreUnderstoodAfterMoving: isSelfSendLeftBehind:movingUp:)
!


!MoveToInstanceMethod reorganize!
('redirecting senders - private' behaviorReceiving:in:ifNone: redirect:written:in: redirectLiteralClass:written:in:)
('retargeting the body - private' retargetedReceiverOf:in:ifNone:)
!

