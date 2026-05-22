"
I can apply the InlineMethod refactoring to a message. I am in charge of handling the configuration windows and instantiating an instance of InlineMethod.
Implementation notes:

- A single implementor will have to be chosen.
- More than one message send can be refactored.
- An option to delete the implementor is provided.
"
Class {
	#name : 'InlineMethodApplier',
	#superclass : 'ScopedRefactoringApplier',
	#instVars : [
		'selectorToInline',
		'wizardStepWindow',
		'shouldShowChanges',
		'browser',
		'shouldRemoveImplementor',
		'triggeringMessageSend',
		'shouldInlineTriggeringMessageSendOnly',
		'implementors',
		'messageSends'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
InlineMethodApplier class >> applyFrom: aCodeProvider [
	
	aCodeProvider selectedMessageName ifNotNil: [ :selectorToInline |
		self
			createAndValueHandlingExceptionsOn: aCodeProvider 
			forMethod: (MethodReference
				class: aCodeProvider selectedClassOrMetaClass 
				selector: selectorToInline )]
]

{ #category : 'instance creation' }
InlineMethodApplier class >> createAndValueHandlingExceptions: creationBlock [


	[super createAndValueHandlingExceptions: creationBlock]
		on: Refactoring refactoringErrorClass
		do: [ :refactoringError | ^self inform: refactoringError messageText ].

]

{ #category : 'instance creation' }
InlineMethodApplier class >> createAndValueHandlingExceptionsOn: aModel forMessageSend: aMessageNodeReference [

	self createAndValueHandlingExceptions: [ self on: aModel forMessageSend: aMessageNodeReference ].

]

{ #category : 'instance creation' }
InlineMethodApplier class >> createAndValueHandlingExceptionsOn: aModel forMethod: aMethodReference [

	self createAndValueHandlingExceptions: [ self on: aModel forMethod: aMethodReference ].

]

{ #category : 'instance creation' }
InlineMethodApplier class >> implementorsNotFoundErrorMessage [

	^'There are no implementors for this message in the selected scope'
]

{ #category : 'instance creation' }
InlineMethodApplier class >> on: aModel forMessageSend: aMessageNodeReference [

	^self new initializeOn: aModel forMessageSend: aMessageNodeReference
	
]

{ #category : 'instance creation' }
InlineMethodApplier class >> on: aModel forMethod: aMethodReference [

	"To be called when calling from method selector or when inlining self method from the code editor"

	^self new initializeOn: aModel for: aMethodReference selector in: aMethodReference actualClass.
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> askForImplementorsAndMessageSends [

	self
		initializeImplementorsAndMessageSends;
		calculateImplementorsAndSenders;
		openImplementorSelectionWindow.
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> askForImplementorsOnly [

	self
		initializeImplementors;
		calculateImplementorsAndSenders;
		openImplementorSelectionWindow.
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> askForMessagesToInlineOnly [

	self
		initializeMessageSends;
		calculateImplementorsAndSenders;
		openMessageSendsSelectionWindow.
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> askIfImplementorShouldBeRemoved [

	self askIfImplementorShouldBeRemoved: 'Do you want to remove the implementor method from the system?'.
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> askIfImplementorShouldBeRemoved: aMessage [

	shouldRemoveImplementor := PopUpMenu confirm: aMessage orCancel: [ self endRequest ]
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> askIfImplementorShouldBeRemovedWhenNoSenders [

	self askIfImplementorShouldBeRemoved: 'This message has no senders so inlining is a no-op. Do you want to remove the implementor method from the system?'.
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> askIfOnlyTriggeringMessageSendShouldBeInlined [

	| scopeMenu choice |

	scopeMenu := PopUpMenu labelArray: #('Inline only this message send' 'Select message sends to inline').
	choice := scopeMenu startUpWithCaption: 'What message sends should be inlined?'.
	choice = 0 ifTrue: [ self endRequest ].
	shouldInlineTriggeringMessageSendOnly := choice = 1
]

{ #category : 'refactoring - changes' }
InlineMethodApplier >> closeBrowser [

	wizardStepWindow delete.
	
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> collectionToAddSendersTo [

	"This is implemented like this so we don't have to duplicate every implementorsAndUsagesInXXX method to
	only calculate implementors or only usages. We always calculate both but if they are not needed they end
	up in this collection that will not be used in the refactoring process - Fernando"

	^self shouldAskForMessageSendsToInline ifTrue: [messageSends] ifFalse: [^Set new].
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> collectionToAddimplementorsTo [

	"This is implemented like this so we don't have to duplicate every implementorsAndMessageSendsInXXX method to
	only calculate implementors or only usages. We always calculate both but if they are not needed they end
	up in this collection that will not be used in the refactoring process - Fernando"

	^self shouldAskForImplementors ifTrue: [implementors] ifFalse: [^Set new]
]

{ #category : 'refactoring - creation' }
InlineMethodApplier >> createAndApplyRefactoring [

	self
		removeMessageSendsBelongingToImplementor;
		createRefactoringHandlingRefactoringExceptions;
		applyRefactoring.

	shouldShowChanges ifTrue: [ self showChanges ]

			
]

{ #category : 'refactoring - creation' }
InlineMethodApplier >> createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor [

	implementors := Array with: anImplementor.
	messageSends := #().
	shouldShowChanges := false.

	self createAndApplyRefactoring 
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> createImplementors [

	^IdentitySet new.
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> createMessageSends [

	^OrderedCollection new.

]

{ #category : 'refactoring - creation' }
InlineMethodApplier >> createRefactoring [

	self assert: implementors size = 1.
	^self refactoringClass
		from: (implementors anyOne)
		intoSendersAndUsages: messageSends
		removeMethod: shouldRemoveImplementor

			
]

{ #category : 'refactoring - changes' }
InlineMethodApplier >> doNotShowChanges [

	shouldShowChanges := false
]

{ #category : 'evaluation' }
InlineMethodApplier >> ifHasNoUsagesAndOneImplementor: trueBlock ifNot: falseBlock [

	| allImplementors |

	allImplementors := Smalltalk allImplementorsOf: selectorToInline.

	"I could try to see if there is one sender and that that sender is in the same method beeing renamed. That could
	mean that it is a recursive call but I should also see if the receiver is self to be sure because if it is other 'type' of
	object the rename could not be safe. To complex for a small posibility - Hernan"
	(allImplementors size = 1 and: [ (Smalltalk allCallsOn: selectorToInline) isEmpty ])
		ifTrue: [ trueBlock value: allImplementors anyOne compiledMethod ]
		ifFalse: falseBlock
]

{ #category : 'accessing' }
InlineMethodApplier >> implementors [

	^implementors 
]

{ #category : 'accessing' }
InlineMethodApplier >> implementors: aCollectionOfImplementors [

	implementors := aCollectionOfImplementors 
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> initializeImplementors [

	implementors := self createImplementors.
	
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> initializeImplementorsAndMessageSends [

	self initializeImplementors.
	self initializeMessageSends.
	
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> initializeMessageSends [

	messageSends := self createMessageSends
	
]

{ #category : 'initialization' }
InlineMethodApplier >> initializeOn: aBrowser for: aSelector in: aSelectedClass [

	"To be used when initiating refactoring from message selector pane"

	selectorToInline := aSelector.
	selectedClass := aSelectedClass.
	browser := aBrowser.
	shouldInlineTriggeringMessageSendOnly := false.
	shouldShowChanges := true.
]

{ #category : 'initialization' }
InlineMethodApplier >> initializeOn: aBrowser forMessageSend: aMessageNodeReference [

	"To be used when initiating refactoring from a message send"

	selectorToInline := aMessageNodeReference selectorOfMessageNode.
	triggeringMessageSend := aMessageNodeReference.
	selectedClass := aMessageNodeReference actualClass.
	browser := aBrowser.
	shouldShowChanges := true.
]

{ #category : 'evaluation' }
InlineMethodApplier >> isOnMessageSend [

	^triggeringMessageSend isNil not.
]

{ #category : 'accessing' }
InlineMethodApplier >> messageSendAt: anIndex ifAbsent: ifAbsentBlock [

	^messageSends at: anIndex ifAbsent: ifAbsentBlock
]

{ #category : 'accessing' }
InlineMethodApplier >> messageSends: aCollectionOfMessageSends [

	messageSends := aCollectionOfMessageSends reject: [:aMessageNodeReference | aMessageNodeReference isMessageListTitle ]
]

{ #category : 'accessing' }
InlineMethodApplier >> messageSendsToInline [

	^messageSends 
]

{ #category : 'refactoring - changes' }
InlineMethodApplier >> messageSetWindowClass [

	^MethodSetWindow
	
]

{ #category : 'accessing' }
InlineMethodApplier >> methodsOfUsagesToInline [

	^messageSends collect: [:aMessageNodeReference | aMessageNodeReference methodReference]
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> openImplementorSelectionWindow [

	| windowClass |

	implementors isEmpty ifTrue: [self handleRefactoringError: (RefactoringError signal: self class implementorsNotFoundErrorMessage)].
	windowClass := self shouldAskForMessageSendsToInline
		ifTrue: [InlineMethodImplementorsWithShowUsagesStepWindow]
		ifFalse: [InlineMethodImplementorsStepWindow].
	^windowClass openFrom: self.
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> openMessageSendsSelectionWindow [

	| windowClass |

	windowClass := self shouldAskForImplementors
		ifTrue: [InlineMethodUsagesWithShowImplementorsStepWindow]
		ifFalse: [InlineMethodUsagesStepWindow].
	^windowClass openFrom: self.
]

{ #category : 'refactoring - creation' }
InlineMethodApplier >> refactoringClass [

	^InlineMethod 
]

{ #category : 'refactoring - creation' }
InlineMethodApplier >> removeMessageSendsBelongingToImplementor [

	messageSends := messageSends reject: [:messageNodeReference |
		messageNodeReference compiledMethod equivalentTo: implementors anyOne
	]

]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> requestRefactoringParameters [

	self isOnMessageSend ifTrue: [self askIfOnlyTriggeringMessageSendShouldBeInlined].
	self askIfImplementorShouldBeRemoved.
]

{ #category : 'accessing' }
InlineMethodApplier >> selectorToInline [

	^selectorToInline 
]

{ #category : 'refactoring - parameters request' }
InlineMethodApplier >> selectorToLookImplementorsFor [

	^selectorToInline 
]

{ #category : 'accessing' }
InlineMethodApplier >> shouldAskForImplementors [

	^triggeringMessageSend isNil not.
]

{ #category : 'accessing' }
InlineMethodApplier >> shouldAskForMessageSendsToInline [

	^shouldInlineTriggeringMessageSendOnly not.
]

{ #category : 'refactoring - changes' }
InlineMethodApplier >> showChanges [

	self showChangesInMessageSetWindow
]

{ #category : 'refactoring - changes' }
InlineMethodApplier >> showChangesInMessageSetWindow [

	self messageSetWindowClass openMessageList: changes asSortedCollection label: 'Changed methods' 
]

{ #category : 'evaluation' }
InlineMethodApplier >> value [

	requestExitBlock := [ ^self ].

	self requestRefactoringParametersHandlingRefactoringExceptions.

	self
		ifHasNoUsagesAndOneImplementor: [ :anImplementor |
			shouldRemoveImplementor ifFalse: [self askIfImplementorShouldBeRemovedWhenNoSenders ]. "Give the user another chance to delete the message"
			shouldRemoveImplementor ifFalse: [^self].
			self createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor
		]
		ifNot: [
			self askScope.
			shouldInlineTriggeringMessageSendOnly ifTrue: [
				messageSends := OrderedCollection new.
				messageSends add: triggeringMessageSend.
				^self askForImplementorsOnly
			].
			self shouldAskForImplementors ifFalse: [implementors := IdentitySet with: (selectedClass >> selectorToInline). ^self askForMessagesToInlineOnly].
			self askForImplementorsAndMessageSends.
	]
]

{ #category : 'evaluation' }
InlineMethodApplier >> wizardEnded [

	requestExitBlock := [ ^self ].

	self
		closeBrowser;
		createAndApplyRefactoring.
]

{ #category : 'accessing' }
InlineMethodApplier >> wizardStepWindow: aWizarStepWindow [

	wizardStepWindow := aWizarStepWindow 
]
