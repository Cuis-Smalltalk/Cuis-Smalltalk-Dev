Class {
	#name : 'ChangeSelectorApplier',
	#superclass : 'ScopedRefactoringApplier',
	#instVars : [
		'oldSelector',
		'wizardStepWindow',
		'shouldShowChanges',
		'browser',
		'implementors',
		'senders'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ChangeSelectorApplier class >> applyFrom: aCodeProvider [
	
	aCodeProvider selectedMessageName ifNotNil: [ :oldSelector |
		self
			createAndValueHandlingExceptionsOn: aCodeProvider
			for: oldSelector
			in: aCodeProvider selectedClassOrMetaClass ].
]

{ #category : 'pre-conditions' }
ChangeSelectorApplier class >> assertCanApplyRefactoringFor: aSelector in: aSelectedClass [

	self subclassResponsibility 
]

{ #category : 'instance creation' }
ChangeSelectorApplier class >> createAndValueHandlingExceptionsOn: aModel for: anOldSelector in: aClassToRefactor [

	self createAndValueHandlingExceptions: [ self on: aModel for: anOldSelector in: aClassToRefactor ]
]

{ #category : 'instance creation' }
ChangeSelectorApplier class >> on: aMessageNode createAndValueHandlingExceptionsOn: aModel in: aSelectedClass at: aSelectedSelector [

	^self createAndValueHandlingExceptionsOn: aModel for: aMessageNode selector key in: aSelectedClass

]

{ #category : 'instance creation' }
ChangeSelectorApplier class >> on: aBrowser for: aSelector in: aSelectedClass [

	self assertCanApplyRefactoringFor: aSelector in: aSelectedClass.

	^self new initializeOn: aBrowser for: aSelector in: aSelectedClass
	
]

{ #category : 'refactoring - parameters request' }
ChangeSelectorApplier >> askForImplementosAndSenders [

	self 
		askScope;
		initializeImplementorsAndSenders;
		calculateImplementorsAndSenders;
		startWizard 
]

{ #category : 'refactoring - changes' }
ChangeSelectorApplier >> closeBrowser [
	
	wizardStepWindow ifNotNil: [ wizardStepWindow delete ]	
]

{ #category : 'refactoring - parameters request' }
ChangeSelectorApplier >> collectionToAddSendersTo [

	^senders
]

{ #category : 'refactoring - parameters request' }
ChangeSelectorApplier >> collectionToAddimplementorsTo [

	^implementors 
]

{ #category : 'refactoring - creation' }
ChangeSelectorApplier >> convertSendersToCompiledMethods [

	senders := senders collect: [ :aMethodReference | aMethodReference compiledMethod ]
]

{ #category : 'refactoring - creation' }
ChangeSelectorApplier >> createAndApplyRefactoring [

	self convertSendersToCompiledMethods.
	
	self 
		createRefactoringHandlingRefactoringExceptions;
		applyRefactoring;
		informChangesToBrowser.
			
	shouldShowChanges ifTrue: [ self showChanges ]
	
			
]

{ #category : 'refactoring - creation' }
ChangeSelectorApplier >> createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor  [
		
	implementors := Array with: anImplementor.
	senders := #().
	shouldShowChanges := false.
	
	self createAndApplyRefactoring 
]

{ #category : 'refactoring - parameters request' }
ChangeSelectorApplier >> createImplementors [

	^IdentitySet new.
]

{ #category : 'refactoring - parameters request' }
ChangeSelectorApplier >> createSenders [

	^Set new

]

{ #category : 'refactoring - changes' }
ChangeSelectorApplier >> doNotShowChanges [

	shouldShowChanges := false
]

{ #category : 'evaluation' }
ChangeSelectorApplier >> ifHasNoSendersAndOneImplementor: trueBlock ifNot: falseBlock [

	| allImplementors |
	
	allImplementors := Smalltalk allImplementorsOf: oldSelector.
	
	"I could try to see if there is one sender and that that sender is in the same method beeing renamed. That could
	mean that it is a recursive call but I should also see if the receiver is self to be sure because if it is other 'type' of 
	object the rename could not be safe. To complex for a small posibility - Hernan"
	(allImplementors size = 1 and: [ (Smalltalk allCallsOn: oldSelector) isEmpty ]) 
		ifTrue: [ trueBlock value: allImplementors anyOne compiledMethod ]
		ifFalse: falseBlock
]

{ #category : 'accessing' }
ChangeSelectorApplier >> implementors [
	
	^implementors 
]

{ #category : 'accessing' }
ChangeSelectorApplier >> implementors: implementorsCollection [

	implementors := implementorsCollection 
]

{ #category : 'refactoring - parameters request' }
ChangeSelectorApplier >> implementorsIsOnlyOneTestMethod [

	^ implementors size = 1 and: [ implementors anyOne isTestMethod ]
]

{ #category : 'refactoring - changes' }
ChangeSelectorApplier >> informChangesToBrowser [

	browser selector: oldSelector refactoredTo: refactoring newSelector
	
]

{ #category : 'refactoring - parameters request' }
ChangeSelectorApplier >> initializeImplementorsAndSenders [

	implementors := self createImplementors.
	senders := self createSenders
]

{ #category : 'initialization' }
ChangeSelectorApplier >> initializeOn: aBrowser for: aSelector in: aSelectedClass [

	oldSelector := aSelector.
	selectedClass := aSelectedClass.
	browser := aBrowser.
	shouldShowChanges := true.
]

{ #category : 'refactoring - changes' }
ChangeSelectorApplier >> messageSetWindowClass [
		
	^MethodSetWindow 
	
]

{ #category : 'accessing' }
ChangeSelectorApplier >> oldSelector [
	
	^oldSelector 
]

{ #category : 'refactoring - parameters request' }
ChangeSelectorApplier >> openChangeSelectorSendersStepWindow [

	ChangeSelectorSendersStepWindow openFrom: self 
]

{ #category : 'refactoring - creation' }
ChangeSelectorApplier >> refactoringClass [

	self subclassResponsibility 
]

{ #category : 'refactoring - parameters request' }
ChangeSelectorApplier >> selectorToLookImplementorsFor [

	^oldSelector 
]

{ #category : 'accessing' }
ChangeSelectorApplier >> senders [
	
	^senders 
]

{ #category : 'accessing' }
ChangeSelectorApplier >> senders: sendersCollection [
 
	senders := sendersCollection 
]

{ #category : 'refactoring - changes' }
ChangeSelectorApplier >> showChanges [
		
	self showChangesInMessageSetWindow
]

{ #category : 'refactoring - changes' }
ChangeSelectorApplier >> showChangesInMessageSetWindow [

	self messageSetWindowClass openMessageList: changes asSortedCollection label: 'Changed methods' 
]

{ #category : 'refactoring - parameters request' }
ChangeSelectorApplier >> startWizard [
	
	self implementorsIsOnlyOneTestMethod
		ifTrue: [ self doNotShowChanges; wizardEnded ]
		ifFalse: [ ChangeSelectorImplementorsStepWindow openFrom: self ]
]

{ #category : 'evaluation' }
ChangeSelectorApplier >> value [

	requestExitBlock := [ ^self ].
		
	self requestRefactoringParametersHandlingRefactoringExceptions.
	
	self 
		ifHasNoSendersAndOneImplementor: [ :anImplementor | 
			self createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor ]
		ifNot: [ self askForImplementosAndSenders ]
]

{ #category : 'evaluation' }
ChangeSelectorApplier >> wizardEnded [

	requestExitBlock := [ ^self ].
	
	self 
		closeBrowser;
		createAndApplyRefactoring.
]

{ #category : 'accessing' }
ChangeSelectorApplier >> wizardStepWindow: aWizarStepWindow  [
	
	wizardStepWindow := aWizarStepWindow 
]
