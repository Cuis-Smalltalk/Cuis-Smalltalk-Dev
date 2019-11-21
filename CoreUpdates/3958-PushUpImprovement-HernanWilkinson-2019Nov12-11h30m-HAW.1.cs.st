'From Cuis 5.0 [latest update: #3956] on 12 November 2019 at 11:49:46 am'!
!classDefinition: #PushUpMethod category: #'Tools-Refactoring'!
Refactoring subclass: #PushUpMethod
	instanceVariableNames: 'method'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushUpMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #PushUpMethodApplier
	instanceVariableNames: 'browser methodToPushUp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!CompiledMethod methodsFor: 'testing' stamp: 'MSC 10/14/2019 13:44:00'!
accessesInstanceVariable: aName

	^(self readsInstanceVariable: aName) or: [self writesInstanceVariable: aName].! !


!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'HAW 11/12/2019 11:30:59'!
assertIsNotAccessingInstanceVariable: aMethodToPushUp
	
	aMethodToPushUp methodClass instVarNames 
		do: [ :instVarName | 
			(aMethodToPushUp accessesInstanceVariable: instVarName) ifTrue: [self signalMethodCannotAccessInstanceVariable]].! !

!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'MSC 10/14/2019 20:57:57'!
assertIsValidToPushUpMethod: aMethodToPushUp

	self assertIsNotAccessingInstanceVariable: aMethodToPushUp.
! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'MSC 10/19/2019 23:33:15'!
warnIfExistMethodToPushUpOnSuperClass: aMethodToPushUp 

	((aMethodToPushUp methodClass superclass) methodDict includesKey: aMethodToPushUp selector) 
		ifTrue: [self refactoringWarning: self warningMesssageForExistMethodToPushUpOnSuperClass ].! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'MSC 10/19/2019 22:41:32'!
warnIsValidToPushUpMethod: aMethodToPushUp

	self warnIfExistMethodToPushUpOnSuperClass: aMethodToPushUp.
! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'HAW 11/12/2019 11:39:38'!
warningMesssageForExistMethodToPushUpOnSuperClass

	^'Method already exist in superclass'.
		
	! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 11/12/2019 11:38:58'!
errorMessageForMethodCannotAccessInstanceVariable
	
	^ 'Can not push up a method that access an instance variable'! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'MSC 10/19/2019 23:29:03'!
signalMethodCannotAccessInstanceVariable
	
	self refactoringError: self errorMessageForMethodCannotAccessInstanceVariable! !


!PushUpMethodApplier methodsFor: 'initialization' stamp: 'MSC 10/14/2019 13:09:53'!
initializeOn: aBrowser for: aMethodToPushUp
	
	browser := aBrowser.
	methodToPushUp := aMethodToPushUp.! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 11/12/2019 11:41:50'!
requestRefactoringParameters

	! !

!PushUpMethodApplier methodsFor: 'refactoring - creation' stamp: 'MSC 10/14/2019 19:37:02'!
createRefactoring
		
	^self refactoringClass for: methodToPushUp.! !

!PushUpMethodApplier methodsFor: 'refactoring - creation' stamp: 'MSC 10/14/2019 19:36:48'!
refactoringClass

	^PushUpMethod! !

!PushUpMethodApplier methodsFor: 'refactoring - changes' stamp: 'MSC 10/14/2019 13:10:22'!
informChangesToBrowser

	| classMethod |
	
	classMethod := methodToPushUp methodClass asString.
	
	browser classListIndex: (browser classListIndexOf: classMethod).! !

!PushUpMethodApplier methodsFor: 'refactoring - changes' stamp: 'MSC 10/14/2019 13:09:46'!
showChanges

	self informChangesToBrowser.! !


!PushUpMethodApplier class methodsFor: 'instance creation' stamp: 'MSC 10/14/2019 12:11:10'!
on: aBrowser for: aMethodToPushUp
	
	^self new initializeOn: aBrowser for: aMethodToPushUp ! !


!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 11/12/2019 11:44:45'!
pushUpSelector

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(PushUpMethodApplier on: model for: model selectedClassOrMetaClass>>selectedSelector) value ].! !


!PushUpMethod methodsFor: 'initialization' stamp: 'HAW 8/18/2018 11:44:09'!
initializeFor: aMethodToPushup

	method := aMethodToPushup ! !

!PushUpMethod methodsFor: 'applying' stamp: 'HAW 3/4/2019 15:23:40'!
apply

	| methodCategory |

	methodCategory := method methodClass organization categoryOfElement: method selector.
	method methodClass superclass
		compile: method sourceCode
		classified: methodCategory.

	method methodClass removeSelector: method selector.
 ! !


!PushUpMethod class methodsFor: 'instance creation' stamp: 'MSC 10/19/2019 22:42:30'!
for: aMethodToPushUp

	self assertIsValidToPushUpMethod: aMethodToPushUp.
	self warnIsValidToPushUpMethod: aMethodToPushUp.

	^self new initializeFor: aMethodToPushUp! !

!methodRemoval: PushUpMethodApplier #askConfirmation stamp: 'HAW 11/12/2019 11:41:54'!
PushUpMethodApplier removeSelector: #askConfirmation!
!methodRemoval: PushUpMethodApplier #confirmationMessageText stamp: 'HAW 11/12/2019 11:42:00'!
PushUpMethodApplier removeSelector: #confirmationMessageText!
