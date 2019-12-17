'From Cuis 5.0 [latest update: #3969] on 14 December 2019 at 11:29:06 am'!
!classDefinition: #MoveMethod category: #'Tools-Refactoring'!
Refactoring subclass: #MoveMethod
	instanceVariableNames: 'method'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #MoveToInstanceOrClassMethod category: #'Tools-Refactoring'!
MoveMethod subclass: #MoveToInstanceOrClassMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushDownMethod category: #'Tools-Refactoring'!
MoveMethod subclass: #PushDownMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushUpMethod category: #'Tools-Refactoring'!
MoveMethod subclass: #PushUpMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #MoveMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #MoveMethodApplier
	instanceVariableNames: 'browser methodToMove'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #MoveToInstanceOrClassMethodApplier category: #'Tools-Refactoring'!
MoveMethodApplier subclass: #MoveToInstanceOrClassMethodApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushDownMethodApplier category: #'Tools-Refactoring'!
MoveMethodApplier subclass: #PushDownMethodApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushUpMethodApplier category: #'Tools-Refactoring'!
MoveMethodApplier subclass: #PushUpMethodApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!MoveMethod methodsFor: 'initialization' stamp: 'HAW 12/14/2019 11:04:04'!
initializeFor: aMethodToPushup

	method := aMethodToPushup! !

!MoveMethod methodsFor: 'applying - private' stamp: 'HAW 12/14/2019 11:05:51'!
moveMethod

	self subclassResponsibility ! !

!MoveMethod methodsFor: 'applying - private' stamp: 'HAW 12/14/2019 11:09:30'!
removeMethod

	method methodClass removeSelector: method selector.
 ! !

!MoveMethod methodsFor: 'applying' stamp: 'HAW 12/14/2019 11:05:41'!
apply

	self
		moveMethod;
		removeMethod
 ! !

!MoveMethod methodsFor: 'applying' stamp: 'HAW 12/14/2019 11:05:01'!
methodCategory

	^method methodClass organization categoryOfElement: method selector! !


!MoveToInstanceOrClassMethod methodsFor: 'applying - private' stamp: 'HAW 12/14/2019 11:08:02'!
moveMethod

	| targetClass |

	targetClass := method methodClass isMeta
		ifTrue: [method methodClass soleInstance]
		ifFalse: [method methodClass class].

	targetClass
		compile: method sourceCode
		classified: self methodCategory.

	! !


!PushDownMethod methodsFor: 'applying - private' stamp: 'HAW 12/14/2019 11:10:31'!
moveMethod

	| methodCategory sourceCode |

	methodCategory := self methodCategory.
	sourceCode := method sourceCode.

	method methodClass subclassesDo: [:subclass |
		subclass
			compile: sourceCode
			classified: methodCategory.
	].
! !


!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'HAW 12/14/2019 10:27:16'!
addShadowedInstVarsOf: subclass in: aMethodToPushDown to: subclassesWithShadowedInstVarNames

	| shadowedInstVarNames |

	shadowedInstVarNames := subclass instVarNames select: [:instVarName |
		(aMethodToPushDown hasArgumentOrTemporary: instVarName) ].

	 shadowedInstVarNames ifNotEmpty: [
		subclassesWithShadowedInstVarNames at: subclass put: shadowedInstVarNames ]! !

!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'HAW 12/14/2019 10:26:56'!
assertIsValidToPushDownMethod: aMethodToPushDown

	| subclassesWithShadowedInstVarNames |

	subclassesWithShadowedInstVarNames := Dictionary new.

	aMethodToPushDown methodClass subclassesDo: [:subclass |
		self addShadowedInstVarsOf: subclass in: aMethodToPushDown to: subclassesWithShadowedInstVarNames ].

	subclassesWithShadowedInstVarNames ifNotEmpty: [
			self signalMethodCannotShadowInstVarOfSubclasses: subclassesWithShadowedInstVarNames]
		! !

!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'HAW 12/14/2019 10:57:17'!
warnIfExistMethodToPushDownOnSubClass: aMethodToPushDown

	| subclassesImplementingMessage |

	subclassesImplementingMessage := aMethodToPushDown methodClass subclasses select: [:subclass |
		subclass includesSelector: aMethodToPushDown selector ].

	subclassesImplementingMessage ifNotEmpty: [
		self warnMessageAlreadyImplementedIn: subclassesImplementingMessage ]
	! !

!PushDownMethod class methodsFor: 'exceptions' stamp: 'HAW 12/14/2019 11:23:44'!
errorMessageCanNotPushDownWithShadowedInstVarsOf: subclassesWithShadowedInstVarNames

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not push down method because the following subclasses';
			newLine;
			nextPutAll: 'would have shadowed instance variables:'.

		subclassesWithShadowedInstVarNames
			keysAndValuesDo: [ :class :instVarNames |
				stream
					newLine;
					print: class;
					nextPutAll: ': '.
				instVarNames asCommaSeparated: [:instVarName | stream nextPutAll: instVarName ] on: stream ]].


! !

!PushDownMethod class methodsFor: 'exceptions' stamp: 'HAW 12/14/2019 10:39:22'!
signalMethodCannotShadowInstVarOfSubclasses: subclassesWithShadowedInstVarNames

	self refactoringError: (
		self errorMessageCanNotPushDownWithShadowedInstVarsOf: subclassesWithShadowedInstVarNames)! !

!PushDownMethod class methodsFor: 'warnings' stamp: 'HAW 12/14/2019 10:58:41'!
warnMessageAlreadyImplementedIn: subclassesImplementingMessage

	self refactoringWarning:
		(self warningMesssageForMessageAlreadyImplementedIn: subclassesImplementingMessage)! !

!PushDownMethod class methodsFor: 'warnings' stamp: 'HAW 12/14/2019 10:49:27'!
warningMesssageForMessageAlreadyImplementedIn: subclassesImplementingMessage

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Method already exist in the following subclasses:';
			newLine.

		subclassesImplementingMessage asCommaSeparated: [ :aClass | stream print: aClass ] on: stream.

		stream
			newLine;
			nextPutAll: 'If you continue they will be overwritten' ].! !


!PushUpMethod methodsFor: 'applying - private' stamp: 'HAW 12/14/2019 11:11:15'!
moveMethod

	method methodClass superclass
		compile: method sourceCode
		classified: self methodCategory! !


!PushUpMethod class methodsFor: 'warnings' stamp: 'HAW 12/14/2019 10:54:05'!
methodToPushUpExistOnSuperclassWarningMessage

	^'Method already exist in superclass'.

	! !


!MoveMethodApplier methodsFor: 'initialization' stamp: 'HAW 12/14/2019 11:15:12'!
initializeOn: aBrowser for: aMethodToMove

	browser := aBrowser.
	methodToMove := aMethodToMove.! !

!MoveMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 12/14/2019 11:15:34'!
createRefactoring

	^self refactoringClass for: methodToMove.! !

!MoveMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 12/14/2019 11:19:17'!
refactoringClass

	self subclassResponsibility ! !

!MoveMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 12/14/2019 11:16:56'!
requestRefactoringParameters

	! !

!MoveMethodApplier methodsFor: 'refactoring - changes' stamp: 'HAW 12/14/2019 11:17:14'!
informChangesToBrowser

	browser
		reformulateList;
		changed: #messageList;
		setClassOrganizer! !

!MoveMethodApplier methodsFor: 'refactoring - changes' stamp: 'HAW 12/14/2019 11:17:10'!
showChanges

	self informChangesToBrowser.! !


!MoveMethodApplier class methodsFor: 'instance creation' stamp: 'HAW 12/14/2019 11:13:33'!
on: aBrowser for: aMethodToMove

	^self new initializeOn: aBrowser for: aMethodToMove ! !


!PushDownMethodApplier methodsFor: 'refactoring - creation' stamp: 'fz 12/4/2019 15:38:49'!
refactoringClass

	^PushDownMethod! !


!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 12/14/2019 11:27:21'!
pushDownSelector

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(PushDownMethodApplier on: model for: model selectedClassOrMetaClass>>selectedSelector) value ].! !


!PushDownMethod class methodsFor: 'instance creation' stamp: 'HAW 12/14/2019 10:04:49'!
for: aMethodToPushDown

	self assertIsValidToPushDownMethod: aMethodToPushDown.
	self warnIfExistMethodToPushDownOnSubClass: aMethodToPushDown.

	^self new initializeFor: aMethodToPushDown ! !


!PushUpMethod class methodsFor: 'instance creation' stamp: 'HAW 12/14/2019 10:52:55'!
for: aMethodToPushUp

	self assertIsValidToPushUpMethod: aMethodToPushUp.
	self warnIfExistMethodToPushUpOnSuperClass: aMethodToPushUp.

	^self new initializeFor: aMethodToPushUp! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'HAW 12/14/2019 10:54:19'!
warnIfExistMethodToPushUpOnSuperClass: aMethodToPushUp

	(aMethodToPushUp methodClass superclass includesSelector: aMethodToPushUp selector)
		ifTrue: [self refactoringWarning: self methodToPushUpExistOnSuperclassWarningMessage ].! !

!methodRemoval: PushUpMethodApplier class #on:for: stamp: 'HAW 12/14/2019 11:14:04'!
PushUpMethodApplier class removeSelector: #on:for:!
!methodRemoval: PushUpMethodApplier #createRefactoring stamp: 'HAW 12/14/2019 11:17:35'!
PushUpMethodApplier removeSelector: #createRefactoring!
!methodRemoval: PushUpMethodApplier #initializeOn:for: stamp: 'HAW 12/14/2019 11:17:27'!
PushUpMethodApplier removeSelector: #initializeOn:for:!
!methodRemoval: PushUpMethodApplier #requestRefactoringParameters stamp: 'HAW 12/14/2019 11:17:31'!
PushUpMethodApplier removeSelector: #requestRefactoringParameters!
!methodRemoval: PushUpMethodApplier #showChanges stamp: 'HAW 12/14/2019 11:17:41'!
PushUpMethodApplier removeSelector: #showChanges!
!methodRemoval: PushUpMethodApplier #informChangesToBrowser stamp: 'HAW 12/14/2019 11:17:41'!
PushUpMethodApplier removeSelector: #informChangesToBrowser!
!methodRemoval: PushDownMethodApplier class #on:for: stamp: 'HAW 12/14/2019 11:14:00'!
PushDownMethodApplier class removeSelector: #on:for:!
!methodRemoval: PushDownMethodApplier #informChangesToBrowser stamp: 'HAW 12/14/2019 11:17:14'!
PushDownMethodApplier removeSelector: #informChangesToBrowser!
!methodRemoval: PushDownMethodApplier #initializeOn:for: stamp: 'HAW 12/14/2019 11:16:37'!
PushDownMethodApplier removeSelector: #initializeOn:for:!
!methodRemoval: PushDownMethodApplier #showChanges stamp: 'HAW 12/14/2019 11:17:10'!
PushDownMethodApplier removeSelector: #showChanges!
!methodRemoval: PushDownMethodApplier #createRefactoring stamp: 'HAW 12/14/2019 11:17:03'!
PushDownMethodApplier removeSelector: #createRefactoring!
!methodRemoval: PushDownMethodApplier #requestRefactoringParameters stamp: 'HAW 12/14/2019 11:16:56'!
PushDownMethodApplier removeSelector: #requestRefactoringParameters!
!methodRemoval: MoveToInstanceOrClassMethodApplier class #on:for: stamp: 'HAW 12/14/2019 11:13:33'!
MoveToInstanceOrClassMethodApplier class removeSelector: #on:for:!
!methodRemoval: MoveToInstanceOrClassMethodApplier #createRefactoring stamp: 'HAW 12/14/2019 11:15:34'!
MoveToInstanceOrClassMethodApplier removeSelector: #createRefactoring!
!methodRemoval: MoveToInstanceOrClassMethodApplier #informChangesToBrowser stamp: 'HAW 12/14/2019 11:16:04'!
MoveToInstanceOrClassMethodApplier removeSelector: #informChangesToBrowser!
!methodRemoval: MoveToInstanceOrClassMethodApplier #showChanges stamp: 'HAW 12/14/2019 11:16:06'!
MoveToInstanceOrClassMethodApplier removeSelector: #showChanges!
!methodRemoval: MoveToInstanceOrClassMethodApplier #initializeOn:for: stamp: 'HAW 12/14/2019 11:15:12'!
MoveToInstanceOrClassMethodApplier removeSelector: #initializeOn:for:!
!methodRemoval: PushUpMethod class #warningMesssageForExistMethodToPushUpOnSuperClass stamp: 'HAW 12/14/2019 10:54:05'!
PushUpMethod class removeSelector: #warningMesssageForExistMethodToPushUpOnSuperClass!
!methodRemoval: PushUpMethod class #warnIsValidToPushUpMethod: stamp: 'HAW 12/14/2019 10:52:59'!
PushUpMethod class removeSelector: #warnIsValidToPushUpMethod:!
!methodRemoval: PushUpMethod #initializeFor: stamp: 'HAW 12/14/2019 11:04:24'!
PushUpMethod removeSelector: #initializeFor:!
!methodRemoval: PushUpMethod #apply stamp: 'HAW 12/14/2019 11:11:21'!
PushUpMethod removeSelector: #apply!
!methodRemoval: PushDownMethod class #warningMesssageForExistMethodToPushDownOnSubClass stamp: 'HAW 12/14/2019 10:22:33'!
PushDownMethod class removeSelector: #warningMesssageForExistMethodToPushDownOnSubClass!
!methodRemoval: PushDownMethod class #signalMethodCannotShadowInstVarOfSubclasses stamp: 'HAW 12/14/2019 10:10:28'!
PushDownMethod class removeSelector: #signalMethodCannotShadowInstVarOfSubclasses!
!methodRemoval: PushDownMethod class #warningMesssageForMessageAlreadyImplementedIn stamp: 'HAW 12/14/2019 10:23:05'!
PushDownMethod class removeSelector: #warningMesssageForMessageAlreadyImplementedIn!
!methodRemoval: PushDownMethod class #errorMessageCanNotPushDownWithShadowedInstVarsOf stamp: 'HAW 12/14/2019 10:38:53'!
PushDownMethod class removeSelector: #errorMessageCanNotPushDownWithShadowedInstVarsOf!
!methodRemoval: PushDownMethod class #warnMethodAlreadyImplementedIn: stamp: 'HAW 12/14/2019 10:58:43'!
PushDownMethod class removeSelector: #warnMethodAlreadyImplementedIn:!
!methodRemoval: PushDownMethod class #errorMessageForTempOrArgVarDeclaredAsInstVarOnSubClass stamp: 'HAW 12/14/2019 10:38:25'!
PushDownMethod class removeSelector: #errorMessageForTempOrArgVarDeclaredAsInstVarOnSubClass!
!methodRemoval: PushDownMethod class #signalMethodCannotShadowAnInstVarOfASubClass stamp: 'HAW 12/14/2019 10:09:53'!
PushDownMethod class removeSelector: #signalMethodCannotShadowAnInstVarOfASubClass!
!methodRemoval: PushDownMethod class #warnIsValidToPushDownMethod: stamp: 'HAW 12/14/2019 10:04:54'!
PushDownMethod class removeSelector: #warnIsValidToPushDownMethod:!
!methodRemoval: PushDownMethod #initializeFor: stamp: 'HAW 12/14/2019 11:04:18'!
PushDownMethod removeSelector: #initializeFor:!
!methodRemoval: PushDownMethod #apply stamp: 'HAW 12/14/2019 11:10:48'!
PushDownMethod removeSelector: #apply!
!methodRemoval: MoveToInstanceOrClassMethod #initializeFor: stamp: 'HAW 12/14/2019 11:08:20'!
MoveToInstanceOrClassMethod removeSelector: #initializeFor:!
!methodRemoval: MoveToInstanceOrClassMethod #apply stamp: 'HAW 12/14/2019 11:08:14'!
MoveToInstanceOrClassMethod removeSelector: #apply!

!MoveMethod reorganize!
('initialization' initializeFor:)
('applying - private' moveMethod removeMethod)
('applying' apply methodCategory)
!

!classDefinition: #MoveToInstanceOrClassMethod category: #'Tools-Refactoring'!
MoveMethod subclass: #MoveToInstanceOrClassMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!MoveToInstanceOrClassMethod reorganize!
('applying - private' moveMethod)
!

!classDefinition: #PushDownMethod category: #'Tools-Refactoring'!
MoveMethod subclass: #PushDownMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!PushDownMethod reorganize!
('applying - private' moveMethod)
!


!PushDownMethod class reorganize!
('instance creation' for:)
('pre-conditions' addShadowedInstVarsOf:in:to: assertIsValidToPushDownMethod: warnIfExistMethodToPushDownOnSubClass:)
('exceptions' errorMessageCanNotPushDownWithShadowedInstVarsOf: signalMethodCannotShadowInstVarOfSubclasses:)
('warnings' warnMessageAlreadyImplementedIn: warningMesssageForMessageAlreadyImplementedIn:)
!

!classDefinition: #PushUpMethod category: #'Tools-Refactoring'!
MoveMethod subclass: #PushUpMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!PushUpMethod reorganize!
('applying - private' moveMethod)
!


!PushUpMethod class reorganize!
('instance creation' for:)
('pre-conditions' assertIsNotAccessingInstanceVariable: assertIsValidToPushUpMethod:)
('warnings' methodToPushUpExistOnSuperclassWarningMessage warnIfExistMethodToPushUpOnSuperClass:)
('exceptions' errorMessageForMethodCannotAccessInstanceVariable signalMethodCannotAccessInstanceVariable)
!

!classDefinition: #MoveToInstanceOrClassMethodApplier category: #'Tools-Refactoring'!
MoveMethodApplier subclass: #MoveToInstanceOrClassMethodApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!MoveToInstanceOrClassMethodApplier reorganize!
('refactoring - creation' refactoringClass)
('refactoring - parameters request' confirmationMessageText methodHasSenders requestRefactoringParameters)
!


!MoveToInstanceOrClassMethodApplier class reorganize!
('as yet unclassified')
!


!PushDownMethodApplier reorganize!
('refactoring - creation' refactoringClass)
!


!PushDownMethodApplier class reorganize!
('as yet unclassified')
!

!classDefinition: #PushUpMethodApplier category: #'Tools-Refactoring'!
MoveMethodApplier subclass: #PushUpMethodApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!PushUpMethodApplier reorganize!
('refactoring - creation' refactoringClass)
!


!PushUpMethodApplier class reorganize!
('as yet unclassified')
!
