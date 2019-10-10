'From Cuis 5.0 [latest update: #3846] on 9 October 2019 at 10:19:37 am'!
!classDefinition: #RenameInstanceVariable category: #'Tools-Refactoring'!
Refactoring subclass: #RenameInstanceVariable
	instanceVariableNames: 'oldVariable addInstanceVariable rewriter renamedReferences newVariable methodsAndRangesToChange classToRefactor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!CodeProvider methodsFor: 'contents' stamp: 'HAW 10/8/2019 16:39:33'!
instanceVariableRenamed

	self acceptedContentsChanged
! !


!Debugger methodsFor: 'accessing' stamp: 'HAW 10/8/2019 16:52:56'!
resetToSelectedContextWith: newMethod

	| ctxt |

	ctxt := interruptedProcess popTo: self selectedContext.
	ctxt == self selectedContext
		ifFalse:
			[self inform: 'Method saved, but current context unchanged\because of unwind error. Click OK to see error' withNewLines]
		ifTrue:
			[newMethod isQuick ifFalse:
				[interruptedProcess
					restartTopWith: newMethod;
				 	stepToSendOrReturn].
			contextVariablesInspector object: nil].
	self resetContext: ctxt.
	! !

!Debugger methodsFor: 'contents' stamp: 'HAW 10/8/2019 16:54:07'!
instanceVariableRenamed

	| newMethod |
	
	newMethod := self selectedClass compiledMethodAt: self selectedContext selector.
	self resetToSelectedContextWith: newMethod ! !


!Categorizer class methodsFor: 'well known categories' stamp: 'HAW 10/8/2019 14:45:15'!
initialization
	
	^'initialization'! !

!Categorizer class methodsFor: 'well known categories' stamp: 'HAW 10/8/2019 17:18:38'!
instanceCreation

	^'instance creation'! !


!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'HAW 10/9/2019 10:06:00'!
isAtClassNameInClassDefinition: anIndex

	^(classDefinitionNode rangeForNode: classCreationMessageNode arguments first ifAbsent: [ ^ false ]) first includes: anIndex! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'HAW 10/9/2019 10:04:36'!
isAtSuperclassInClassDefinition: anIndex

	^(classDefinitionNode rangeForNode: superClassNode ifAbsent: [ ^ false ]) first includes: anIndex ! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'HAW 10/9/2019 10:14:44'!
isClassDefinition
	
	^classDefinitionNode encoder classEncoding isMeta not! !

!ClassDefinitionNodeAnalyzer methodsFor: 'parameters positions' stamp: 'HAW 10/9/2019 10:18:15'!
instanceVariableNamesPosition
	
	^self isClassDefinition ifTrue: [ self class instanceVariableNamesPositionForClassDefinition ] ifFalse: [ self class instanceVariableNamesPositionForMetaclassDefinition ]! !


!ClassDefinitionNodeAnalyzer class methodsFor: 'parameters positions' stamp: 'HAW 10/9/2019 10:18:41'!
instanceVariableNamesPositionForClassDefinition

	^2! !

!ClassDefinitionNodeAnalyzer class methodsFor: 'parameters positions' stamp: 'HAW 10/9/2019 10:18:46'!
instanceVariableNamesPositionForMetaclassDefinition

	^1! !


!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 10/9/2019 09:16:21'!
changeInstanceVariableName

	| instanceVariableNames oldVariableIndex |
	
	instanceVariableNames := classToRefactor instVarNames.
	oldVariableIndex := instanceVariableNames indexOf: oldVariable.
	instanceVariableNames at: oldVariableIndex put: newVariable.! !

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 10/9/2019 09:07:24'!
keepMethodToChangeNamed: aSelector in: aClass 

	| methodToChange rangesToChange |
	
	methodToChange := aClass >> aSelector.
	rangesToChange :=  methodToChange methodNode positionsForInstanceVariable: oldVariable ifAbsent: [ #() ].

	methodsAndRangesToChange add: methodToChange -> rangesToChange ! !

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 10/9/2019 09:16:15'!
lookForMethodsReferencingOldVariable
	
	methodsAndRangesToChange := OrderedCollection new.
	classToRefactor withAllSubclassesDo: [ :aClass |  self lookForMethodsReferencingOldVariableIn: aClass ].
! !

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 10/9/2019 09:06:51'!
lookForMethodsReferencingOldVariableIn: aClass

	(aClass whichSelectorsAccess: oldVariable) do: [ :aSelector | self keepMethodToChangeNamed: aSelector in: aClass ].
	! !

!RenameInstanceVariable methodsFor: 'initialization' stamp: 'HAW 10/9/2019 09:14:17'!
initializeFrom: anOldvariable to: aNewVariable in: aClassToRefactor 

	oldVariable := anOldvariable.
	newVariable := aNewVariable.
	classToRefactor := aClassToRefactor ! !


!Debugger methodsFor: 'accessing' stamp: 'HAW 10/8/2019 16:53:16'!
contents: aText notifying: aController
	"The retrieved information has changed and its source must now be updated.
	 In this case, the retrieved information is the method of the selected context."

	| result selector classOfMethod category h newMethod |

	contextStackIndex = 0 ifTrue: [^false].

	classOfMethod := self selectedClass.
	category := self selectedMessageCategoryName.
	selector :=self selectedClass parserClass selectorFrom: aText.

	selector ~= self selectedMessageName ifTrue: [
		self inform: 'Can not change the selector in the debugger'.
		^false].	
 	(classOfMethod = UndefinedObject and: [ selector = Scanner doItSelector or: [ selector = Scanner doItInSelector ]]) ifTrue: [
		self inform: 'DoIt and DoItIn: methods can not be changed'.
	 	^false].

	self selectedContext isExecutingBlock ifTrue: [
		h := self selectedContext activeHome.
		h ifNil: [
			self inform: 'Method for block not found on stack, can''t edit and continue'.
			^false].
		(self confirm: 'I will have to revert to the method from\which this block originated.  Is that OK?' withNewLines) ifFalse: [^false].
		self resetContext: h.
		(result := self contents: aText notifying: aController) ifTrue: [self acceptedContentsChanged].
		^result].
		
	selector := classOfMethod
				compile: aText
				classified: category
				notifying: aController.
	selector ifNil: [^false]. "compile cancelled"
	newMethod := classOfMethod compiledMethodAt: selector.

	newMethod isQuick ifTrue: [
		contextStackIndex + 1 > contextStack size ifTrue: [
			self inform: 'Can not compile a quick method in the stack base context'.
			^false].
		self down.
		self selectedContext jump: (self selectedContext previousPc - self selectedContext pc)].

	self resetToSelectedContextWith: newMethod.
	
	^true! !

!Debugger methodsFor: 'context stack menu' stamp: 'HAW 10/8/2019 17:18:14'!
askForCategoryIn: aClass default: aDefaultCategory

	| categories index category classCategories |
	
	categories := OrderedCollection with: 'new ...'. 
	
	aClass isMeta ifTrue: [ categories add: Categorizer instanceCreation ].
	classCategories := aClass allMethodCategoriesIntegratedThrough: Object.
	aClass isMeta ifTrue: [ classCategories remove: Categorizer instanceCreation ifAbsent: []].
	
	categories addAll: classCategories.	
	index := PopUpMenu 
		withCaption: 'Please provide a good category for the new method!!'
		chooseFrom: categories.
	index = 0 ifTrue: [^ aDefaultCategory].
	category := index = 1 
		ifTrue: [FillInTheBlankMorph request: 'Enter category name:']
		ifFalse: [categories at: index].
		
	^ category isEmpty ifTrue: [aDefaultCategory] ifFalse: [category]! !

!Debugger methodsFor: 'method creation' stamp: 'HAW 10/8/2019 17:15:10'!
implement: aMessage inClass: aClass context: aContext 
	
	self implement: aMessage inCategory: (self askForCategoryIn: aClass default: Categorizer default) fromClass: aClass context: aContext! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'HAW 10/8/2019 17:21:29'!
allMethodCategoriesIntegratedThrough: mostGenericClass
	"Answer a list of all the method categories of the receiver and all its superclasses, up through mostGenericClass"

	| aColl |
	aColl _ OrderedCollection new.
	self withAllSuperclasses do: [ :aClass |
		(aClass includesBehavior: mostGenericClass)
			ifTrue: [ aColl addAll: aClass organization categories ]].
	aColl remove: 'no messages' asSymbol ifAbsent: nil.

	^ aColl asSet asSortedCollection: [ :a :b | a asLowercase < b asLowercase ]

"ColorTileMorph allMethodCategoriesIntegratedThrough: TileMorph"! !


!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'HAW 10/9/2019 10:14:50'!
isAtCategory: anIndex

	^self isClassDefinition and: [ self is: anIndex atStringParameterNumber: self class categoryPosition ]
	! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'HAW 10/9/2019 10:14:57'!
isAtClassName: anIndex

	^self isClassDefinition and: [ self isAtClassNameInClassDefinition: anIndex ]
	! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'HAW 10/9/2019 10:09:14'!
isAtInstanceVariables: anIndex

	^self is: anIndex atStringParameterNumber: self instanceVariableNamesPosition! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'HAW 10/9/2019 10:15:11'!
isAtSuperclass: anIndex

	^self isClassDefinition and: [ self isAtSuperclassInClassDefinition: anIndex ]! !


!AutoCompleterSelectorsCollector methodsFor: 'entries to show - private' stamp: 'HAW 10/8/2019 17:19:27'!
prioritizedCategories 
	
	^{Categorizer instanceCreation}! !


!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 10/9/2019 09:14:52'!
newSourceOf: aMethodAndRangesToChange 
	
	| newSource ranges methodToChange |
	
	methodToChange := aMethodAndRangesToChange key.
	ranges := aMethodAndRangesToChange value.
	newSource := methodToChange sourceCode copyReplacing: ranges with: newVariable.
	
	^newSource
	! !

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 10/9/2019 09:09:41'!
renameReferencesToOldVariable
	
	renamedReferences := OrderedCollection new.
	methodsAndRangesToChange do: [ :aMethodAndRangesToChange | self renameReferencesToOldVariableInMethod: aMethodAndRangesToChange ].
	! !

!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 10/9/2019 09:01:03'!
renameReferencesToOldVariableInMethod: aMethodAndRangesToChange 
	
	| methodToChange |
	
	methodToChange := aMethodAndRangesToChange key.
	methodToChange methodClass compile: (self newSourceOf: aMethodAndRangesToChange).
	renamedReferences add: methodToChange methodReference ! !

!RenameInstanceVariable methodsFor: 'applying' stamp: 'HAW 10/9/2019 09:08:49'!
apply

	self 
		lookForMethodsReferencingOldVariable;
		changeInstanceVariableName;
		renameReferencesToOldVariable.
		
	^renamedReferences 
		! !


!RenameInstanceVariable class methodsFor: 'instance creation' stamp: 'HAW 10/9/2019 09:13:37'!
from: anOldvariable to: aNewVariable in: aClassToRefactor 
	
	| trimmedNewVariable |
	
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	self assert: aClassToRefactor defines: anOldvariable.	
	NewInstanceVariablePrecondition valueOf: trimmedNewVariable for: aClassToRefactor.	
	
	^self new initializeFrom: anOldvariable to: trimmedNewVariable in: aClassToRefactor ! !


!RenameInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 10/9/2019 09:21:29'!
requestRefactoringParameters

	self 
		chooseInstanceVariable;
		askNewVariableName! !

!RenameInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 10/8/2019 16:55:08'!
informChangesToBrowser
	
	browser instanceVariableRenamed ! !

!RenameInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 10/8/2019 16:55:44'!
showChanges

	self informChangesToBrowser! !


!RenameInstanceVariableApplier class methodsFor: 'instance creation' stamp: 'HAW 10/8/2019 17:02:16'!
on: aBrowser for: anInstanceVariableName at:  aClassToRefactor 
	
	^self new initializeOn: aBrowser for: anInstanceVariableName at: aClassToRefactor ! !

!methodRemoval: RenameInstanceVariableApplier #applyRefactoring!
RenameInstanceVariableApplier removeSelector: #applyRefactoring!
!methodRemoval: RenameInstanceVariableApplier #isOnDebugger!
RenameInstanceVariableApplier removeSelector: #isOnDebugger!
!methodRemoval: RenameInstanceVariableApplier #openChangedMethods!
RenameInstanceVariableApplier removeSelector: #openChangedMethods!
!methodRemoval: RenameInstanceVariableApplier #openChangedMethodsWhenChangesNotEmpty!
RenameInstanceVariableApplier removeSelector: #openChangedMethodsWhenChangesNotEmpty!
!methodRemoval: RenameInstanceVariableApplier #value!
RenameInstanceVariableApplier removeSelector: #value!
!methodRemoval: RenameInstanceVariableApplier #warnIfOnDebugger!
RenameInstanceVariableApplier removeSelector: #warnIfOnDebugger!
!methodRemoval: RenameInstanceVariable #addNewInstanceVariable!
RenameInstanceVariable removeSelector: #addNewInstanceVariable!
!methodRemoval: RenameInstanceVariable #classToRefactor!
RenameInstanceVariable removeSelector: #classToRefactor!
!methodRemoval: RenameInstanceVariable #copyOldInstanceVariableToNewOne!
RenameInstanceVariable removeSelector: #copyOldInstanceVariableToNewOne!
!methodRemoval: RenameInstanceVariable #initializeFrom:addingWith:!
RenameInstanceVariable removeSelector: #initializeFrom:addingWith:!
!methodRemoval: RenameInstanceVariable #newVariable!
RenameInstanceVariable removeSelector: #newVariable!
!methodRemoval: RenameInstanceVariable #removeOldInstanceVariable!
RenameInstanceVariable removeSelector: #removeOldInstanceVariable!
!methodRemoval: RenameInstanceVariable #renameReferencesToOldVariableInClass:!
RenameInstanceVariable removeSelector: #renameReferencesToOldVariableInClass:!
!methodRemoval: ClassDefinitionNodeAnalyzer class #instanceVariableNamesPosition!
ClassDefinitionNodeAnalyzer class removeSelector: #instanceVariableNamesPosition!
!methodRemoval: ClassDefinitionNodeAnalyzer #isMetaclassDefinition!
ClassDefinitionNodeAnalyzer removeSelector: #isMetaclassDefinition!

!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context: resetToSelectedContextWith:)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack handleLabelUpdatesIn:whenExecuting: peelToFirst proceed restart returnValue: selectPC send stepIntoBlock up where)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('class list' selectedClass)
('dependents access' updateInspectors wantsSteps)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: fixReceiverInspector lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
('breakpoints' toggleBreakOnEntry)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:withCaption:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUnderstand implement:inCategory:fromClass:context: implement:inClass:context: overrideMethodOnSubclass shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnOverridableMethod wasInterruptedOnSubclassResponsibility)
('testing' is: isEditingMethod)
('auto complete' classOfThisContext computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('contents' instanceVariableRenamed)
!


!Categorizer class reorganize!
('class initialization' allCategory default initialize nullCategory)
('instance creation' defaultList:)
('documentation' documentation)
('well known categories' initialization instanceCreation)
!


!ClassDefinitionNodeAnalyzer reorganize!
('initialization' initializeFor:)
('testing' isAtCategory: isAtClassName: isAtClassNameInClassDefinition: isAtInstanceVariables: isAtSuperclass: isAtSuperclassInClassDefinition: isClassDefinition)
('accessing' superclass)
('testing - private' is:atStringParameterNumber:)
('parameters positions' instanceVariableNamesPosition)
!

!classDefinition: #RenameInstanceVariable category: #'Tools-Refactoring'!
Refactoring subclass: #RenameInstanceVariable
	instanceVariableNames: 'oldVariable renamedReferences methodsAndRangesToChange newVariable classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!RenameInstanceVariable reorganize!
('applying - private' changeInstanceVariableName keepMethodToChangeNamed:in: lookForMethodsReferencingOldVariable lookForMethodsReferencingOldVariableIn: newSourceOf: renameReferencesToOldVariable renameReferencesToOldVariableInMethod:)
('applying' apply)
('initialization' initializeFrom:to:in:)
!


!RenameInstanceVariableApplier reorganize!
('refactoring - parameters request' askNewVariableName chooseInstanceVariable requestRefactoringParameters selectVariableLabel)
('refactoring - creation' createRefactoring)
('refactoring - changes' informChangesToBrowser showChanges)
('initialization' initializeOn:for:at:)
!

