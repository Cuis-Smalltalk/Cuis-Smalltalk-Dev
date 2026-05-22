"
I am a refactoring that extracts a selected piece of code to a separate method. The input is the following:

* interval of code to extract (from index - to index)
* the CompiledMethod where this change applies
* the new method selector + argument names (instance of Message)
* the category name for the new method

Many conditions have to be satisfied for this refactoring to be made, I delegate into SourceCodeOfMethodToBeExtractedPrecondition and NewSelectorPrecondition most of these checks. Refer to those classes' comments for more information.
"
Class {
	#name : 'ExtractMethodNewMethod',
	#superclass : 'Refactoring',
	#instVars : [
		'categoryOfNewSelector',
		'existingMethod',
		'newSourceCode'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'private - pre-conditions' }
ExtractMethodNewMethod class >> assert: aSelector canBeDefinedIn: aClass [

	NewSelectorPrecondition valueFor: aSelector on: aClass
]

{ #category : 'private - pre-conditions' }
ExtractMethodNewMethod class >> assert: aMethodToRefactor containsValidCodeToBeExtractedAt: anIntervalToExtract [

	SourceCodeOfMethodToBeExtractedPrecondition valueFor: anIntervalToExtract of: aMethodToRefactor
]

{ #category : 'private - pre-conditions' }
ExtractMethodNewMethod class >> assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodNodeToRefactor [

	| parseNodesToParameterize |
	
	parseNodesToParameterize := ExtractMethodParametersDetector
		valueFor: aMethodNodeToRefactor
		at: anIntervalToExtract.
	
	(newMessage hasRightNumberOfArguments and: [ newMessage arguments size = parseNodesToParameterize size ])
		ifFalse: [ self signalExtractMethodWithWrongNumberOfArgumentsError ]
]

{ #category : 'private - pre-conditions' }
ExtractMethodNewMethod class >> assert: anIntervalToExtract isWithinBoundsOf: sourceCode [

	(self is: anIntervalToExtract withinBoundsOf: sourceCode)
		ifFalse: [ self signalOutOfBoundsIntervalError ]
]

{ #category : 'pre-conditions' }
ExtractMethodNewMethod class >> assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: anIntervalToExtract [

	self
		assertIntervalToExtractIsNotEmpty: anIntervalToExtract;
		assert: anIntervalToExtract isWithinBoundsOf: aMethodToExtractCodeFrom sourceCode;
		assert: aMethodToExtractCodeFrom containsValidCodeToBeExtractedAt: anIntervalToExtract
]

{ #category : 'private - pre-conditions' }
ExtractMethodNewMethod class >> assertIntervalToExtractIsNotEmpty: anIntervalToExtract [

	(self isNotEmpty: anIntervalToExtract)
		ifFalse: [ self signalNoSelectedCodeError ]
]

{ #category : 'instance creation' }
ExtractMethodNewMethod class >> fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments [

	| trimmedIntervalToExtract newSourceCode |

	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract;
		assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodToExtractCodeFrom notOptimizedMethodNode.

	"With the renamed arguments functionality, I want to signal errors at creation time, not at applying time.
	So I need to cheeck that renaming arguments is valid, and to do that I need to rename them all, that is why
	now the new source code is created at instantiation time and not applying time - Hernan."
	newSourceCode := (ExtractMethodNewMethodSourceCode
		fromInterval: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		withArgumentNames: renamedArguments) value.

	^ self new
		initializeOf: aMethodToExtractCodeFrom
		in: aCategory
		newSourceCode: newSourceCode
]

{ #category : 'error messages' }
ExtractMethodNewMethod class >> invalidMethodNameError [

	^ 'Invalid method name'
]

{ #category : 'private - pre-conditions' }
ExtractMethodNewMethod class >> is: anIntervalToExtract withinBoundsOf: aSourceCode [

	^ anIntervalToExtract first >= 1 and: [ anIntervalToExtract last <= aSourceCode size ]
]

{ #category : 'private - pre-conditions' }
ExtractMethodNewMethod class >> isNotEmpty: anInterval  [

	^ anInterval first <= anInterval last
]

{ #category : 'error messages' }
ExtractMethodNewMethod class >> noSelectionErrorMessage [

	^ 'Please select some code for extraction'
]

{ #category : 'error messages' }
ExtractMethodNewMethod class >> outOfBoundsSelectionErrorMessage [

	^ 'The requested source code selection interval is out of bounds'
]

{ #category : 'exceptions' }
ExtractMethodNewMethod class >> signalExtractMethodWithWrongNumberOfArgumentsError [

	self refactoringError: self wrongNumberOfArgumentsGivenErrorMessage
]

{ #category : 'exceptions' }
ExtractMethodNewMethod class >> signalInvalidMethodNameError [

	self refactoringError: self invalidMethodNameError
]

{ #category : 'exceptions' }
ExtractMethodNewMethod class >> signalNoSelectedCodeError [

	self refactoringError: self noSelectionErrorMessage
]

{ #category : 'exceptions' }
ExtractMethodNewMethod class >> signalOutOfBoundsIntervalError [

	self refactoringError: self outOfBoundsSelectionErrorMessage
]

{ #category : 'error messages' }
ExtractMethodNewMethod class >> wrongNumberOfArgumentsGivenErrorMessage [

	^ 'The number of arguments in the given selector is not correct'
]

{ #category : 'applying' }
ExtractMethodNewMethod >> apply [

	existingMethod methodClass
		compile: newSourceCode 
		classified: categoryOfNewSelector.
	
	
]

{ #category : 'initialization' }
ExtractMethodNewMethod >> initializeOf: aMethodToExtractCodeFrom in: aCategory newSourceCode: aNewSourceCode    [

	existingMethod := aMethodToExtractCodeFrom.
	categoryOfNewSelector := aCategory.
	newSourceCode := aNewSourceCode 
]
