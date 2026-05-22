Class {
	#name : 'ExtractMethodApplierTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'object factory' }
ExtractMethodApplierTest >> argumentNamed: anArgumentName [

	"The instance is not complete, but it is enoght for testing - Hernan"
	^TempVariableNode new name: anArgumentName
		
]

{ #category : 'assertions' }
ExtractMethodApplierTest >> assertSignals: anErrorMessage with: userAnswer argumentNames: argumentNames   [

	| argumentNodes |
	
	argumentNodes := argumentNames collect: [ :argumetName | self argumentNamed: argumetName ].
	
	self
		should: [ ExtractMethodApplier new saveBinaryOrKeywordSelector: userAnswer withArguments: argumentNodes ]
		raise: RefactoringError 
		withMessageText: anErrorMessage 
]

{ #category : 'tests' }
ExtractMethodApplierTest >> test01InvalidMessageNameWhenKeywordWithoutParamenter [

	self assertSignals: ExtractMethodNewMethod invalidMethodNameError with: 'm1:' argumentNames: {'p1'} 
]

{ #category : 'tests' }
ExtractMethodApplierTest >> test02InvalidMessageNameWhenMultipleParamentersBetweenKeywords [

	self assertSignals: ExtractMethodNewMethod invalidMethodNameError with: 'm1: a b m2: c' argumentNames: {'p1'. 'p2'}.
	
]

{ #category : 'tests' }
ExtractMethodApplierTest >> test03WrongNumberOfArgumentsWhenParsedArgumensAreNotTheSameAsUserEnteredArguments [

	self assertSignals: ExtractMethodNewMethod wrongNumberOfArgumentsGivenErrorMessage with: 'm1: a' argumentNames: {}.
	
]
