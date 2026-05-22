"
I am a refactoring, a code transformation preserving behavior, based on some input (provided from the end user through a RefactoringApplier; or provided programmatically). Instances of me have usually only public method, #apply, which does all the work.

In case the refactoring cannot be made, or there is a problem during the application of it, I can throw errors using the class message #refactoringError:, or warnings using the class message #refactoringWarning:
"
Class {
	#name : 'Refactoring',
	#superclass : 'Object',
	#classVars : [
		'AllowedToRefactorClassCompilers'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'allowed to refactor class compilers' }
Refactoring class >> addAllowedToRefactorClassCompiler: aCompiler [

	self allowedToRefactorClassCompilers add: aCompiler 
]

{ #category : 'allowed to refactor class compilers' }
Refactoring class >> allowedToRefactorClassCompilers [

	AllowedToRefactorClassCompilers ifNil: [
		AllowedToRefactorClassCompilers := Set with: Compiler ].

	^AllowedToRefactorClassCompilers
]

{ #category : 'allowed to refactor class compilers' }
Refactoring class >> allowedToRefactorClassCompilersIncludes: aCompiler [

	^self allowedToRefactorClassCompilers includes: aCompiler 
]

{ #category : 'exceptions' }
Refactoring class >> canNotRefactorDueToReferencesError: aMessageText references: references to: referencee  [
	
	^self canNotRefactorDueToReferencesErrorClass
		signal: aMessageText 
		references: references 
		to: referencee 
]

{ #category : 'exceptions' }
Refactoring class >> canNotRefactorDueToReferencesErrorClass [
	
	^CanNotRefactorDueToReferencesError
]

{ #category : 'testing' }
Refactoring class >> canRefactor: aPotentialClassToRefactor [

	^self allowedToRefactorClassCompilersIncludes: aPotentialClassToRefactor compilerClass
]

{ #category : 'exceptions' }
Refactoring class >> refactoringError: aMessage [

	self refactoringErrorClass signal: aMessage.
]

{ #category : 'exceptions' }
Refactoring class >> refactoringErrorClass [

	^ RefactoringError.
]

{ #category : 'warnings' }
Refactoring class >> refactoringWarning: aMessageText [

	^ self refactoringWarningClass signal: aMessageText.
]

{ #category : 'warnings' }
Refactoring class >> refactoringWarningClass [

	^ RefactoringWarning.
]

{ #category : 'warnings' }
Refactoring class >> referencesWarningClass [
	
	^ReferencesRefactoringWarning
]

{ #category : 'warnings' }
Refactoring class >> warnAboutReferences: references with: aMessageText of: primaryReferencee toAll: allReferenced [
 
	^self referencesWarningClass
		signal: aMessageText 
		references: references 
		of: primaryReferencee 
		toAll: allReferenced 
]

{ #category : 'applying' }
Refactoring >> apply [

	self subclassResponsibility 
]
