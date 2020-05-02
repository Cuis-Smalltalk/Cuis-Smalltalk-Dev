'From Cuis 5.0 [latest update: #4109] on 2 May 2020 at 12:13:20 am'!
!classDefinition: #Refactoring category: #'Tools-Refactoring'!
Object subclass: #Refactoring
	instanceVariableNames: ''
	classVariableNames: 'AllowedToRefactorClassCompilers '
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!Refactoring class methodsFor: 'testing' stamp: 'HAW 5/1/2020 23:58:00'!
canRefactor: aPotentialClassToRefactor

	^self allowedToRefactorClassCompilersIncludes: aPotentialClassToRefactor compilerClass! !

!Refactoring class methodsFor: 'allowed to refactor class compilers' stamp: 'HAW 5/2/2020 00:03:05'!
addAllowedToRefactorClassCompiler: aCompiler

	self allowedToRefactorClassCompilers add: aCompiler ! !

!Refactoring class methodsFor: 'allowed to refactor class compilers' stamp: 'HAW 5/1/2020 23:59:39'!
allowedToRefactorClassCompilers

	AllowedToRefactorClassCompilers ifNil: [
		AllowedToRefactorClassCompilers := Set with: Compiler ].

	^AllowedToRefactorClassCompilers! !

!Refactoring class methodsFor: 'allowed to refactor class compilers' stamp: 'HAW 5/1/2020 23:58:58'!
allowedToRefactorClassCompilersIncludes: aCompiler

	^self allowedToRefactorClassCompilers includes: aCompiler ! !


!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 5/1/2020 23:55:29'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor

	| potentialImplementor |

	"Phil B. requested to avoid refactoring OMeta2 classes, so right now
	it avoids implementors and senders whose compilerClass is not register
	as allowed compiler - Hernan"
	(self canRefactor: aPotentialClassToRefactor) ifFalse: [ ^self ].

	potentialImplementor := aPotentialClassToRefactor compiledMethodAt: anOldSelector ifAbsent: [ nil ].
	potentialImplementor ifNotNil: [ implementors add: potentialImplementor ].

	(aPotentialClassToRefactor whichSelectorsReferTo: anOldSelector) do: [ :aSelector |
		senders add: (MethodReference class: aPotentialClassToRefactor selector: aSelector) ]! !

!classDefinition: #Refactoring category: #'Tools-Refactoring'!
Object subclass: #Refactoring
	instanceVariableNames: ''
	classVariableNames: 'AllowedToRefactorClassCompilers'
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
