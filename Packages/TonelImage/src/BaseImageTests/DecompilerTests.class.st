"
Apparently the decompiler does not really work totally.
Here are a bunch of methods that can help improving the decompiler:
	- blockingClasses return class for which it is impossible to decompile methods 
	- failures are problems that lead to a DNU
	- decompilerDiscrepancies are the results of running decompileTestHelper..as you see the pattern 	
	is quite present.
"
Class {
	#name : 'DecompilerTests',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'utilities' }
DecompilerTests >> blockingClasses [


	^ #(CompiledMethod)
]

{ #category : 'utilities' }
DecompilerTests >> checkDecompileMethod: oldMethod [
	
	| cls selector oldMethodNode methodNode newMethod oldCodeString newCodeString |
	cls _ oldMethod methodClass.
	selector := oldMethod selector.
	oldMethodNode _ Decompiler new
						decompile: selector
						in: cls
						method: oldMethod.
	[oldMethodNode properties includesKey: #warning]
		whileTrue: [oldMethodNode properties removeKey: #warning].
	oldCodeString _ oldMethodNode decompileString.
	methodNode _ [ Compiler new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil]
						on: SyntaxErrorNotification
						do: [ :ex |
							ex errorMessage = 'Cannot store into'
								ifTrue: [ex return: #badStore].
							ex pass ].
	"Ignore cannot store into block arg errors; they're not our issue."
	methodNode ~~ #badStore ifTrue: [
		newMethod _ methodNode generate: #(0 0 0 0).
		newCodeString := (Decompiler new
							decompile: selector
							in: cls
							method: newMethod) decompileString.
		"Decompiler might move temp declarations (keeping valid, equivalent code) and therefore affect the temp numbering.
		Make temp numbering canonical in decompiler code, to avoid bogus failures"
		oldCodeString _ self withFixedTempNumbering: oldCodeString.
		newCodeString _ self withFixedTempNumbering: newCodeString.
		"(DifferenceFinder displayPatchFrom: oldCodeString to: newCodeString tryWords: true)
			editLabel: 'Decompilation Differences for ', cls name,'>>',selector."
		"(DifferenceFinder displayPatchFrom: oldMethod abstractSymbolic to: newMethod abstractSymbolic tryWords: true)
			editLabel: 'Bytecode Differences for ', cls name,'>>',selector."
		self assert: oldCodeString = newCodeString
			description: cls name asString, ' ', selector asString
			resumable: true ]
]

{ #category : 'utilities' }
DecompilerTests >> decompileClassesInRootCategoryOf: aClass [

	| rootCategory |
	
	rootCategory := SystemOrganization rootCategoryOf: aClass category.
	(SystemOrganization allClassesAt: rootCategory) do: [ :class |
		class selectorsAndMethodsDo: [ :selector :method |
			(class organization categoryOfElement: selector) ifNotNil: [:category |
				(category beginsWith: '*') ifFalse: [
					(self exceptionClassForFailureFor: class selector: selector)
						ifNil: [ self checkDecompileMethod: method ]
						ifNotNil: [ :exceptionClass |
							self
								should: [ self checkDecompileMethod: method ]
								raise: exceptionClass ] ] ] ] ]
]

{ #category : 'filtering errors' }
DecompilerTests >> decompilerFailures [
	"Here is the list of failures: either a syntax error, a hard error or some failure to decompile correctly.
	 Collected initially via 
		DecompilerTestFailuresCollector new computeFailures.
	 But can be maintained manually."

	"class name, selector, error class name or nil"
	^#(
		(Dictionary scanFor: TestFailure)
		(IdentityDictionary scanFor: TestFailure)
		(IdentitySet scanFor: TestFailure)
		(Integer lowBit TestFailure)
		(MethodDictionary scanFor: TestFailure)
		(Set scanFor: TestFailure)
		(WeakIdentityKeyDictionary scanFor: TestFailure)
		(WeakKeyDictionary scanFor: TestFailure))
]

{ #category : 'utilities' }
DecompilerTests >> exceptionClassForFailureFor: class selector: selector [
	
	^self decompilerFailures
		detect: [ :each | each first = class name and: [ each second = selector ] ]
		ifFound: [ :each | each third ifNotNil: [ Smalltalk classNamed: each third ] ]
		ifNone: [ nil ]
]

{ #category : 'tests' }
DecompilerTests >> testDecompilerBrowserClasses [

	self decompileClassesInRootCategoryOf: Browser 
]

{ #category : 'tests' }
DecompilerTests >> testDecompilerCollectionClasses [

	self decompileClassesInRootCategoryOf: Collection
]

{ #category : 'tests' }
DecompilerTests >> testDecompilerCompilerClasses [

	self decompileClassesInRootCategoryOf: Compiler 
]

{ #category : 'tests' }
DecompilerTests >> testDecompilerExceptionsClasses [

	self decompileClassesInRootCategoryOf: Exception 
]

{ #category : 'tests' }
DecompilerTests >> testDecompilerGraphicsClasses [

	self decompileClassesInRootCategoryOf: Form 
]

{ #category : 'tests' }
DecompilerTests >> testDecompilerKernelClasses [

	self decompileClassesInRootCategoryOf: Object 
]

{ #category : 'tests' }
DecompilerTests >> testDecompilerMorphicClasses [

	self decompileClassesInRootCategoryOf: Morph 
]

{ #category : 'tests' }
DecompilerTests >> testDecompilerSystemClasses [

	self decompileClassesInRootCategoryOf: Editor 
]

{ #category : 'tests' }
DecompilerTests >> testRemoteTemp [
	| aBlock |
	aBlock := Compiler evaluate: '| x y |  [:a :b | x := a. y := b. x+y]'.
	self shouldnt: [aBlock decompile] raise: Error
	
]

{ #category : 'utilities' }
DecompilerTests >> withFixedTempNumbering: aString [
	| k code newCode tempStart startAt |
	code _ aString.
	`{'argm'. 'temp'}` with: `{'arg'. 'tmp'}` do: [ :a :b |
		k _ 1.
		startAt _ 1.
		[tempStart _ code findString: a startingAt: startAt. tempStart > 0] whileTrue: [ | end |
			end _ tempStart. [end <= code size and: [(code at: end) tokenish]] whileTrue: [end _ end + 1]. end _ end-1.
			newCode _ code  copyReplaceTokens: (code copyFrom: tempStart to: end) with: b, k printString.
			code = newCode ifTrue: [startAt _ tempStart + 1].
			code _ newCode.
			k _ k + 1 ]].
	^code
]
