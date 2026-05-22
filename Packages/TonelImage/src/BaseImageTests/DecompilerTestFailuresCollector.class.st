"
(| dtfc |
dtfc := DecompilerTestFailuresCollector new.
(dtfc class superclass organization listAtCategoryNamed: #tests) do:
	[:s| dtfc perform: s].
dtfc failures asArray)

(Transcript nextPut: ${.
self do: [:mr| Transcript print: mr actualClass; nextPut: $.; space; store: mr methodSymbol; nextPut: $.; cr; flush].
Transcript nextPut: $}; flush)
"
Class {
	#name : 'DecompilerTestFailuresCollector',
	#superclass : 'DecompilerTests',
	#instVars : [
		'failures'
	],
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'as yet unclassified' }
DecompilerTestFailuresCollector class >> isAbstract [

	^ true
]

{ #category : 'accessing' }
DecompilerTestFailuresCollector >> assert: aBoolean description: aString resumable: resumableBoolean [

	| method |
	aBoolean ifTrue: [ ^self ].
	method := thisContext sender home tempAt: 1.
	self noteFailure: TestFailure for: method
]

{ #category : 'utilities' }
DecompilerTestFailuresCollector >> checkDecompileMethod: oldMethod [
	
	[ super checkDecompileMethod: oldMethod ]
		on: Error
		do: [ :error | self noteFailure: error class for: oldMethod ]
]

{ #category : 'accessing' }
DecompilerTestFailuresCollector >> computeFailures [
	
	| tests |
	failures := nil.
	tests := (DecompilerTests organization listAtCategoryNamed: #tests) select:
				[ :s | s beginsWith: 'testDecompilerInClasses' ].
	tests
		do: [ :each | self perform: each ]
		displayingProgress: 'Computing failures...'.
	^self decompilerFailures
]

{ #category : 'accessing' }
DecompilerTestFailuresCollector >> decompilerFailures [

	failures ifNil: [ ^#() ].
	^failures asArray sort: [ :a :b |
		| r |
		r := a first compare: b first caseSensitive: false.
		r = 1 or: [ r = 2 and: [
			(a second compare: b second caseSensitive: false) <= 2 ] ] ]
]

{ #category : 'utilities' }
DecompilerTestFailuresCollector >> exceptionClassForFailureFor: class  selector: selector [
	
	^nil
]

{ #category : 'accessing' }
DecompilerTestFailuresCollector >> failures [
	^failures
]

{ #category : 'utilities' }
DecompilerTestFailuresCollector >> noteFailure: exceptionClass for: method [

	(failures ifNil: [ failures := OrderedCollection new ]) add: {
		method methodClass name.
		method selector.
		exceptionClass name }
]
