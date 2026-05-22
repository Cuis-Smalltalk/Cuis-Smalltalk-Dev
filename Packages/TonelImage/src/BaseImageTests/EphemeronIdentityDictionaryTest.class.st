"
SUnit tests for weak identity key dictionaries
"
Class {
	#name : 'EphemeronIdentityDictionaryTest',
	#superclass : 'TestCase',
	#instVars : [
		'keys',
		'dict'
	],
	#category : 'BaseImageTests-System-Finalization',
	#package : 'BaseImageTests',
	#tag : 'System-Finalization'
}

{ #category : 'coverage' }
EphemeronIdentityDictionaryTest >> classToBeTested [

	^ EphemeronIdentityDictionary
]

{ #category : 'running' }
EphemeronIdentityDictionaryTest >> setUp [
	super setUp.
	keys := (1 to: 1000) collect:[:n| 'key', n asString].
	dict := EphemeronIdentityDictionary new.
	keys do:[:n| dict at: n put: n,n].
	super setUp
]

{ #category : 'tests' }
EphemeronIdentityDictionaryTest >> testFinalizeValuesWhenLastChainContinuesAtFront [
	| objectWithHashModulo dictionary capacity a b c |
	objectWithHashModulo :=
		[ :requestedHash :modulo |
			| object allOfThem tries |
			tries := 0.
			allOfThem := OrderedCollection new.
			[
				tries := tries + 1.
				tries > 100 ifTrue: [ ^self ].
				object := Object new.
				allOfThem add: object.
				object hash \\ modulo = requestedHash ] whileFalse.
			allOfThem removeAll.
			object ].

	dictionary := self classToBeTested new.
	capacity := (dictionary instVarNamed: 'array') size.
	a := objectWithHashModulo value: capacity - 2 value: capacity.
	dictionary at: a put: 1.
	b := objectWithHashModulo value: capacity - 1 value: capacity.
	dictionary at: b put: 2.
	c := objectWithHashModulo value: capacity - 2 value: capacity.
	dictionary at: c put: 3.
	self assert: (dictionary instVarNamed: 'array') size equals: capacity.
	self assert: (dictionary array at: capacity - 1) key == a.
	self assert: (dictionary array at: capacity) key == b.
	self assert: (dictionary array at: 1) key == c.
	a := nil.
		self triggerFinalization.

	self assert: (dictionary includesKey: b).
	self assert: (dictionary includesKey: c).
	self assert: dictionary size equals: 2
]

{ #category : 'tests' }
EphemeronIdentityDictionaryTest >> testGrow [
	keys := nil.
	dict at: self put: nil.
		self triggerFinalization.

	"Keys are gone but not yet finalized."
	dict grow
]

{ #category : 'tests' }
EphemeronIdentityDictionaryTest >> testNoNils [
	| d |
	d := EphemeronIdentityDictionary new
	at: 'hello' copy put: 'world';
		yourself.
		self triggerFinalization.
	self deny: (d keys includes: nil)
]

{ #category : 'running' }
EphemeronIdentityDictionaryTest >> triggerFinalization [

	| x | 
	
	x := (Interval from: 1 to: 100000) asOrderedCollection.
	x := nil.
	Smalltalk garbageCollect.
	Smalltalk garbageCollectMost.
	Smalltalk garbageCollect.
]
