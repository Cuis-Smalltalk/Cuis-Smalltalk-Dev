Class {
	#name : 'FinalizationRegistryTest',
	#superclass : 'TestCase',
	#instVars : [
		'finalizationRegistry',
		'finalizerThatSignals',
		'finalizerFired',
		'finalizerThatRaisesError'
	],
	#category : 'BaseImageTests-System-Finalization',
	#package : 'BaseImageTests',
	#tag : 'System-Finalization'
}

{ #category : 'running' }
FinalizationRegistryTest >> setUp [

	super setUp.

	finalizationRegistry := FinalizationRegistry new.

	finalizerFired := false.
	finalizerThatSignals := ObjectFinalizer new receiver: [
		finalizerFired := true ] selector: #value.

	finalizerThatRaisesError := ObjectFinalizer new receiver: [ 
		Error signal ] selector: #value.
]

{ #category : 'tests' }
FinalizationRegistryTest >> testAddGC [
	| x |

	
	x := Object new.
	finalizationRegistry add: x.
	self assert: (finalizationRegistry size = 1).
	self assert: (finalizationRegistry isEmpty not).
	x := nil.
	self triggerFinalization.
	self assert: (finalizationRegistry size = 0).
	self assert: (finalizationRegistry isEmpty).
]

{ #category : 'tests' }
FinalizationRegistryTest >> testDo [
	| x |

	
	x := Object new.
	finalizationRegistry add: x.
	self assert: finalizationRegistry size = 1.
	finalizationRegistry keysDo: [:v | self assert: v = x]
]

{ #category : 'tests' }
FinalizationRegistryTest >> testEKeyDictionary [
	
	| d x |
	d := EphemeronIdentityDictionary new.
	x := Object new.
	d at: x put: 5.
	x := nil.
	self triggerFinalization.
	self assert: d isEmpty

]

{ #category : 'tests' }
FinalizationRegistryTest >> testFinalizationWithError [
	| reference |

	"This test can not run without the dev environment:
	It is expected to raise an UnhandledException, and the trick at the end to close
	the debugger can only be done if #isDevelopmentEnvironmentPresent"
	Smalltalk isDevelopmentEnvironmentPresent ifFalse:
		[ ^self ].

	finalizationRegistry
		add: Object new
		executor: finalizerThatRaisesError.
	finalizationRegistry
		add: (reference := Object new)
		executor: finalizerThatSignals.

	self triggerFinalization.
	self deny: finalizerFired.
	reference := nil.
	self triggerFinalization.
	self assert: finalizerFired.
	UISupervisor whenUIinSafeState:
		[DebuggerWindow closeAllDebuggersSuchThat:
			[:dbgWindow| '*Ephemeron Mourning*' match: dbgWindow label]]
]

{ #category : 'tests' }
FinalizationRegistryTest >> testLegacy  [
	| x |

	
	x := Object new.
	finalizationRegistry add: x.
	x := nil.
	finalizationRegistry finalizeValues
]

{ #category : 'tests' }
FinalizationRegistryTest >> testRemove [
	| x |

	
	x := Object new.
	finalizationRegistry add: x.
	self assert: finalizationRegistry size = 1.
	finalizationRegistry removeKey: x ifAbsent: [self assert: false].
	finalizationRegistry removeKey: x ifAbsent: [^self assert: true].
	self assert: false.

]

{ #category : 'tests' }
FinalizationRegistryTest >> testRemoveAll [
	| x y |

	
	x := Object new.
	finalizationRegistry add: x.
	y := Object new.
	finalizationRegistry add: y.
	self assert: finalizationRegistry size = 2.
	finalizationRegistry removeAll.
	self assert: finalizationRegistry size = 0.

]

{ #category : 'tests' }
FinalizationRegistryTest >> testkey [
	| x |

	
	x := Object new.
	finalizationRegistry add: x.
	self assert: finalizationRegistry size = 1.
	self assert: (finalizationRegistry keys size = 1).
	finalizationRegistry keys do: [:k | self assert: k = x].
	
]

{ #category : 'helpers' }
FinalizationRegistryTest >> triggerFinalization [

	| x | 
	
	x := (Interval from: 1 to: 100000) asOrderedCollection.
	x := nil.
	Smalltalk garbageCollect.
	Smalltalk garbageCollectMost.
	Smalltalk garbageCollect.
]
