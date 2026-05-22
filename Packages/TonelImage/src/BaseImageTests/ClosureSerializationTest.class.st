Class {
	#name : 'ClosureSerializationTest',
	#superclass : 'TestCase',
	#instVars : [
		'ivar'
	],
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'sample closures' }
ClosureSerializationTest >> bc01 [
	^[ self gimme5 ]
]

{ #category : 'sample closures' }
ClosureSerializationTest >> bc02 [
	^[ ivar + 4 ]
]

{ #category : 'sample closures' }
ClosureSerializationTest >> bc03 [
	^[ ivar _ ivar + 3 ]
]

{ #category : 'sample closures' }
ClosureSerializationTest >> bc04 [
	| t |
	t _ self gimme5.
	^[ t * 2 ]
]

{ #category : 'sample closures' }
ClosureSerializationTest >> bc05 [
	| t |
	t _ self gimme5.
	^[ t _ t * 2 ]
]

{ #category : 'sample closures' }
ClosureSerializationTest >> bc06 [
	| u t |
	u _ self gimme5 + 2.
	t _ self gimme5.
	^{[ u * 2 ]. [ t * 2 ].[t _ t*3]}.
]

{ #category : 'sample closures' }
ClosureSerializationTest >> bc07 [
	| a b c d e f g |
	a _ self gimme5 + 1.
	b _ self gimme5 + 2.
	c _ self gimme5 + 3.
	d _ self gimme5 + 4.
	e _ self gimme5 + 5.
	f _ self gimme5 + 6.
	g _ self gimme5 + 7.
	^{[ a * 10 + b * 10 + c * 10 + d * 10 + e + f + g ]. [ a _ 7. a * 2 ].[ c _ 9. d _ d*10. a * 10 + b * 10 + c + d]}.
]

{ #category : 'sample closures' }
ClosureSerializationTest >> bc08 [

	| u t |
	u _ self gimme5 + 2.
	t _ self gimme5.
	^{
		[ | x y z | x _ t. y _ x*0+2. z _ x * t. {u * 2. x. y. z} ].
		[ :a | t * 2 + a ].
		[|x y | t _ t*3. y _ t*2. {x. t. y}].
		[:a :b | a * b * t ]
	}.
]

{ #category : 'sample closures' }
ClosureSerializationTest >> bc09 [

	| clo1 clo2 closure |
	closure := (
			([ :a |
				[ :b |
					clo1 := [ :c | {c. b. a.}]. 
					clo2 := [ :d :e | {a. b. e. d.}]
				]
			]) value: $a
		) value: 'b'.
	^{ clo1. clo2. closure }
]

{ #category : 'sample closures' }
ClosureSerializationTest >> bc10 [

	^ [ :a :b | a + b ]
]

{ #category : 'sample closures' }
ClosureSerializationTest >> bc11 [
	| a b c d e f g |
	a := self gimme5 + 1.
	b := self gimme5 + 2.
	c := self gimme5 + 3.
	d := self gimme5 + 4.
	e := self gimme5 + 5.
	f := self gimme5 + 6.
	g := self gimme5 + 7.
	{ b. d. e. f }"print".
	^{[ g + a * 10 + a + a + g + c ]. [ b := 7. c := c * 2 ]}.
]

{ #category : 'aux' }
ClosureSerializationTest >> gimme5 [
	^1+4
]

{ #category : 'setUp/tearDown' }
ClosureSerializationTest >> setUp [
	super setUp.
	ivar := 7.
	^ivar
]

{ #category : 'testing' }
ClosureSerializationTest >> testSample01 [
	| blockClosure materialized |
	blockClosure _ self bc01.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.
]

{ #category : 'testing' }
ClosureSerializationTest >> testSample02 [
	| blockClosure materialized |
	blockClosure _ self bc02.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.
]

{ #category : 'testing' }
ClosureSerializationTest >> testSample03 [
	| blockClosure materialized |
	blockClosure _ self bc03.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.
]

{ #category : 'testing' }
ClosureSerializationTest >> testSample04 [
	| blockClosure materialized |
	blockClosure _ self bc04.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.
]

{ #category : 'testing' }
ClosureSerializationTest >> testSample05 [
	| blockClosure materialized |
	blockClosure _ self bc05.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.
]

{ #category : 'testing' }
ClosureSerializationTest >> testSample06 [
	| blockClosures materialized firstRun |

	blockClosures _ self bc06.
	firstRun _ blockClosures second value.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures first value = materialized first value.
	self assert: firstRun = materialized second value.
	self deny: blockClosures second value = materialized second value.

	blockClosures _ self bc06.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures first value = materialized first value.
	self deny: firstRun = materialized second value.
	self assert: blockClosures second value = materialized second value.
]

{ #category : 'testing' }
ClosureSerializationTest >> testSample07 [
	| blockClosures materialized |

	blockClosures _ self bc07.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures third value = materialized third value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures first value = materialized first value.
	self deny: blockClosures third value = materialized third value.
	self deny: blockClosures first value = materialized first value.

	blockClosures _ self bc07.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures first value = materialized first value.
]

{ #category : 'testing' }
ClosureSerializationTest >> testSample08 [
	| blockClosures materialized |

	blockClosures _ self bc08.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: (blockClosures second value: 7) = (materialized second value: 7).
	self assert: blockClosures third value = materialized third value.
	self deny: (blockClosures second value: 7) = (materialized second value: 7).
	self deny: (blockClosures fourth value: 2 value: 3) = (materialized fourth value: 2 value: 3).

	blockClosures _ self bc08.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: (blockClosures second value: 7) = (materialized second value: 7).
	self assert: blockClosures third value = materialized third value.
	self assert: (blockClosures second value: 7) = (materialized second value: 7).
	self assert: (blockClosures fourth value: 2 value: 3) = (materialized fourth value: 2 value: 3).
]

{ #category : 'testing' }
ClosureSerializationTest >> testSample09 [
	| blockClosures materialized |

	blockClosures _ self bc09.
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].

	self assert: (blockClosures first value: $c) = (materialized first value: $c).
	self assert: (blockClosures second value: $d value: #e) = (materialized second value: $d value: #e).
	self assert: (blockClosures third value: $d value: $e) = (materialized third value: $d value: $e).
]

{ #category : 'testing' }
ClosureSerializationTest >> testSample10 [
	| blockClosure1 blockClosure2 blockClosure3 value materialized1 materialized2a materialized2b materialized3a materialized3b materialized3c |

	blockClosure1 _ self bc10.
	blockClosure2 _ blockClosure1 withFirstArg: 1.
	blockClosure3 _ (blockClosure1 withFirstArg: 1) withFirstArg: 2.
	value _ blockClosure1 value: 1 value: 2.
	self assert: (blockClosure2 value: 2) = value.
	self assert: blockClosure3 value = value.

	materialized1 _ blockClosure1 veryDeepCopy.
	materialized2a _ materialized1 withFirstArg: 1.
	materialized2b _ blockClosure2 veryDeepCopy.
	materialized3a _ (materialized1 withFirstArg: 1) withFirstArg: 2.
	materialized3b _ materialized2b withFirstArg: 2.
	materialized3c _ blockClosure3 veryDeepCopy.
	self assert: (materialized1 value: 1 value: 2) = value.
	self assert: (materialized2a value: 2) = value.
	self assert: (materialized2b value: 2) = value.
	self assert: materialized3a value = value.
	self assert: materialized3b value = value.
	self assert: materialized3c value = value.

]

{ #category : 'testing' }
ClosureSerializationTest >> testSample11 [
	| blockClosures materialized |

	blockClosures _ self bc11.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures first value = materialized first value.

	blockClosures _ self bc11.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures first value = materialized first value.
]
