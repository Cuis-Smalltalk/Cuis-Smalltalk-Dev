Class {
	#name : 'IfNotNilTests',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Objects',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Objects'
}

{ #category : 'tests' }
IfNotNilTests >> testIfNilIfNotNil0Arg [

	self assert: (5@4 ifNil: [#foo] ifNotNil: [#bar]) = #bar.
	self assert: (nil ifNil: [#foo] ifNotNil: [#bar]) = #foo
]

{ #category : 'tests' }
IfNotNilTests >> testIfNilIfNotNil0ArgAsVar [

	| block1 block2 |
	block1 := [#foo].
	block2 := [#bar].
	self assert: (5@4 ifNil: block1 ifNotNil: block2) = #bar.
	self assert: (nil ifNil: block1 ifNotNil: block2) = #foo
]

{ #category : 'tests' }
IfNotNilTests >> testIfNilIfNotNil1Arg [

	self assert: (5@4 ifNil: [#foo] ifNotNil: [:a | a printString]) = '5@4'.
	self assert: (nil ifNil: [#foo] ifNotNil: [:a | a printString]) = #foo
]

{ #category : 'tests' }
IfNotNilTests >> testIfNilIfNotNil1ArgAsVar [

	| block1 block2 |
	block1 := [#foo].
	block2 := [:a | a printString].
	self assert: (5@4 ifNil: block1 ifNotNil: block2) = '5@4'.
	self assert: (nil ifNil: block1 ifNotNil: block2) = #foo
]

{ #category : 'tests' }
IfNotNilTests >> testIfNotNil0Arg [

	self assert: (5@4 ifNotNil: [#foo]) = #foo.
	self assert: (nil ifNotNil: [#foo]) = nil
]

{ #category : 'tests' }
IfNotNilTests >> testIfNotNil0ArgAsVar [

	| block |
	block := [#foo].
	self assert: (5@4 ifNotNil: block) = #foo.
	self assert: (nil ifNotNil: block) = nil
]

{ #category : 'tests' }
IfNotNilTests >> testIfNotNil1Arg [

	self assert: (5@4 ifNotNil: [:a | a printString]) = '5@4'.
	self assert: (nil ifNotNil: [:a | a printString]) = nil
]

{ #category : 'tests' }
IfNotNilTests >> testIfNotNil1ArgAsVar [

	| block |
	block := [:a | a printString].
	self assert: (5@4 ifNotNil: block) = '5@4'.
	self assert: (nil ifNotNil: block) = nil
]

{ #category : 'tests' }
IfNotNilTests >> testIfNotNilIfNil0Arg [

	self assert: (5@4 ifNotNil: [#foo] ifNil: [#bar]) = #foo.
	self assert: (nil ifNotNil: [#foo] ifNil: [#bar]) = #bar
]

{ #category : 'tests' }
IfNotNilTests >> testIfNotNilIfNil0ArgAsVar [

	| block1 block2 |
	block1 := [#foo].
	block2 := [#bar].
	self assert: (5@4 ifNotNil: block2 ifNil: block1) = #bar.
	self assert: (nil ifNotNil: block2 ifNil: block1) = #foo
]

{ #category : 'tests' }
IfNotNilTests >> testIfNotNilIfNil1Arg [

	self assert: (5@4 ifNotNil: [:a | a printString] ifNil: [#foo]) = '5@4'.
	self assert: (nil ifNotNil: [:a | a printString] ifNil: [#foo]) = #foo
]

{ #category : 'tests' }
IfNotNilTests >> testIfNotNilIfNil1ArgAsVar [

	| block1 block2 |
	block1 := [#foo].
	block2 := [:a | a printString].
	self assert: (5@4 ifNotNil: block2 ifNil: block1) = '5@4'.
	self assert: (nil ifNotNil: block2 ifNil: block1) = #foo
]
