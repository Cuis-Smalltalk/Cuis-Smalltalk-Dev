Class {
	#name : 'WeakIdentitySetTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'testing' }
WeakIdentitySetTest >> test [
	"
	(also tests WeakSet a bit)
	WeakIdentitySetTest new test
	"
	| ary1 ary2 count w wi |
	ary1 := { 3@4 . 4@5 }.
	ary2 := { 13@4 . 14@5 }.
	w := WeakSet new.
	w addAll: ary1.
	wi := WeakIdentitySet new.
	wi addAll: ary2.
	
	self assert: w size = 2.
	count := 0.
	w do: [ :each |
		count := count + 1.
		self assert: each class == Point ].
	self assert: count = 2.
	self assert: (w includes: ary1 first).
	self assert: (w includes: ary1 second).
	self assert: (w includes: 3@4).
	self assert: (w includes: 4@5).

	self assert: wi size = 2.
	count := 0.
	wi do: [ :each |
		count := count + 1.
		self assert: each class == Point ].
	self assert: count = 2.
	self assert: (wi includes: ary2 first).
	self assert: (wi includes: ary2 second).
	self deny: (wi includes: 13@4).
	self deny: (wi includes: 14@5).

	"Now make one element in each disappear"
	ary1 at: 1 put: 9.
	ary2 at: 1 put: 99.
	Smalltalk garbageCollect.

	self assert: w size = 1.
	self assert: w estimatedSize = 2.
	count := 0.
	w do: [ :each |
		count := count + 1 ].
	self assert: count = 1.
	self deny: (w includes: ary1 first).
	self assert: (w includes: ary1 second).

	self assert: wi size = 1.
	self assert: wi estimatedSize = 2.
	count := 0.
	wi do: [ :each |
		count := count + 1 ].
	self assert: count = 1.
	self deny: (wi includes: ary2 first).
	self assert: (wi includes: ary2 second).
]
