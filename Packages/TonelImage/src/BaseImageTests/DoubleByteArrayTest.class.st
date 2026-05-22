Class {
	#name : 'DoubleByteArrayTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections-Arrayed',
	#package : 'BaseImageTests',
	#tag : 'Collections-Arrayed'
}

{ #category : 'testing' }
DoubleByteArrayTest >> testSigned [
	"
	
	DoubleByteArray holds 16 bit unsigned or signed numbers.
	DoubleByteArrayTest new testSigned
	"
        | u16 |
        u16 := DoubleByteArray new: 7.

	self assert: u16 size = 7.
        u16 signedAt: 1 put:  16r7123.
        self assert: ((u16 signedAt: 1) = 16r7123).
        u16 signedAt: 1 put: 0.
        self assert: ((u16 signedAt: 1) = 0).
        u16 signedAt: 1 put:  -1.
        self assert: ((u16 signedAt: 1) = -1).
        u16 signedAt: 1 put:  -32768.
        self assert: ((u16 signedAt: 1) = -32768).
        self should: [u16 signedAt: 1 put:  -32769] raise: Error. "to big"
        self should: [u16 signedAt: 1 put:  (16r7FFF + 1)] raise: Error. "too big"
]

{ #category : 'testing' }
DoubleByteArrayTest >> testUnsigned [
	"
	DoubleByteArray holds 16 bit unsigned or signed numbers.
	DoubleByteArrayTest new testUnsigned
	"
        | u16 |
        u16 := DoubleByteArray new: 5.

	self assert: u16 size = 5.
        u16 at: 1 put:  16r8123.
        self assert: ((u16 at: 1) = 16r8123).
        u16 at: 1 put: 0.
        self assert: ((u16 at: 1) = 0).
        u16 at: 1 put:  32768.
        self assert: ((u16 at: 1) = 32768).
        u16 at: 1 put:  16rFFFF.
        self assert: ((u16 at: 1) = 16rFFFF).
        self should: [u16 at: 1 put:  -1] raise: Error. "wrong sign"
        self should: [u16 at: 1 put:  (16rFFFF + 1)] raise: Error. "too big"
]
