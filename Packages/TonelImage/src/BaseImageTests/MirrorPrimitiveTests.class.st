Class {
	#name : 'MirrorPrimitiveTests',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'tests' }
MirrorPrimitiveTests >> testMirrorAt [
	| stackpBefore stackpAfter array byteArray |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	array := { 1. 2. 3 }.
	byteArray := ByteArray with: 1 with: 2 with: 3.
	self assert: (thisContext object: array basicAt: 1) = 1.
	self assert: (thisContext object: byteArray basicAt: 2) = 2.
	thisContext object: array basicAt: 2 put: #two.
	self assert: array = #(1 #two 3).
	thisContext object: byteArray basicAt: 2 put: 222.
	self assert: byteArray asArray = #(1 222 3).
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter. "Make sure primitives pop all their arguments"
	self should: [thisContext object: array basicAt: 4] raise: Error.
	self should: [thisContext object: byteArray basicAt: 0] raise: Error.
	self should: [thisContext object: byteArray basicAt: 1 put: -1] raise: Error
]

{ #category : 'tests' }
MirrorPrimitiveTests >> testMirrorClass [
	| stackpBefore stackpAfter |
	stackpBefore := thisContext stackPtr.
	self assert: (thisContext objectClass: Array new) = Array.
	self assert: (thisContext objectClass: 1) = 1 class.
	self assert: (thisContext objectClass: ProtoObject new) = ProtoObject.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter "Make sure primitives pop all their arguments"
]

{ #category : 'tests' }
MirrorPrimitiveTests >> testMirrorEqEq [
	| stackpBefore stackpAfter |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	self assert: (thisContext object: Array new eqeq: Array new) == false.
	self assert: (thisContext object: Array eqeq: Array) == true.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter "Make sure primitives pop all their arguments"
]

{ #category : 'tests' }
MirrorPrimitiveTests >> testMirrorInstVarAt [
	| stackpBefore stackpAfter array point |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	array := { 1. 2. 3 }.
	point := Point x: 1 y: 2.
	self assert: (thisContext object: array instVarAt: 1) = 1.
	self assert: (thisContext object: point instVarAt: 2) = 2.
	thisContext object: array instVarAt: 2 put: #two.
	self assert: array = #(1 #two 3).
	thisContext object: point instVarAt: 1 put: 1/2.
	self assert: point = (Point x: 1 / 2 y: 2).
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter. "Make sure primitives pop all their arguments"
	self should: [thisContext object: array instVarAt: 4] raise: Error.
	self should: [thisContext object: point instVarAt: 3] raise: Error
]

{ #category : 'tests' }
MirrorPrimitiveTests >> testMirrorPerform [
	| stackpBefore stackpAfter anInterval |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	anInterval := 1 to: 2.
	self assert: (thisContext object: anInterval perform:# species withArguments: #() inClass: Interval) == Array.
	self assert: (thisContext object: anInterval perform:# species withArguments: #() inClass: Interval superclass) == Interval.
	self should: [thisContext object: anInterval perform:# species withArguments: #() inClass: Point]
		raise: Error.
	self should: [thisContext object: anInterval perform:# species withArguments: OrderedCollection new inClass: Interval]
		raise: Error.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter "Make sure primitives pop all their arguments"
]

{ #category : 'tests' }
MirrorPrimitiveTests >> testMirrorSize [
	| stackpBefore stackpAfter |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	self assert: (thisContext objectSize: #(1 2 3)) = 3.
	self assert: (thisContext objectSize: '123' asByteString) = 3.
	self assert: (thisContext objectSize: nil) = 0.
	self assert: (thisContext objectSize: 1) = 0.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter. "Make sure primitives pop all their arguments"
]
