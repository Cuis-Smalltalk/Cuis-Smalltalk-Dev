"
General system - wide image health tests.
"
Class {
	#name : 'SystemConsistencyTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-Support',
	#package : 'BaseImageTests',
	#tag : 'System-Support'
}

{ #category : 'testing' }
SystemConsistencyTest >> testMethodsWithUnboundGlobals [
	"
	SystemConsistencyTest new testMethodsWithUnboundGlobals
	"
	self assert: Smalltalk methodsWithUnboundGlobals isEmpty
]
