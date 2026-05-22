Class {
	#name : 'MetaclassTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Classes',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Classes'
}

{ #category : 'tests' }
MetaclassTest >> testHasSubclassesReturnsThisClassHasSubclasses [

	self assert: Object hasSubclasses equals: Object class hasSubclasses.
	self assert: self class hasSubclasses equals: self class class hasSubclasses 
]
