Class {
	#name : 'SmartRefStreamTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-Object Storage',
	#package : 'BaseImageTests',
	#tag : 'System-Object Storage'
}

{ #category : 'testing' }
SmartRefStreamTest >> testDiskProxy [
	"
	SmartRefStreamTest new testDiskProxy
	"
	| newInstance oldInstance |
	self flag: #jmvVer2.
	oldInstance _ { Smalltalk . Display . Morph}.
	newInstance _ SmartRefStream unStream: (SmartRefStream streamedRepresentationOf: oldInstance).
	1 to: oldInstance size do: [ :i |
		self assert: (newInstance at: i) == (oldInstance at: i) ]
]

{ #category : 'testing' }
SmartRefStreamTest >> testSortedCollection [
	"
	SmartRefStreamTest new testSortedCollection
	"
	| newInstance oldInstance |
	oldInstance _ SortedCollection sortBlock: [ :a :b | a printString < b printString ].
	oldInstance add: 'hi'; add: 'there'; add: 'you'; add: 'all'.
	newInstance _ SmartRefStream unStream: (SmartRefStream streamedRepresentationOf: oldInstance).
	self assert: newInstance asArray = oldInstance asArray
]

{ #category : 'testing' }
SmartRefStreamTest >> testWeakDumps [
	"Test that if we serialize a model with weak references to views, only the model is serialized and not the views.
	
	Note: The bug became apparent only when dumping a model to a SmartRefStream, that calls #references, and the serialized stream
	was later materialized in an image where the view classes had been deleted. In such rare cases, materialization would fail when trying to reference these
	absent classes. If serializing to a ReferenceStream, the bug didn't become apparent (views were never serialized). If serializing to a SmartRefStream, but
	view classes still existed, the bug didn't really become apparent (because views were not actually deserialized), the only effect was a larger file.
	
	SmartRefStreamTest new testWeakDumps
	"
	| oldInstance window refStream |
	oldInstance _ TextModel withText: 'This is a text'.
	window _ SystemWindow editText: oldInstance label: 'old instance' wrap: true.
	refStream _ SmartRefStream on: (DummyStream on: nil).
	refStream nextPut: oldInstance.
	self deny: (refStream references keys anySatisfy: [ :dumpedObject | dumpedObject isKindOf: Morph ]).
	UISupervisor whenUIinSafeState: [window delete].
]
