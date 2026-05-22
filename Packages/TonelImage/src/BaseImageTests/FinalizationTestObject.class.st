Class {
	#name : 'FinalizationTestObject',
	#superclass : 'Object',
	#instVars : [
		'finalizeAction'
	],
	#category : 'BaseImageTests-System-Finalization',
	#package : 'BaseImageTests',
	#tag : 'System-Finalization'
}

{ #category : 'finalization' }
FinalizationTestObject >> finalize [
	finalizeAction value
]

{ #category : 'accessing' }
FinalizationTestObject >> finalizeAction: aBlock [
	finalizeAction := aBlock
]
