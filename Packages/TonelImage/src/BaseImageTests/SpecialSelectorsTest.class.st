"
Tests for proper behavior of Special Selectors, for example #@
"
Class {
	#name : 'SpecialSelectorsTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Objects',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Objects'
}

{ #category : 'testing' }
SpecialSelectorsTest >> testCollectionAtSymbol [
	self assert: (#(1 2 3) @ #(10 20 30)) class == Array
]

{ #category : 'testing' }
SpecialSelectorsTest >> testNumberAtSymbol [
	self assert: (1@2) class == Point
]
