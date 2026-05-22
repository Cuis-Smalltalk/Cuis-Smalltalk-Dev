Class {
	#name : 'GrayFormTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Graphics-Display Objects',
	#package : 'BaseImageTests',
	#tag : 'Graphics-Display Objects'
}

{ #category : 'as yet unclassified' }
GrayFormTest >> testIsGrayForm [
	self assert: (GrayForm new is: #GrayForm)
]
