Class {
	#name : 'ColorFormTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Graphics-Display Objects',
	#package : 'BaseImageTests',
	#tag : 'Graphics-Display Objects'
}

{ #category : 'testing' }
ColorFormTest >> testIsColorForm [
	self assert: (ColorForm new is: #ColorForm).
]
