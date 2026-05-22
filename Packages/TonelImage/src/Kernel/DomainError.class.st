"
A DomainError is an error occuring when a mathematical function is used outside its domain of validity.
"
Class {
	#name : 'DomainError',
	#superclass : 'ArithmeticError',
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'library membership' }
DomainError class >> includeInMinimalKernel03Float [
	"Making this method part of the MinimalKernel03Float library means
	this class is also included, even if no actually used method is added."
]
