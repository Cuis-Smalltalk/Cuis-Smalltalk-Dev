"
WeakArray is an array which holds only weakly on its elements. This means whenever an object is only referenced by instances of WeakArray it will be garbage collected.
"
Class {
	#name : 'WeakArray',
	#superclass : 'Array',
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'library membership' }
WeakArray class >> includeInMinimalKernel02Symbol [
	"Making this method part of the MinimalKernel02Symbol library means
	this class is also included, even if no actually used method is added."
]
