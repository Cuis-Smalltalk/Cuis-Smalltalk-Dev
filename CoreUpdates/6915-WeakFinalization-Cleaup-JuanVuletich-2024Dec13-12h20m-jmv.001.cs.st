'From Cuis7.3 [latest update: #6918] on 13 December 2024 at 12:22:43 pm'!

!WeakRegistry methodsFor: 'adding' stamp: 'jmv 12/13/2024 12:05:32'!
add: anObject
	"Add anObject to the receiver. Store the object as well as the associated executor."
	| executor |
	executor := anObject shallowCopy actAsExecutor.
	self protected:[
		valueDictionary at: anObject put: executor.
	].
	^anObject! !

!methodRemoval: UniFileStream #actAsExecutor stamp: 'jmv 12/13/2024 12:21:39'!
UniFileStream removeSelector: #actAsExecutor!
!methodRemoval: Object #executor stamp: 'jmv 12/13/2024 12:22:03'!
Object removeSelector: #executor!
