"
I represent a collection of links, which are containers for other objects. Using the message sequence addFirst:/removeLast causes the receiver to behave as a stack; using addLast:/removeFirst causes the receiver to behave as a queue.
"
Class {
	#name : 'LinkedList',
	#superclass : 'SequenceableCollection',
	#instVars : [
		'firstLink',
		'lastLink'
	],
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'adding' }
LinkedList >> add: aLink  [
	"Add aLink to the end of the receiver's list."

	^self addLast: aLink
]

{ #category : 'adding' }
LinkedList >> add: link before: otherLink [

	| aLink |
	firstLink == otherLink ifTrue: [^ self addFirst: link].
	aLink := firstLink.
	[aLink == nil] whileFalse: [
		aLink nextLink == otherLink ifTrue: [
			link nextLink: aLink nextLink.
			aLink nextLink: link.
			^ link
		].
		 aLink := aLink nextLink.
	].
	^ self errorNotFound: otherLink
]

{ #category : 'adding' }
LinkedList >> addFirst: aLink  [
	"Add aLink to the beginning of the receiver's list."

	self isEmpty ifTrue: [lastLink := aLink].
	aLink nextLink: firstLink.
	firstLink := aLink.
	^aLink
]

{ #category : 'adding' }
LinkedList >> addLast: aLink  [
	"Add aLink to the end of the receiver's list."

	self isEmpty
		ifTrue: [firstLink := aLink]
		ifFalse: [lastLink nextLink: aLink].
	lastLink := aLink.
	^aLink
]

{ #category : 'accessing' }
LinkedList >> at: index [

	^self at: index ifAbsent: [ self errorSubscriptBounds: index ]
]

{ #category : 'accessing' }
LinkedList >> at: index ifAbsent: exceptionBlock [

	| i |
	index < 1 ifTrue: [ ^exceptionBlock value ].
	i := 0.
	self do: [ :link |
		(i := i + 1) = index ifTrue: [ ^ link ]].
	^ exceptionBlock value
]

{ #category : 'enumerating' }
LinkedList >> do: aBlock [
	| aLink |
	aLink := firstLink.
	[aLink == nil] whileFalse:
		[aBlock value: aLink.
		 aLink := aLink nextLink]
]

{ #category : 'accessing' }
LinkedList >> first [
	"Answer the first link;  create an error if the receiver is empty."

	self emptyCheck.
	^firstLink
]

{ #category : 'testing' }
LinkedList >> isEmpty [
	^firstLink == nil
]

{ #category : 'accessing' }
LinkedList >> last [
	"Answer the last link;  create an error if the receiver is empty."

	self emptyCheck.
	^lastLink
]

{ #category : 'removing' }
LinkedList >> remove: aLink ifAbsent: aBlock   [
	"Remove aLink from the receiver. If it is not there, answer the result of
	evaluating aBlock."

	| tempLink |
	aLink == firstLink
		ifTrue: [firstLink := aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink := nil]]
		ifFalse: [tempLink := firstLink.
				[tempLink ifNil: [^aBlock value].
				 tempLink nextLink == aLink]
					whileFalse: [tempLink := tempLink nextLink].
				tempLink nextLink: aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink := tempLink]].
	aLink nextLink: nil.
	^aLink
]

{ #category : 'removing' }
LinkedList >> removeFirst [
	"Remove the first element.  If the receiver is empty, cause an error;
	otherwise answer the removed element.  Using the sequence addFirst:/removeFirst
	causes the receiver to behave as a stack; using addLast:/removeFirst causes the
	receiver to behave as a queue."

	| oldLink |
	self emptyCheck.
	oldLink := firstLink.
	firstLink == lastLink
		ifTrue: [firstLink := nil. lastLink := nil]
		ifFalse: [firstLink := oldLink nextLink].
	oldLink nextLink: nil.
	^oldLink
]

{ #category : 'removing' }
LinkedList >> removeLast [
	"Remove the receiver's last element.  If the receiver is empty, cause an error;
	otherwise answer the removed element.  Using addLast:/removeLast causes the
	receiver to behave as a stack; using addFirst:/removeLast causes the receiver to
	behave as a queue."

	| oldLink aLink |
	self emptyCheck.
	oldLink := lastLink.
	firstLink == lastLink
		ifTrue: [firstLink := nil. lastLink := nil]
		ifFalse: [aLink := firstLink.
				[aLink nextLink == oldLink] whileFalse:
					[aLink := aLink nextLink].
				 aLink nextLink: nil.
				 lastLink := aLink].
	oldLink nextLink: nil.
	^oldLink
]

{ #category : 'enumerating' }
LinkedList >> species [

	^ Array
]
