'From Cuis7.7 [latest update: #7856] on 7 April 2026 at 12:21:56 pm'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'FJG 4/Apr/2026 14:16:45'!
at: index ifPresent: presentBlock
	"Lookup the given key in the receiver. If it is present, answer the value
	of evaluating the given block with the value associated with the index.
	Otherwise, answer nil."

	| v |
	v := self at: index ifAbsent: [ ^ nil ].
	^ presentBlock value: v
! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'FJG 4/Apr/2026 14:20:15'!
at: index ifPresent: presentBlock ifAbsent: exceptionBlock
	"Answer the result of evaluating presentBlock with the element at my
	position index. If I do not contain an element at index, answer the result
	of evaluating exceptionBlock."

	(self isInBounds: index) ifFalse: [ ^ exceptionBlock value ].
	^ presentBlock value: (self at: index)! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'FJG 4/Apr/2026 14:20:59'!
at: index ifAbsent: exceptionBlock
	"Answer the element at my position index. If I do not contain an element
	at index, answer the result of evaluating the argument, exceptionBlock."

	(self isInBounds: index) ifFalse: [ ^ exceptionBlock value ].
	^ self at: index! !


!Dictionary methodsFor: 'accessing' stamp: 'FJG 4/Apr/2026 14:20:02'!
at: key ifPresent: presentBlock ifAbsent: absentBlock
	"Answer the result of evaluating presentBlock with the value
	associated with key or, if key isn't found, answer the result of
	evaluating absentBlock."
	"
		Smalltalk at: #zork ifPresent: [ :cls | (cls name, ' present') print ] ifAbsent: [ 'zork absent' print ]
		Smalltalk at: #Number ifPresent: [ :cls | (cls name, ' present') print ] ifAbsent: [ 'Number absent' print ]
	"

	| assoc |
	assoc := array at: (self findElementOrNil: key).
	assoc ifNil: [ ^ absentBlock value ].
	^ presentBlock value: assoc value! !

