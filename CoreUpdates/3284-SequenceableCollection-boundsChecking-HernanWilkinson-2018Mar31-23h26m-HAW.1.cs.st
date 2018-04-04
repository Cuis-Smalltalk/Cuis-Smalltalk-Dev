'From Cuis 5.0 of 7 November 2016 [latest update: #3283] on 31 March 2018 at 11:34:13 pm'!

!SequenceableCollection methodsFor: 'testing' stamp: 'HAW 3/31/2018 23:26:34'!
ifInBounds: anIndex ifNot: aValuable

	^(self isInBounds: anIndex) ifTrue: [ anIndex ] ifFalse: aValuable ! !

!SequenceableCollection methodsFor: 'testing' stamp: 'HAW 3/31/2018 23:26:24'!
isInBounds: anIndex

	^anIndex between: 1 and: self size! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'HAW 3/31/2018 23:26:53'!
at: index ifAbsent: exceptionBlock
	"Answer the element at my position index. If I do not contain an element
	at index, answer the result of evaluating the argument, exceptionBlock."

	(self isInBounds: index) ifTrue: [^self at: index].
	^exceptionBlock value! !

