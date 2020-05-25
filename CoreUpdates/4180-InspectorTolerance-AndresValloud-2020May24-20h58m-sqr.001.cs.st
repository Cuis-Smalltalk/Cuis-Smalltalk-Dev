'From Cuis 5.0 [latest update: #4179] on 24 May 2020 at 9:02:58 pm'!

!SequenceableCollectionInspector methodsFor: 'accessing' stamp: 'sqr 5/24/2020 21:00:15'!
fieldList
	"Must tolerate malformed objects"

	| fieldsHere |
	object isNil ifTrue: [^#()].
	fieldsHere _ [self indexLabelsForSize: self objectSize]
		on: UnhandledError
		do: [:ex | ex return: #()].
	^self baseFieldList , fieldsHere
"
OrderedCollection new inspect
(OrderedCollection newFrom: #(3 5 7 123)) inspect
(OrderedCollection newFrom: (1 to: 1000)) inspect
"! !

!SequenceableCollectionInspector methodsFor: 'accessing' stamp: 'sqr 5/24/2020 21:00:01'!
objectSize
	"Must tolerate malformed objects"

	^[object size]
		on: UnhandledError
		do: [:ex | ex return: 0]! !

