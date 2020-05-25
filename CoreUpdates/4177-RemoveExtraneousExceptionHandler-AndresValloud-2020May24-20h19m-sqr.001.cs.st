'From Cuis 5.0 [latest update: #4176] on 24 May 2020 at 8:20:17 pm'!

!SequenceableCollectionInspector methodsFor: 'accessing' stamp: 'sqr 5/24/2020 06:32:42'!
fieldList

	| fieldsHere |
	object isNil ifTrue: [^#()].
	fieldsHere _ self indexLabelsForSize: self objectSize.
	^self baseFieldList , fieldsHere
"
OrderedCollection new inspect
(OrderedCollection newFrom: #(3 5 7 123)) inspect
(OrderedCollection newFrom: (1 to: 1000)) inspect
"! !

