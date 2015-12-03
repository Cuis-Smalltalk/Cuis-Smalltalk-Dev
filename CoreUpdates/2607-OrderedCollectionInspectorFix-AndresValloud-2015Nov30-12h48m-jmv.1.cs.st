'From Cuis 4.2 of 25 July 2013 [latest update: #2605] on 30 November 2015 at 12:50:43.477151 pm'!

!OrderedCollectionInspector methodsFor: 'as yet unclassified' stamp: 'sqr 11/28/2015 17:11'!
fieldList

	| fieldsHere |
	object isNil ifTrue: [^OrderedCollection new].
	fieldsHere _
		[
			(object size <= (self i1 + self i2)
				ifTrue: [(1 to: object size) collect: [:i | i printString]]
				ifFalse: [(1 to: self i1) , (object size-(self i2-1) to: object size) collect: [:i | i printString]])
		] on: Error do: [:ex | ex return: OrderedCollection new].	
	^self baseFieldList , fieldsHere
"
OrderedCollection new inspect
(OrderedCollection newFrom: #(3 5 7 123)) inspect
(OrderedCollection newFrom: (1 to: 1000)) inspect
"! !

