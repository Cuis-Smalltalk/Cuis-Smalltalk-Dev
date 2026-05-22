Class {
	#name : 'OrderedDictionaryTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'aux' }
OrderedDictionaryTest >> sampleData [
	^ { {1. 'uno'}. {6. 'seis'}. {8. 'ocho'}. {7. 'siete'}. {2. 'dos'}. {3. 'tres'}. {4. 'cuatro'}. {5. 'cinco'}}
]

{ #category : 'aux' }
OrderedDictionaryTest >> sampleData2 [
	^ { {1. 'uno'}. {8. 'ocho'}. {3. 'tres'}}
]

{ #category : 'tests' }
OrderedDictionaryTest >> testCopy [
	| data dict dictCopy dataToRemove  |

	"Test that #do:, #select: and #collect: iterate in the correct order"
	dict := OrderedDictionary new.
	data := self sampleData.
	dataToRemove := self sampleData2.

	data do: [ :pair |
		dict at: pair first put: pair second ].

	dictCopy := dict copy.

	dataToRemove do: [ :pair |
		dict removeKey: pair first ].
	dataToRemove do: [ :pair |
		dictCopy removeKey: pair first ].
	self assert: dict keys = dictCopy keys.
]

{ #category : 'tests' }
OrderedDictionaryTest >> testOrder [
	| data dict orderingByCollect orderingByDo orderingBySelect aux desiredOrder |

	"Test that #do:, #select: and #collect: iterate in the correct order"
	dict _ OrderedDictionary new.
	data _ self sampleData.
	desiredOrder _ data collect: [ :pair | pair second ].
	data do: [ :pair |
		dict at: pair first put: pair second ].

	orderingByDo _ Array streamContents: [ :strm | dict do: [ :each | strm nextPut: each ]].
	self assert: orderingByDo = desiredOrder.

	aux _ dict select: [ :each | true ].
	orderingBySelect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingBySelect = desiredOrder.

	aux _ dict collect: [ :each | each yourself ].
	orderingByCollect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingByCollect = desiredOrder.


	"Test that order is correct even if different. Also test alternative way to add stuff to dict."
	dict _ OrderedDictionary new.
	data _ self sampleData reversed.
	desiredOrder _ data collect: [ :pair | pair second ].
	data do: [ :pair |
		dict add: pair first -> pair second ].

	orderingByDo _ Array streamContents: [ :strm | dict do: [ :each | strm nextPut: each ]].
	self assert: orderingByDo = desiredOrder.

	aux _ dict select: [ :each | true ].
	orderingBySelect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingBySelect = desiredOrder.

	aux _ dict collect: [ :each | each yourself ].
	orderingByCollect _ Array streamContents: [ :strm | aux do: [ :each | strm nextPut: each ]].
	self assert: orderingByCollect = desiredOrder.

]

{ #category : 'tests' }
OrderedDictionaryTest >> testRemove [
	| data dict dataToRemove desiredOrder orderingByDo |

	"Test that #do:, #select: and #collect: iterate in the correct order"
	dict _ OrderedDictionary new.
	data _ self sampleData.
	dataToRemove _ self sampleData2.

	data do: [ :pair |
		dict at: pair first put: pair second ].
	dataToRemove do: [ :pair |
		dict removeKey: pair first ].

	orderingByDo _ Array streamContents: [ :strm | dict do: [ :each | strm nextPut: each ]].

	desiredOrder _ OrderedCollection new.
	data do: [ :pair | desiredOrder add: pair second ].
	dataToRemove do: [ :pair | desiredOrder remove: pair second ].
	
	self assert: orderingByDo = desiredOrder asArray.
]
