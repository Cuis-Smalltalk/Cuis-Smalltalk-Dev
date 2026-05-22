Class {
	#name : 'SequenceDifference',
	#superclass : 'Object',
	#instVars : [
		'x',
		'y',
		'lcs'
	],
	#category : 'Tools-Code Differ',
	#package : 'Tools',
	#tag : 'Code Differ'
}

{ #category : 'instance creation' }
SequenceDifference class >> x: x y: y lcs: sequence [
	^self new x: x y: y lcs: sequence

]

{ #category : 'testing' }
SequenceDifference >> <= sequence [
	^lcs size <= sequence lcsSize

]

{ #category : 'printing' }
SequenceDifference >> asText [
	^Text streamContents: [:rtf | self printTextOn: rtf]
]

{ #category : 'printing' }
SequenceDifference >> attributesFor: condition  [
	condition == #unchanged
		ifTrue: [ 
			^ {TextEmphasis normal} ].
	condition == #removed
		ifTrue: [ 
			^ {TextEmphasis struckThrough. TextColor red} ].
	condition == #inserted
		ifTrue: [ 
			^ {TextColor green} ]
]

{ #category : 'enumerating' }
SequenceDifference >> do: aBlock [
	| j i item |
	i := j := 1.
	lcs do: [:pair | 
		[i < pair first] whileTrue: [
			item := x at: i.
			aBlock value: item value: #removed.
			i := i + 1].
		[j < pair second] whileTrue: [
			item := y at: j.
			aBlock value: item value: #inserted.
			j := j + 1].
		item := x at: i.
		aBlock value: item value: #unchanged.
		i := i + 1.
		j := j + 1].
	i to: x size do: [:k | 
		item := x at: k.
		aBlock value: item value: #removed].
	j to: y size do: [:k | 
		item := y at: k.
		aBlock value: item value: #inserted]

]

{ #category : 'services' }
SequenceDifference >> invert [
	| swap |
	swap := x.
	x := y.
	y := swap.
	lcs := lcs collect: [:pair | pair copy swap: 1 with: 2]

]

{ #category : 'accessing' }
SequenceDifference >> lcsSize [
	^lcs size
]

{ #category : 'accessing' }
SequenceDifference >> partsSize [
	| count last |
	count := 0.
	self do: [:item :condition | 
		last = condition ifFalse: [
			count := count + 1.
			last := condition]].
	^count

]

{ #category : 'printing' }
SequenceDifference >> printTextOn: rtf [
	self do: [:item :condition | | attributes |
		attributes := self attributesFor: condition.
		rtf withAttributes: attributes do: [rtf nextPutAll: item asString]]
]

{ #category : 'services' }
SequenceDifference >> size [
	^lcs sum: [:pair | (x at: pair first) size]

]

{ #category : 'accessing' }
SequenceDifference >> x: aCollection y: anotherCollection lcs: pairCollection [
	x := aCollection.
	y := anotherCollection.
	lcs := pairCollection sort: [ :a :b | a first < b first ]
]
