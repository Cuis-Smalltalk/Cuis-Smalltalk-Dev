"
See comments in ClosureScanner.
"
Class {
	#name : 'WeightExplorerWrapper',
	#superclass : 'ReferencesExplorerWrapper',
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'reference chain' }
WeightExplorerWrapper >> contents [

	| answer |
	answer := Array new: model paths size.
	model paths withIndexDo:
		[:each :eachIndex |
			| name newWrapper |
			name := self weightStringFrom: model object to: each object count: eachIndex.
			newWrapper := self class with: each object name: name model: each parent: self.
			newWrapper finderSkipsClasses: self finderSkipsClasses.
			answer at: eachIndex put: newWrapper.
		].
	^answer
]

{ #category : 'printing' }
WeightExplorerWrapper >> weightStringFrom: parentObject to: childObject count: anInteger [

	| answer |
	answer := String writeStream.
	self print: parentObject on: answer.
	^answer contents
]
