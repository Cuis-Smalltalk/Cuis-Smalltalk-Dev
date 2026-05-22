"
See comments in ClosureScanner.
"
Class {
	#name : 'ReferencesExplorerWrapper',
	#superclass : 'ObjectExplorerWrapper',
	#instVars : [
		'finderSkipsClasses'
	],
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'reference chain' }
ReferencesExplorerWrapper >> contents [

	| answer |
	answer := Array new: model paths size.
	model paths withIndexDo:
		[:each :eachIndex |
			| name newWrapper |
			name := self pathStringFrom: model object to: each object count: eachIndex.
			newWrapper := self class with: each object name: name model: each parent: self.
			newWrapper finderSkipsClasses: self finderSkipsClasses.
			answer at: eachIndex put: newWrapper.
		].
	^answer
]

{ #category : 'accessing' }
ReferencesExplorerWrapper >> finderSkipsClasses [

	^finderSkipsClasses
]

{ #category : 'accessing' }
ReferencesExplorerWrapper >> finderSkipsClasses: aBoolean [

	finderSkipsClasses := aBoolean
]

{ #category : 'reference chain' }
ReferencesExplorerWrapper >> hasContents [

	^model paths notEmpty
]

{ #category : 'printing' }
ReferencesExplorerWrapper >> pathStringFrom: parentObject to: childObject count: anInteger [

	| answer referenceDescription |
	answer := String writeStream.
	referenceDescription := self referenceDescriptionFrom: parentObject to: childObject count: anInteger.
	self print: parentObject on: answer.
	answer
		nextPutAll: ' ';
		nextPutAll: referenceDescription;
		nextPutAll: ' --> '.
	self print: childObject on: answer.
	^answer contents
]

{ #category : 'printing' }
ReferencesExplorerWrapper >> print: anObject on: aStream [

	aStream
		nextPut: $(;
		print: anObject class;
		nextPut: $:;
		print: anObject identityHash;
		nextPut: $)
]

{ #category : 'printing' }
ReferencesExplorerWrapper >> referenceDescriptionFrom: parentObject to: childObject count: anInteger [

	| parentClass referencesSoFar |
	referencesSoFar := 0.
	parentClass := ReferenceFinderMirror objectClass: parentObject.
	(self finderSkipsClasses not and: [parentClass == childObject]) ifTrue:
		[
			referencesSoFar := referencesSoFar + 1.
			referencesSoFar = anInteger ifTrue: [^'class']
		].
	parentClass == CompiledMethod
		ifTrue:
			[
				1 to: parentObject numLiterals do:
					[:eachIndex |
						| eachLiteral |
						eachLiteral := parentObject literalAt: eachIndex.
						eachLiteral == childObject ifTrue:
							[
								referencesSoFar := referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^'literalAt: ', eachIndex printString]
							]
					]
			]
		ifFalse:
			[
				1 to: parentClass instSize do:
					[:eachIndex |
						| eachVariable |
						eachVariable := ReferenceFinderMirror object: parentObject instVarAt: eachIndex.
						eachVariable == childObject ifTrue:
							[
								referencesSoFar := referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^parentClass allInstVarNames at: eachIndex]
							]
					].
				1 to: (ReferenceFinderMirror objectSize: parentObject) do:
					[:eachIndex |
						| eachSlot |
						eachSlot := ReferenceFinderMirror object: parentObject basicAt: eachIndex.
						eachSlot == childObject ifTrue:
							[
								referencesSoFar := referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^'at: ', eachIndex printString]
							]
					]
			].
	"self halt."
	^'unknown (maybe the reference changed)'
]
