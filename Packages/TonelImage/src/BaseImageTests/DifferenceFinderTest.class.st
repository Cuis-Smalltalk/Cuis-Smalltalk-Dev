Class {
	#name : 'DifferenceFinderTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Tools-Code Differ',
	#package : 'BaseImageTests',
	#tag : 'Tools-Code Differ'
}

{ #category : 'all' }
DifferenceFinderTest >> testCharacters [
	| lcs |
	lcs := DifferenceFinder charactersOf: 'GAC' and: 'AGCAT'.
	lcs compute.
	self
		assert: (lcs lcsAt: 1 at: 1) anyOne size = 0;
		assert: (lcs lcsAt: 1 at: 2) anyOne size = 1;
		assert: (lcs lcsAt: 1 at: 3) anyOne size = 1;
		assert: (lcs lcsAt: 1 at: 4) anyOne size = 1;
		assert: (lcs lcsAt: 1 at: 5) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 1) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 2) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 3) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 4) anyOne size = 2;
		assert: (lcs lcsAt: 2 at: 5) anyOne size = 2;
		assert: (lcs lcsAt: 3 at: 1) anyOne size = 1;
		assert: (lcs lcsAt: 3 at: 2) anyOne size = 1;
		assert: (lcs lcsAt: 3 at: 3) anyOne size = 2;
		assert: (lcs lcsAt: 3 at: 4) anyOne size = 2;
		assert: (lcs lcsAt: 3 at: 5) anyOne size = 2

]

{ #category : 'all' }
DifferenceFinderTest >> testCharacters2 [
	| finder x y |
	finder := DifferenceFinder charactersOf: 'GAC' and: 'AGCAT'.
	finder compute.
	self assert: finder differences size = 3.
	finder differences do: [:diff | 
		x := String streamContents: [:strm | 
			diff
				do: [:char :condition | (#(#removed #unchanged) includes: condition)
					ifTrue: [strm nextPut: char]]].
		self assert: x = 'GAC'.
		y := String streamContents: [:strm | 
			diff
				do: [:char :condition | (#(#inserted #unchanged) includes: condition)
					ifTrue: [strm nextPut: char]]].
		self assert: y = 'AGCAT']

]

{ #category : 'all' }
DifferenceFinderTest >> testCode [
	| old new finder |
	old := 'differencesText
	| change sourceString current diff rtf selectedString |
	change := self selectedMethod ifNil: [self currentClass].
	current := change currentVersion.
	sourceString := current isNil
		ifTrue: ['']
		ifFalse: [current isCompiledMethod ifTrue: [(self
			formatSource: current sourceString
			inClass: current classField)
			sourceCode] ifFalse: [current sourceString]].
	selectedString := (current isNil or: [current isCompiledMethod not])
		ifTrue: [change sourceString]
		ifFalse: [((self
			formatSource: change sourceString
			inClass: current classField) ifNil: [change]) sourceCode].
	diff := TextDiffBuilder from: sourceString to: selectedString.
	rtf := RTFText new setFont: TextFont.
	rtf setTabStops: self tabStops.
	diff run; printPatchSequenceOn: rtf.
	^rtf contents contents'.
	new := 'differencesText
	| change sourceString current finder rtf selectedString |
	change := self selectedMethod ifNil: [self currentClass].
	current := change currentVersion.
	sourceString := current isNil ifTrue: [''] ifFalse: [
		current isCompiledMethod
			ifTrue: [(self
				formatSource: current sourceString
				inClass: current classField)
				sourceCode]
			ifFalse: [current sourceString]].
	selectedString := (current isNil or: [current isCompiledMethod not])
		ifTrue: [change sourceString]
		ifFalse: [((self
			formatSource: change sourceString
			inClass: current classField)
			ifNil: [change])
			sourceCode].
	finder := DifferenceFinder wordsOf: sourceString and: selectedString.
	finder compute.
	rtf := RTFText new setFont: TextFont.
	rtf setTabStops: self tabStops.
	finder differences first printTextOn: rtf.
	^rtf contents contents'.
	finder := DifferenceFinder wordsOf: old and: new.
	finder compute

]

{ #category : 'all' }
DifferenceFinderTest >> testLines [
	| finder alan ian x y |
	alan := 'The best way to
	predict
	the future is to
	invent
	it.
	A. Kay'.
	ian := 'The best way to
	invent
	the future is to
	not predicting
	it.
	Ian Piumarta'.
	finder := DifferenceFinder linesOf: alan and: ian.
	finder compute.
	finder differences do: [:diff | 
		x := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#removed #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: x = alan.
		y := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#inserted #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: y = ian]

]

{ #category : 'all' }
DifferenceFinderTest >> testWords [
	| finder alan ian x y |
	alan := 'The best way to
	predict
	the future is to
	invent
	it.
	A. Kay'.
	ian := 'The best way to
	invent 	the future is to
	not predicting
	it.
	Ian Piumarta'.
	finder := DifferenceFinder wordsOf: alan and: ian.
	finder compute.
	finder differences do: [:diff | 
		x := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#removed #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: x = alan.
		y := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#inserted #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: y = ian]

]
