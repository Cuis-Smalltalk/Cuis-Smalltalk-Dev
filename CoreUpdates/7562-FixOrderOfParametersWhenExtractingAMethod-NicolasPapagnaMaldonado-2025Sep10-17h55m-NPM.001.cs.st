'From Cuis7.5 [latest update: #7539] on 10 September 2025 at 6:35:53 pm'!

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'NPM 9/10/2025 18:05:50'!
firstPositionInIntervalToExtractIn: sourceRanges 

	^ (self intervalsToExtractIn: sourceRanges)
		min: [ :sourceRange | sourceRange first ]! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'NPM 9/10/2025 18:05:50'!
intervalsToExtractIn: sourceRanges

	^ sourceRanges select: [ :sourceRange | intervalToExtract includesAllOf: sourceRange ]! !


!ExtractMethodParametersDetector methodsFor: 'evaluating' stamp: 'NPM 9/10/2025 18:20:47'!
value
	
	"Return the parse nodes of the parameters to use in the extracted method.
	
	Implementation details
	--------------
	The enumeration order of the parse nodes is non-deterministic.
	
	To fix this, I keep track of the position for each parameter node to sort
	results in the order they appear in the source code."
	
	| parameterNodePositions |
	
	parameterNodePositions := Dictionary new.
	
	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(self shouldBeParameterized: parseNode appearingIn: sourceRanges)
			ifTrue: [ 
				parameterNodePositions
					at: parseNode
					put: (self firstPositionInIntervalToExtractIn: sourceRanges) ]].
		
	^ parameterNodePositions keys sort: [ :parameterNode :anotherParameterNode |
		(parameterNodePositions at: parameterNode) <= (parameterNodePositions at: anotherParameterNode) ]! !


!ExtractMethodParametersDetector reorganize!
('evaluating' value)
('initialization' initializeFor:at:)
('private' blockNodesEnclosingIntervalToExtract definedInOuterScope: firstPositionInIntervalToExtractIn: intervalToExtractIncludesAnyOf: intervalToExtractIsCoveredByAnyOf: intervalsToExtractIn: isNotExtractedAlongWithItsDeclaration: nodesThatAddVariablesToScope shouldBeParameterized:appearingIn:)
!

