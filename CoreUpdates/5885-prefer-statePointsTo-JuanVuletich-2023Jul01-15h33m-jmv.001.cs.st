'From Cuis 6.0 [latest update: #5882] on 1 July 2023 at 3:33:24 pm'!

!Metaclass class methodsFor: 'anti-corruption' stamp: 'jmv 7/1/2023 15:33:13'!
isScarySelector: newbieSelector

	"Return true if newbieSelector is already a part of Metaclass protocol."
	(#(new new: lastUnderscoreMeansSubscript) statePointsTo: newbieSelector) ifTrue: [ ^false ].
	(Metaclass includesSelector: newbieSelector) ifTrue: [^ true].
	(ClassDescription includesSelector: newbieSelector) ifTrue: [^ true].
	(Behavior includesSelector: newbieSelector) ifTrue: [^ true].
	^ false
! !


!SHTextStylerST80 methodsFor: 'private - show subscript' stamp: 'jmv 7/1/2023 15:33:17'!
hasToShowSubscriptOf: range

	^ #(instVar classVar globalVar workspaceVar poolConstant
		patternArg methodArg patternTempVar tempVar		
		blockPatternArg blockArg blockPatternTempVar blockTempVar 
		incompleteIdentifier undefinedIdentifier) statePointsTo: range rangeType! !

