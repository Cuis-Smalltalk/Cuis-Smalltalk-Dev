'From Cuis 4.2 of 25 July 2013 [latest update: #2752] on 2 May 2016 at 10:15:19.396385 am'!

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 5/2/2016 10:12'!
setAttributesFromRanges: ranges

	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	ranges do: [ :range |

		(self attributesFor: range rangeType) ifNotNil: [ :attributes |
			attributes do: [ :each |
				formattedText addAttribute: each from: range start to: range end ]].

		(classOrMetaClass isNil or: [
			classOrMetaClass theNonMetaClass lastUnderscoreMeansSubscript]) ifTrue: [
				(#( instVar classVar globalVar workspaceVar poolConstant
					patternArg methodArg patternTempVar tempVar		
					blockPatternArg blockArg blockPatternTempVar blockTempVar 
					incompleteIdentifier undefinedIdentifier) pointsTo: range rangeType )
						ifTrue: [
							formattedText lastIndexOf: $_ startingAt: range end endingAt: range start do: [ :i |
								formattedText addAttribute: ShoutTextEmphasis subscript from: i to: range end ] ]]]! !

