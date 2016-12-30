'From Cuis 5.0 of 7 November 2016 [latest update: #3019] on 30 December 2016 at 11:46:58 am'!

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 12/30/2016 11:44:19'!
setAttributesFromRanges: ranges

	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].

	"Optimize for mutation speed unless method is really large but with very few distict elements:
	Source code that includes just big literals is better served by conventional Text+RunArray"
	"Do it only if we are not breaking textModel!! (for instance, StyledTextEditor asks for formatting just sections, not whole #actualContents)."
	formattedText == textModel actualContents ifTrue: [
		(formattedText size > 2000 and: [ ranges size < 50 ]) ifFalse: [
			formattedText _ formattedText optimizedForMutationSpeed.
			textModel basicActualContents: formattedText ]].

	ranges do: [ :range |

		"Smalltalk text styling"
		(self attributesFor: range rangeType) ifNotNil: [ :attributes |
			attributes do: [ :each |
				formattedText addAttribute: each from: range start to: range end ]].

		"Show as subscripts if appropriate."
		classOrMetaClass ifNotNil: [
			classOrMetaClass theNonMetaClass lastUnderscoreMeansSubscript ifTrue: [
				(#( instVar classVar globalVar workspaceVar poolConstant
					patternArg methodArg patternTempVar tempVar		
					blockPatternArg blockArg blockPatternTempVar blockTempVar 
					incompleteIdentifier undefinedIdentifier) pointsTo: range rangeType )
						ifTrue: [
							formattedText lastIndexOf: $_ startingAt: range end endingAt: range start do: [ :i |
								formattedText addAttribute: ShoutTextEmphasis subscript from: i to: range end ] ]]]]! !

