'From Cuis 5.0 of 7 November 2016 [latest update: #3179] on 15 September 2017 at 3:38:52 pm'!
!classDefinition: #SHRange category: #'Tools-Syntax Highlighting'!
Object subclass: #SHRange
	instanceVariableNames: 'start end type blockDepth '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!SHParserST80 methodsFor: 'recording ranges' stamp: 'tween 4/28/2004 10:20'!
rangeType: aSymbol start: s end: e 
	^ranges add: (SHRange start: s end: e type: aSymbol)! !


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

!methodRemoval: SHRange #blockDepth!
SHRange removeSelector: #blockDepth!
!methodRemoval: SHRange #blockDepth:!
SHRange removeSelector: #blockDepth:!
!classDefinition: #SHRange category: #'Tools-Syntax Highlighting'!
Object subclass: #SHRange
	instanceVariableNames: 'start end type'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
