'From Cuis 5.0 of 7 November 2016 [latest update: #3168] on 10 September 2017 at 4:45:51 pm'!
!classDefinition: #SHRange category: #'Tools-Syntax Highlighting'!
Object subclass: #SHRange
	instanceVariableNames: 'start end type blockDepth '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!Preferences class methodsFor: 'shout' stamp: 'jmv 9/10/2017 16:40:28'!
highlightBlockNesting
	^ self
		valueOfFlag: #highlightBlockNesting
		ifAbsent: [true]! !


!SHRange methodsFor: 'accessing' stamp: 'jmv 9/6/2017 20:25:10'!
blockDepth
	^blockDepth! !

!SHRange methodsFor: 'accessing' stamp: 'jmv 9/6/2017 20:22:45'!
blockDepth: anInteger
	blockDepth := anInteger! !


!SHParserST80 methodsFor: 'recording ranges' stamp: 'jmv 9/9/2017 15:21:28'!
rangeType: aSymbol start: s end: e 
	^ranges add: ((SHRange start: s end: e type: aSymbol) blockDepth: blockDepth)! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 9/10/2017 16:41:27'!
setAttributesFromRanges: ranges

	| alpha start end |
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].

	"Optimize for mutation speed unless method is really large but with very few distict elements:
	Source code that includes just big literals is better served by conventional Text+RunArray"
	"Do it only if we are not breaking textModel!! (for instance, StyledTextEditor asks for formatting just sections, not whole #actualContents)."
	formattedText == textModel actualContents ifTrue: [
		(formattedText size > 2000 and: [ ranges size < 50 ]) ifFalse: [
			formattedText _ formattedText optimizedForMutationSpeed.
			textModel basicActualContents: formattedText ]].

	ranges withNextDo: [ :range :nextRangeOrNil |

		"Smalltalk text styling"
		(self attributesFor: range rangeType) ifNotNil: [ :attributes |
			attributes do: [ :each |
				formattedText addAttribute: each from: range start to: range end ]].

		Preferences highlightBlockNesting ifTrue: [
			alpha _ range blockDepth / 16.0 min: 0.5.
			start _ range start.
			end _ nextRangeOrNil ifNotNil: [ nextRangeOrNil start - 1 ] ifNil: [ range end ].
			formattedText 
				addAttribute: (ShoutTextBackgroundColor 
				color: (Theme current text alpha: alpha) ) from: start to: end ].

		"Show as subscripts if appropriate."
		classOrMetaClass ifNotNil: [
			classOrMetaClass theNonMetaClass lastUnderscoreMeansSubscript ifTrue: [
				(#( instVar classVar globalVar workspaceVar poolConstant
					patternArg methodArg patternTempVar tempVar		
					blockPatternArg blockArg blockPatternTempVar blockTempVar 
					incompleteIdentifier undefinedIdentifier) pointsTo: range rangeType )
						ifTrue: [
							formattedText lastIndexOf: $_ startingAt: range end endingAt: range start do: [ :i |
								formattedText addAttribute: ShoutTextEmphasis subscript from: i to: range end ] ]]].
				]! !

!classDefinition: #SHRange category: #'Tools-Syntax Highlighting'!
Object subclass: #SHRange
	instanceVariableNames: 'start end type blockDepth'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
