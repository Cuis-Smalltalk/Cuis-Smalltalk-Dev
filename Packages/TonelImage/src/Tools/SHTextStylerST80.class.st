"
I style Smalltalk methods and expressions.

My 'styleTable' class instance var holds an array ofArrays which control how each token is styled/coloured. See my defaultStyleTable class method for its structure.
My styleTable can be changed by either modifying the defaultStyleTable class method and then executing SHTextStylerST80 initialize ; or by giving me a new styleTable through my #styleTable: class method.

I use these 2 preferences:
	#syntaxHighlightingAsYouType  - controls whether methods are styled in browsers
	#showAssignmentAsLeftArrow - controls whether assignments are shown in the Smalltalk-80 style

"
Class {
	#name : 'SHTextStylerST80',
	#superclass : 'SHTextStyler',
	#instVars : [
		'classOrMetaClass',
		'workspace',
		'parser'
	],
	#classInstVars : [
		'styleTable',
		'textAttributes'
	],
	#category : 'Tools-Syntax Highlighting',
	#package : 'Tools',
	#tag : 'Syntax Highlighting'
}

{ #category : 'style table' }
SHTextStylerST80 class >> attributeArrayForColor: aColorOrNil emphasis: anEmphasisSymbolOrArrayorNil [
	"Answer a new Array containing any non nil TextAttributes specified"
	| answer emphArray |

	answer := #().
	aColorOrNil ifNotNil: [ answer := answer, {ShoutTextColor color: aColorOrNil} ].
	anEmphasisSymbolOrArrayorNil ifNotNil: [
		emphArray := anEmphasisSymbolOrArrayorNil isSymbol 
			ifTrue: [ {anEmphasisSymbolOrArrayorNil} ] 
			ifFalse: [ anEmphasisSymbolOrArrayorNil ].
		emphArray do: [ :each |
			each ~= #normal
				ifTrue: [
					answer := answer, {ShoutTextEmphasis perform: each}]]].
	^answer
]

{ #category : 'style table' }
SHTextStylerST80 class >> attributesFor: aSymbol [

	textAttributes ifNil: [ textAttributes := self initialTextAttributes ].
	^ textAttributes at: aSymbol ifAbsent: nil
]

{ #category : 'style table' }
SHTextStylerST80 class >> defaultStyleTable [
	^ Theme current generateShoutConfig
]

{ #category : 'style table' }
SHTextStylerST80 class >> initialTextAttributes [
	| d element color emphasis attrArray |
	d := IdentityDictionary new.
	self styleTable do: [ :each |
		element := each first.
		color := each at: 2 ifAbsent: nil.
		color := color ifNotNil: [ Color colorFrom: color ].
		emphasis := each at: 3 ifAbsent: nil.
		attrArray := self attributeArrayForColor: color emphasis: emphasis.
		attrArray notEmpty ifTrue: [
			d at: element put: attrArray ]].
	^ d
]

{ #category : 'class initialization' }
SHTextStylerST80 class >> initialize   [
	"Clear styleTable and textAttributesByPixelSize cache so that they will 
	reinitialize.	 

		SHTextStylerST80 initialize
	" 
	
	styleTable := nil.
	textAttributes := nil.	
]

{ #category : 'class initialization' }
SHTextStylerST80 class >> releaseClassCachedState [
	
	styleTable := nil.
	textAttributes := nil
]

{ #category : 'style table' }
SHTextStylerST80 class >> styleTable [
	
	styleTable ifNotNil:[^styleTable].
	^styleTable := self defaultStyleTable.	
						
]

{ #category : 'private' }
SHTextStylerST80 >> applySmalltalkStylingIn: range using: st80RangeType [
 
	| rangeType |
	
	rangeType := st80RangeType ofCurrentRangeOrMessageSendIn: range.
	(self attributesFor: rangeType) ifNotNil: [ :attributes |
		attributes do: [ :each | formattedText addAttribute: each from: range start to: range end ]].

]

{ #category : 'private' }
SHTextStylerST80 >> attributesFor: aSymbol  [
	^self class attributesFor: aSymbol
	
]

{ #category : 'accessing' }
SHTextStylerST80 >> classOrMetaClass: aBehavior [
	classOrMetaClass := aBehavior
]

{ #category : 'private - show subscript' }
SHTextStylerST80 >> formatAsSubscript: range [

	^ formattedText lastIndexOf: $_ startingAt: range end endingAt: range start do: [ :i |
		formattedText addAttribute: ShoutTextEmphasis subscript from: i to: range end ]
]

{ #category : 'styling' }
SHTextStylerST80 >> getReady [
	"Prepare to parse"

	self terminateBackgroundStylingProcess.
	parser ifNil: [ parser := SHParserST80 new ].
	parser
		workspace: workspace;
		classOrMetaClass: classOrMetaClass.
]

{ #category : 'private - show subscript' }
SHTextStylerST80 >> hasToShowSubscriptOf: range [

	^ #(instVar classVar globalVar workspaceVar poolConstant
		patternArg methodArg patternTempVar tempVar		
		blockPatternArg blockArg blockPatternTempVar blockTempVar 
		incompleteIdentifier undefinedIdentifier) statePointsTo: range rangeType
]

{ #category : 'private' }
SHTextStylerST80 >> optimizeForMutationSpeed: rangesSize [

	"Optimize for mutation speed unless method is really large but with very few distict elements:
	Source code that includes just big literals is better served by conventional Text+RunArray"
	"Do it only if we are not breaking textModel! (for instance, StyledTextEditor asks for formatting just sections, not whole #actualContents)."
	| newFormattedText |
	"Not if we are obsoleted because #actualContents changed."
	formattedText == textModel actualContents ifTrue: [
		(formattedText size > 2000 and: [ rangesSize < 50 ]) ifFalse: [
			"What folows could take a little while"
			newFormattedText := formattedText optimizedForMutationSpeed.
			"if already #optimizedForMutationSpeed, do nothing"
			formattedText == newFormattedText ifFalse: [
				"We may be running in a background process... So check again, just in case actualContents changed in the mean time."
				formattedText == textModel actualContents ifTrue: [
					formattedText := newFormattedText.
					textModel basicActualContents: newFormattedText ]]]].


]

{ #category : 'private' }
SHTextStylerST80 >> parseText [
	"Answer a collection of SHRanges by parsing aText."

	parser 	source: formattedText asPlainString.
	parser parse.
	^ parser ranges.
]

{ #category : 'styling' }
SHTextStylerST80 >> parseWorkspaceFrom: start to: end [
	"Run the Shout parser. Useful for Workspaces.
	Note: classOrMetaClass is assumed to be nil.
	Note: after last call, do `textModel changed: #shoutStyled`"

	parser
		source: (formattedText asPlainString copyFrom: start to: end).
	parser parse.
]

{ #category : 'private' }
SHTextStylerST80 >> privateAutoHighlight [

	| attr cs s start found hs |
	textModel shouldAutoHighlight ifTrue: [
		textModel textProvider autoHighlightString ifNotNil: [ :autoHighlight |
			"Message sends. May include, for instance, arguments of keyword selectors."
			attr := `TextBackgroundColor color: (Color red alpha: 0.2)`.
			(textModel textProvider messageSendsRangesOf: autoHighlight) do: [ :interval |
				formattedText 
					addAttribute: attr
					from: interval first
					to: interval last ].
			"Searched text. Senders of symbols, including nested in literal arrays."
			textModel textProvider autoHighlightAllOccurrences ifTrue: [
				cs := Preferences at: #caseSensitiveFinds.
				s := formattedText size.
				hs := autoHighlight size.
				start := 1.
				[start < s] whileTrue: [
					found := formattedText findString: autoHighlight startingAt: start caseSensitive: cs.
					start := s.
					found = 0 ifFalse: [
						formattedText addAttribute: attr from: found to: found+hs-1.
						start := found+hs ]]]
			].
		textModel textProvider justDidAutoHighlight ].
]

{ #category : 'private' }
SHTextStylerST80 >> privateConvertAssignmentsOffset: delta [
	"See also CodeProvider class >> #normalizeCodeForSaving:in:	
	See also Clipboard >> #storeObject:
	See also Workspace >> #contentsForFileSaving"
	"Turn $_ into a left arrow"

	self
		replaceStringForRangesWithType: #assignment
		with: `Character smalltalkLeftArrow asString`
		offset: delta.
	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow. Include a #zeroWidthSpace so as not to modify ranges, etc."
			self
				replaceStringForRangesWithType: #ansiAssignment
				with: `Character smalltalkLeftArrow asString, Character zeroWidthSpace asString`
				offset: delta.
			"Turn $^into up arrow"
			self
				replaceStringForRangesWithType: #return
				with: `Character smalltalkUpArrow asString`
				offset: delta ].
]

{ #category : 'private' }
SHTextStylerST80 >> privateStyle [

	| alpha end start count startIndexes c hue |
	parser ranges ifNotNil: [ :ranges |
		self setAttributesFromRanges: ranges ].

	(Preferences at: #highlightBlockNesting) ifTrue: [
		startIndexes := parser blockDepthsStartIndexes.
		count := startIndexes size.
		parser blockDepths withIndexDo: [ :depth :idx |
			start := startIndexes at: idx.
			end := idx = count ifTrue: [formattedText size] ifFalse: [ (startIndexes at: idx+1)-1].
			alpha := 0.
			depth > 0 ifTrue: [
				"alpha := depth+0.5 / 12.0 min: 0.80."
				alpha := depth sqrt * 0.1 - 0.05 min: 0.5 ].
			hue := depth * 75. 
			c := Color h: hue s: 0.05 v: 0.4 alpha: alpha.
			formattedText 
				addAttribute: (ShoutTextBackgroundColor color: c ) 
				from: start 
				to: end ]].
]

{ #category : 'private' }
SHTextStylerST80 >> replaceStringForRangesWithType: aSymbol with: aString offset: delta [
	"Answer aText if no replacements, or a copy of aText with 
	each range with a type of aSymbol replaced by aString"

	| toReplace increaseInLength |
	toReplace := parser ranges select: [ :each |
		each rangeType = aSymbol ].
	toReplace isEmpty ifTrue: [ ^self ].
	increaseInLength := 0.

	(toReplace asArray sort: [ :a :b | a start <= b start ]) 
		do: [ :each | | end start thisIncrease | 
			start := each start + increaseInLength.
			end := each end + increaseInLength.
			formattedText replaceFrom: start + delta to: end + delta with: aString.
			thisIncrease := aString size - each length.
			increaseInLength := increaseInLength + thisIncrease ]
]

{ #category : 'private' }
SHTextStylerST80 >> setAttributesFromRanges: ranges [

	| st80RangeType |
	self optimizeForMutationSpeed: ranges size.
	st80RangeType := SHST80RangeType for: formattedText string in: classOrMetaClass.
	
	ranges do: [ :currentRange |
		self 
			applySmalltalkStylingIn: currentRange using: st80RangeType;
			showAsSubscriptIfAppropriate: currentRange.			
		st80RangeType lastRange: currentRange ]
]

{ #category : 'private - show subscript' }
SHTextStylerST80 >> showAsSubscriptIfAppropriate: range [

	classOrMetaClass ifNotNil: [
		classOrMetaClass theNonMetaClass lastUnderscoreMeansSubscript ifTrue: [
			(self hasToShowSubscriptOf: range ) ifTrue: [ self formatAsSubscript: range ]]]
]

{ #category : 'styling' }
SHTextStylerST80 >> styleWorkspaceOffset: delta allowEmphasis: aBoolean [
	"Style a copy of part of the model text. Then apply attributes to model text. Useful for Workspaces.
	Note: classOrMetaClass is assumed to be nil.
	Note: after last call, do `textModel changed: #shoutStyled`"

	parser ranges ifNotNil: [ :ranges |
		ranges do: [ :range |
			(self attributesFor: range rangeType) ifNotNil: [ :attributes |
				attributes do: [ :each |
					"For Workspaces, avoid attributes that affect text layout: very large contents would be slow."
					(aBoolean or: [each emphasisCode noMask: 3]) ifTrue: [
						formattedText
							addAttribute: each
							from: range start + delta
							to: range end + delta ]]]]].
]

{ #category : 'accessing' }
SHTextStylerST80 >> workspace: aWorkspace [
	workspace := aWorkspace
]

{ #category : 'styling' }
SHTextStylerST80 >> workspaceContents: text [

	self terminateBackgroundStylingProcess.
	formattedText := text.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
]
