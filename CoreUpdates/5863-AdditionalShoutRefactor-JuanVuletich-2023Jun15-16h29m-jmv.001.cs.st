'From Cuis 6.0 [latest update: #5861] on 15 June 2023 at 4:30:36 pm'!

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/15/2023 15:49:28'!
getReady
	self subclassResponsibility ! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 6/15/2023 15:49:42'!
getReady
	"Prepare to parse"

	self terminateBackgroundStylingProcess.
	parser ifNil: [ parser := SHParserST80 new ].
	parser
		workspace: workspace;
		classOrMetaClass: classOrMetaClass.! !

!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 6/15/2023 15:32:32'!
parseWorkspaceFrom: start to: end
	"Run the Shout parser. Useful for Workspaces.
	Note: classOrMetaClass is assumed to be nil.
	Note: after last call, do `textModel changed: #shoutStyled`"

	parser
		source: (formattedText asPlainString copyFrom: start to: end).
	parser parse.! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 15:44:37'!
parseText
	"Answer a collection of SHRanges by parsing aText."

	parser 	source: formattedText asPlainString.
	parser parse.
	^ parser ranges.! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 6/15/2023 15:49:58'!
convertAndStyleIfNeededWith: anSHTextStyler
	| separator fragmentStart fragmentEnd done|
	(anSHTextStyler notNil and: [self shouldStyle: self actualContents with: anSHTextStyler])
		ifFalse: [^ self].

	self styleByParagraphs ifFalse: [
		^super convertAndStyleIfNeededWith: anSHTextStyler ].

	actualContents := actualContents optimizedForMutationSpeed.	
	anSHTextStyler getReady.
	anSHTextStyler autoHighlightWorkspace: actualContents.
	anSHTextStyler convertWorkspaceAssignments.
	separator := String newLineString, String newLineString.
	fragmentStart := 1.
	done := false.
	[done] whileFalse: [
		fragmentEnd := actualContents
			indexOfSubCollection: separator
			startingAt: fragmentStart
			ifAbsent: [done := true. actualContents size].
		anSHTextStyler parseWorkspaceFrom: fragmentStart to: fragmentEnd.
		anSHTextStyler styleWorkspaceFrom: fragmentStart to: fragmentEnd allowEmphasis: self allowStylingWithEmphasis.
		fragmentStart := fragmentEnd+separator size ].
	self changed: #shoutStyled! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/15/2023 15:49:52'!
convertAndStyle: text allowBackgroundStyleProcess: aBoolean
	"Do the styling on the model text.
	After finishing, tell model, by triggering #shoutStyled."
	self getReady.
	formattedText := text.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	(aBoolean and: [formattedText size > 4096])
		ifTrue: [
			formattedText size < 65536 ifTrue: [
				self convertAndStyleInBackgroundProcess ]]
		ifFalse: [
			self privateAutoHighlight.
			self privateConvertAssignments.
			self privateStyle.
			textModel changed: #shoutStyled ]! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 6/15/2023 15:48:54'!
autoHighlightWorkspace: text
	"Do first stage of styling.
	Afterwards, call #convertWorkspaceAssignments and
	#styleWorkspaceFrom:to:allowEmphasis: as needed.
	Note: classOrMetaClass is assumed to be nil"

	formattedText := text.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	self privateAutoHighlight.! !

!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 6/15/2023 15:33:29'!
styleWorkspaceFrom: start to: end allowEmphasis: aBoolean
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
							from: range start +start-1
							to: range end +start-1 ]]]]].! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 15:54:16'!
privateConvertAssignments
	"See also CodeProvider class >> #normalizeCodeForCodeFiles:	
	See also Clipboard >> #storeObject:"
	"Turn $_ into a left arrow"

	self parseText.
	self replaceStringForRangesWithType: #assignment with: `Character smalltalkLeftArrow asString`.
	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow"
			self
				replaceStringForRangesWithType: #ansiAssignment
				with: (textModel shoutMustKeepContentsSize
					ifTrue: [`Character smalltalkLeftArrow asString, UnicodeCodePoint zeroWidthSpace asString`]
					ifFalse: [`Character smalltalkLeftArrow asString`]).
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: `Character smalltalkUpArrow asString` ].! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 15:47:06'!
privateStyle

	| alpha end start count startIndexes c hue |
	self parseText.
	parser ranges ifNotNil: [ :ranges |
		self setAttributesFromRanges: ranges ].

	(Preferences at: #highlightBlockNesting) ifTrue: [
		startIndexes := parser blockDepthsStartIndexes.
		count := startIndexes size.
		parser blockDepths withIndexDo: [ :depth :idx |
			start := startIndexes at: idx.
			end := idx = count ifTrue: [formattedText size] ifFalse: [ (startIndexes at: idx+1)-1].
			alpha := depth / 15.0 min: 1.0.
			hue := depth * 60.
			c := Color h: hue s: 0.2 v: 0.5 alpha: alpha.
			formattedText 
				addAttribute: (ShoutTextBackgroundColor color: c ) 
				from: start 
				to: end ]]! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 15:54:19'!
replaceStringForRangesWithType: aSymbol with: aString
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
			formattedText replaceFrom: start to: end with: aString.
			thisIncrease := aString size - each length.
			increaseInLength := increaseInLength + thisIncrease ]! !

!methodRemoval: SHTextStylerST80 #parseSetWorkspace: stamp: 'jmv 6/15/2023 16:30:13'!
SHTextStylerST80 removeSelector: #parseSetWorkspace:!
