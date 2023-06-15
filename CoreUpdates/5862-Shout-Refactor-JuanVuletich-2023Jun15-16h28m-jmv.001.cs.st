'From Cuis 6.0 [latest update: #5861] on 15 June 2023 at 4:29:09 pm'!

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/15/2023 15:13:05'!
convertAndStyleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, by triggering #shoutStyled
	The the model should grab the TextAttributes we added to the copy, as appropriate."

	self mutex critical: [
		"This part runs at low priority, and signals sem when finished"
		backgroundProcess :=  [
			self privateAutoHighlight.
			self privateConvertAssignments.
			self privateStyle.
			UISupervisor whenUIinSafeState: [
				textModel changed: #shoutStyled ].
			] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]! !

!SHTextStyler methodsFor: 'private' stamp: 'jmv 6/15/2023 14:59:45'!
privateAutoHighlight
	self subclassResponsibility ! !

!SHTextStyler methodsFor: 'private' stamp: 'jmv 6/15/2023 15:01:20'!
privateConvertAssignments
	self subclassResponsibility ! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 6/15/2023 15:22:36'!
autoHighlightWorkspace: text
	"Do first stage of styling.
	Afterwards, call #convertWorkspaceAssignments and
	#styleWorkspaceFrom:to:allowEmphasis: as needed.
	Note: classOrMetaClass is assumed to be nil"

	self terminateBackgroundStylingProcess.
	formattedText := text.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	self privateAutoHighlight.
	
	parser ifNil: [ parser := SHParserST80 new ].
	parser
		workspace: workspace;
		classOrMetaClass: nil.! !

!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 6/15/2023 15:23:30'!
convertWorkspaceAssignments
	"See #privateConvertAssignments"

	self privateConvertAssignments.! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 15:00:04'!
privateAutoHighlight

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
		textModel textProvider justDidAutoHighlight ].! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 15:23:16'!
privateConvertAssignments
	"See also CodeProvider class >> #normalizeCodeForCodeFiles:	
	See also Clipboard >> #storeObject:"
	"Turn $_ into a left arrow"

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


!Workspace methodsFor: 'user interface support' stamp: 'jmv 6/15/2023 15:21:57'!
convertAndStyleIfNeededWith: anSHTextStyler
	| separator fragmentStart fragmentEnd done|
	(anSHTextStyler notNil and: [self shouldStyle: self actualContents with: anSHTextStyler])
		ifFalse: [^ self].

	self styleByParagraphs ifFalse: [
		^super convertAndStyleIfNeededWith: anSHTextStyler ].

	actualContents := actualContents optimizedForMutationSpeed.
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
		anSHTextStyler styleWorkspaceFrom: fragmentStart to: fragmentEnd allowEmphasis: self allowStylingWithEmphasis.
		fragmentStart := fragmentEnd+separator size ].
	self changed: #shoutStyled! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/15/2023 15:10:21'!
convertAndStyle: text allowBackgroundStyleProcess: aBoolean
	"Do the styling on the model text.
	After finishing, tell model, by triggering #shoutStyled."
	self terminateBackgroundStylingProcess.
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


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 6/15/2023 15:22:44'!
styleWorkspaceFrom: start to: end allowEmphasis: aBoolean
	"Style a copy of part of the model text. Then apply attributes to model text. Useful for Workspaces.
	Note: classOrMetaClass is assumed to be nil.
	Note: after last call, do `textModel changed: #shoutStyled`"

	parser
		source: (formattedText asPlainString copyFrom: start to: end).
	parser parse.
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

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 15:06:42'!
optimizeForMutationSpeed: rangesSize

	"Optimize for mutation speed unless method is really large but with very few distict elements:
	Source code that includes just big literals is better served by conventional Text+RunArray"
	"Do it only if we are not breaking textModel!! (for instance, StyledTextEditor asks for formatting just sections, not whole #actualContents)."
	formattedText == textModel actualContents ifTrue: [
		(formattedText size > 2000 and: [ rangesSize < 50 ]) ifFalse: [
			formattedText := formattedText optimizedForMutationSpeed.
			textModel basicActualContents: formattedText ]].

! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 15:06:43'!
setAttributesFromRanges: ranges

	| st80RangeType |
	self optimizeForMutationSpeed: ranges size.
	st80RangeType := SHST80RangeType for: formattedText string in: classOrMetaClass.
	
	ranges do: [ :currentRange |
		self 
			applySmalltalkStylingIn: currentRange using: st80RangeType;
			showAsSubscriptIfAppropriate: currentRange.			
		st80RangeType lastRange: currentRange ]! !

!methodRemoval: SHTextStylerST80 #convertWorkspace: stamp: 'jmv 6/15/2023 16:28:32'!
SHTextStylerST80 removeSelector: #convertWorkspace:!
!methodRemoval: SHTextStylerST80 #privateConvert stamp: 'jmv 6/15/2023 16:28:43'!
SHTextStylerST80 removeSelector: #privateConvert!
!methodRemoval: SHTextStyler #privateConvert stamp: 'jmv 6/15/2023 16:28:40'!
SHTextStyler removeSelector: #privateConvert!
!methodRemoval: SHTextStyler #styleInBackgroundProcess stamp: 'jmv 6/15/2023 16:28:36'!
SHTextStyler removeSelector: #styleInBackgroundProcess!
