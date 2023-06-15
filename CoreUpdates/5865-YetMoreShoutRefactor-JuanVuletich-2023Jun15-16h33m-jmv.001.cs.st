'From Cuis 6.0 [latest update: #5864] on 15 June 2023 at 4:34:09 pm'!

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/15/2023 16:02:10'!
parseText
	self subclassResponsibility ! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 6/15/2023 16:10:22'!
workspaceContents: text

	formattedText := text.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 6/15/2023 16:10:30'!
convertAndStyleIfNeededWith: anSHTextStyler
	| separator fragmentStart fragmentEnd done|
	(anSHTextStyler notNil and: [self shouldStyle: self actualContents with: anSHTextStyler])
		ifFalse: [^ self].

	self styleByParagraphs ifFalse: [
		^super convertAndStyleIfNeededWith: anSHTextStyler ].

	actualContents := actualContents optimizedForMutationSpeed.	
	anSHTextStyler getReady.
	anSHTextStyler workspaceContents: actualContents.
	anSHTextStyler parseText.
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


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/15/2023 16:02:19'!
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
			self parseText.
			self privateAutoHighlight.
			self privateConvertAssignments.
			self privateStyle.
			textModel changed: #shoutStyled ]! !

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/15/2023 16:02:15'!
convertAndStyleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, by triggering #shoutStyled
	The the model should grab the TextAttributes we added to the copy, as appropriate."

	self mutex critical: [
		"This part runs at low priority, and signals sem when finished"
		backgroundProcess :=  [
			self parseText.
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


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 16:06:10'!
privateConvertAssignments
	"See also CodeProvider class >> #normalizeCodeForCodeFiles:	
	See also Clipboard >> #storeObject:"
	"Turn $_ into a left arrow"

	self replaceStringForRangesWithType: #assignment with: `Character smalltalkLeftArrow asString`.
	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow. Include a #zeroWidthSpace so as not to modify ranges, etc."
			self
				replaceStringForRangesWithType: #ansiAssignment
				with: `Character smalltalkLeftArrow asString, UnicodeCodePoint zeroWidthSpace asString`.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: `Character smalltalkUpArrow asString` ].! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 16:02:29'!
privateStyle

	| alpha end start count startIndexes c hue |
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

!methodRemoval: SHTextStylerST80 #autoHighlightWorkspace: stamp: 'jmv 6/15/2023 16:33:50'!
SHTextStylerST80 removeSelector: #autoHighlightWorkspace:!
