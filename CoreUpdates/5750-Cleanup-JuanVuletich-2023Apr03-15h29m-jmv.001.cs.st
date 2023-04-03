'From Cuis 6.0 [latest update: #5749] on 3 April 2023 at 3:44:29 pm'!

!SHTextStyler commentStamp: 'jmv 4/3/2023 15:30:50' prior: 0!
I am an Abstract class.
Subclasses of me can create formatted, coloured, and styled copies of Text that is given to them.
They may perform their styling asynchronously, in a background process which I create and manage.
	
!


!SHTextStylerST80 commentStamp: 'jmv 4/3/2023 15:31:41' prior: 0!
I style Smalltalk methods and expressions.

My 'styleTable' class instance var holds an array ofArrays which control how each token is styled/coloured. See my defaultStyleTable class method for its structure.
My styleTable can be changed by either modifying the defaultStyleTable class method and then executing SHTextStylerST80 initialize ; or by giving me a new styleTable through my #styleTable: class method.

I use these 2 preferences:
	#syntaxHighlightingAsYouType  - controls whether methods are styled in browsers
	#showAssignmentAsLeftArrow - controls whether assignments are shown in the Smalltalk-80 style
!


!TextModel methodsFor: 'user interface support' stamp: 'jmv 4/3/2023 15:43:30'!
convertAndStyleIfNeededWith: anSHTextStyler
	anSHTextStyler ifNotNil: [
		(self shouldStyle: self actualContents with: anSHTextStyler) ifTrue: [
			anSHTextStyler convertAndStyle: self actualContents allowBackgroundStyleProcess: true.
			self basicActualContents: anSHTextStyler convertedAndStyledText ]]! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 4/3/2023 15:38:17'!
convertAndStyleIfNeededWith: anSHTextStyler
	| separator fragmentStart fragmentEnd done|
	(anSHTextStyler notNil and: [self shouldStyle: self actualContents with: anSHTextStyler])
		ifFalse: [^ self].

	self styleByParagraphs ifFalse: [
		^super convertAndStyleIfNeededWith: anSHTextStyler ].

	actualContents := actualContents optimizedForMutationSpeed.
	anSHTextStyler convertWorkspace: actualContents.
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


!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 4/3/2023 15:39:01'!
convertAndStyleIfNeeded
	"Apply both converting (changes to the characters in the text, such as 	preferred assignment operators),
	and styling (TextAttributes to make Smalltalk code easier to understand)"

	| previousSize delta isAtEnd |
	previousSize := model actualContents size.
	isAtEnd := editor stopIndex > previousSize.
	model convertAndStyleIfNeededWith: styler.
	delta := model actualContents size - previousSize.
	(isAtEnd and: [ delta < 0 ]) ifTrue: [
		delta := 0].
	delta = 0 ifFalse: [
		editor moveCursor: [ :p | p+delta] forward: delta>0 event: nil ].! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 4/3/2023 15:36:17'!
convertAndStyle: text allowBackgroundStyleProcess: aBoolean
	"Do the styling on the model text.
	After finishing, tell model, by triggering #shoutStyled."
	self terminateBackgroundStylingProcess.
	formattedText := text.
	self privateConvert.
	(aBoolean and: [formattedText size > 4096])
		ifTrue: [
			formattedText size < 65536 ifTrue: [
				self styleInBackgroundProcess ]]
		ifFalse: [
			self privateStyle.
			textModel changed: #shoutStyled ]! !

!SHTextStyler methodsFor: 'private' stamp: 'jmv 4/3/2023 15:34:14'!
privateConvert
	self subclassResponsibility ! !

!SHTextStyler methodsFor: 'accessing' stamp: 'jmv 4/3/2023 15:43:28'!
convertedAndStyledText
	^ formattedText! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 4/3/2023 15:37:14'!
convertWorkspace: text
	"Do first stage of styling.
	Afterwards, call #styleWorkspaceFrom:to:allowEmphasis: as needed.
	Note: classOrMetaClass is assumed to be nil"

	self terminateBackgroundStylingProcess.
	formattedText := text.
	self privateConvert.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	
	parser ifNil: [ parser := SHParserST80 new ].
	parser
		workspace: workspace;
		classOrMetaClass: nil! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 4/3/2023 15:34:19'!
privateConvert

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

	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow"
			self
				replaceStringForRangesWithType: #ansiAssignment
				with: (textModel shoutMustKeepContentsSize
					ifTrue: [`Character smalltalkLeftArrow asString, UnicodeCodePoint zeroWidthSpace asString`]
					ifFalse: [`Character smalltalkLeftArrow asString`]).
			"Turn $_ into a left arrow"
			self replaceStringForRangesWithType: #assignment with: `Character smalltalkLeftArrow asString`.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: `Character smalltalkUpArrow asString` ].! !


!SHTextStylerST80 class methodsFor: 'services' stamp: 'jmv 4/3/2023 15:42:26'!
unConvert: aString
	^CodeProvider normalizeCodeForCodeFiles: aString! !


!PluggableTextModel methodsFor: 'testing' stamp: 'jmv 4/3/2023 15:42:28'!
isDirty

	| savedContents |
	textGetter ifNil: [ ^super isDirty ].
	savedContents := textProvider perform: textGetter.
	savedContents = actualContents ifTrue: [ ^false ].
	self textStylerClass ifNotNil: [ :stylerClass |
		"For Smalltalk code, auto conversion of assignment and return arrows doesn't mean dirty."
		savedContents = (stylerClass unConvert: actualContents) ifTrue: [ ^false ]].
	^true! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/3/2023 15:39:07'!
doItProfiling: aBoolean
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"
	| answer |
	answer := self
		evaluateSelectionAndDo: [ :result |
			Transcript finishEntry.
			[result print]
				on: UnhandledError
				do: [:ex | 'printing doIt result failed (remove handler in #doItProfiling: if you prefer the debugger).' print]]
		ifFail: nil
		profiled: aBoolean.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."
	^answer! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/3/2023 15:39:30'!
exploreIt

	self
		evaluateSelectionAndDo: [ :result | result explore ]
		ifFail: [ morph flash ]
		profiled: false.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/3/2023 15:39:36'!
inspectSelectionOrLine

	self
		evaluateSelectionAndDo: [ :result | result inspect ]
		ifFail: [ morph flash ]
		profiled: false.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 4/3/2023 15:39:40'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	
	(aBoolean or: hasUnacceptedEdits) ifTrue: [
		self convertAndStyleIfNeeded ].
	needsFit := aBoolean.
	
	aBoolean == hasUnacceptedEdits ifFalse: [
		hasUnacceptedEdits := aBoolean].
	aBoolean ifFalse: [ hasEditingConflicts := false].
! !


!TextModelMorph methodsFor: 'updating' stamp: 'jmv 4/3/2023 15:39:44'!
updateActualContents 

	"Some day, it would be nice to keep objects and update them
	instead of throwing them away all the time for no good reason..."
	scroller
		releaseEditorAndTextComposition;
		installEditorAndTextComposition;
		convertAndStyleIfNeeded.
	self setScrollDeltas.
	self redrawNeeded.
	^self ! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 4/3/2023 15:35:51'!
styleWorkspaceFrom: start to: end allowEmphasis: aBoolean
	"Style a copy of part of the model text. Then apply attributes to model text. Useful for Workspaces.
	Call #convertWorkspace: becore calling us.
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

!methodRemoval: SHTextStylerST80 class #unFormat: stamp: 'jmv 4/3/2023 15:42:31'!
SHTextStylerST80 class removeSelector: #unFormat:!
!methodRemoval: SHTextStylerST80 #formatWorkspace: stamp: 'jmv 4/3/2023 15:35:56'!
SHTextStylerST80 removeSelector: #formatWorkspace:!
!methodRemoval: SHTextStylerST80 #privateFormatAndConvert stamp: 'jmv 4/3/2023 15:34:49'!
SHTextStylerST80 removeSelector: #privateFormatAndConvert!
!methodRemoval: SHTextStyler #formatAndStyle:allowBackgroundStyleProcess: stamp: 'jmv 4/3/2023 15:36:39'!
SHTextStyler removeSelector: #formatAndStyle:allowBackgroundStyleProcess:!
!methodRemoval: SHTextStyler #privateFormatAndConvert stamp: 'jmv 4/3/2023 15:34:48'!
SHTextStyler removeSelector: #privateFormatAndConvert!
!methodRemoval: SHTextStyler #formattedText stamp: 'jmv 4/3/2023 15:43:36'!
SHTextStyler removeSelector: #formattedText!
!methodRemoval: InnerTextMorph #formatAndStyleIfNeeded stamp: 'jmv 4/3/2023 15:40:01'!
InnerTextMorph removeSelector: #formatAndStyleIfNeeded!
!methodRemoval: SmalltalkEditor #convertAndStyleIfNeededexploreIt stamp: 'jmv 4/3/2023 15:39:24'!
SmalltalkEditor removeSelector: #convertAndStyleIfNeededexploreIt!
!methodRemoval: Workspace #formatAndStyleIfNeededWith: stamp: 'jmv 4/3/2023 15:38:33'!
Workspace removeSelector: #formatAndStyleIfNeededWith:!
!methodRemoval: TextModel #formatAndStyleIfNeededWith: stamp: 'jmv 4/3/2023 15:38:37'!
TextModel removeSelector: #formatAndStyleIfNeededWith:!

!SHTextStylerST80 reorganize!
('styling' convertWorkspace: styleWorkspaceFrom:to:allowEmphasis:)
('private - show subscript' formatAsSubscript: hasToShowSubscriptOf: showAsSubscriptIfAppropriate:)
('private' applySmalltalkStylingIn:using: attributesFor: optimizeForMutationSpeed: parseSetWorkspace: privateConvert privateStyle replaceStringForRangesWithType:with: setAttributesFromRanges:)
('accessing' classOrMetaClass: workspace:)
!

