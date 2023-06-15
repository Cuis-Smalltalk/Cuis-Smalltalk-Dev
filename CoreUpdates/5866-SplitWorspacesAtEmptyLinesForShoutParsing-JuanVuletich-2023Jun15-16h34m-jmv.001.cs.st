'From Cuis 6.0 [latest update: #5864] on 15 June 2023 at 4:36:08 pm'!

!SHTextStyler methodsFor: 'private' stamp: 'jmv 6/15/2023 16:23:12'!
privateConvertAssignmentsOffset: delta
	self subclassResponsibility ! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 6/15/2023 16:17:53'!
styleWorkspaceOffset: delta allowEmphasis: aBoolean
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
							to: range end + delta ]]]]].! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 16:22:59'!
privateConvertAssignmentsOffset: delta
	"See also CodeProvider class >> #normalizeCodeForCodeFiles:	
	See also Clipboard >> #storeObject:"
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
				with: `Character smalltalkLeftArrow asString, UnicodeCodePoint zeroWidthSpace asString`
				offset: delta.
			"Turn $^into up arrow"
			self
				replaceStringForRangesWithType: #return
				with: `Character smalltalkUpArrow asString`
				offset: delta ].! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 16:19:56'!
replaceStringForRangesWithType: aSymbol with: aString offset: delta
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
			increaseInLength := increaseInLength + thisIncrease ]! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 6/15/2023 16:24:50'!
convertAndStyleIfNeededWith: anSHTextStyler
	| separator fragmentStart fragmentEnd done|
	(anSHTextStyler notNil and: [self shouldStyle: self actualContents with: anSHTextStyler])
		ifFalse: [^ self].

	self styleByParagraphs ifFalse: [
		^super convertAndStyleIfNeededWith: anSHTextStyler ].

	actualContents := actualContents optimizedForMutationSpeed.	
	anSHTextStyler getReady.
	anSHTextStyler workspaceContents: actualContents.
	separator := String newLineString, String newLineString.
	fragmentStart := 1.
	done := false.
	[done] whileFalse: [
		fragmentEnd := actualContents
			indexOfSubCollection: separator
			startingAt: fragmentStart
			ifAbsent: [done := true. actualContents size].
		anSHTextStyler parseWorkspaceFrom: fragmentStart to: fragmentEnd.
		anSHTextStyler privateConvertAssignmentsOffset: fragmentStart-1.
		anSHTextStyler styleWorkspaceOffset: fragmentStart-1 allowEmphasis: self allowStylingWithEmphasis.
		fragmentStart := fragmentEnd+separator size ].
	self changed: #shoutStyled! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/15/2023 16:23:24'!
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
			self privateConvertAssignmentsOffset: 0.
			self privateStyle.
			textModel changed: #shoutStyled ]! !

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/15/2023 16:23:29'!
convertAndStyleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, by triggering #shoutStyled
	The the model should grab the TextAttributes we added to the copy, as appropriate."

	self mutex critical: [
		"This part runs at low priority, and signals sem when finished"
		backgroundProcess :=  [
			self parseText.
			self privateAutoHighlight.
			self privateConvertAssignmentsOffset: 0.
			self privateStyle.
			UISupervisor whenUIinSafeState: [
				textModel changed: #shoutStyled ].
			] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]! !

!methodRemoval: SHTextStylerST80 #convertWorkspaceAssignments stamp: 'jmv 6/15/2023 16:34:59'!
SHTextStylerST80 removeSelector: #convertWorkspaceAssignments!
!methodRemoval: SHTextStylerST80 #privateConvertAssignments stamp: 'jmv 6/15/2023 16:35:13'!
SHTextStylerST80 removeSelector: #privateConvertAssignments!
!methodRemoval: SHTextStylerST80 #replaceStringForRangesWithType:with: stamp: 'jmv 6/15/2023 16:35:19'!
SHTextStylerST80 removeSelector: #replaceStringForRangesWithType:with:!
!methodRemoval: SHTextStylerST80 #styleWorkspaceFrom:to:allowEmphasis: stamp: 'jmv 6/15/2023 16:35:21'!
SHTextStylerST80 removeSelector: #styleWorkspaceFrom:to:allowEmphasis:!
!methodRemoval: SHTextStyler #privateConvertAssignments stamp: 'jmv 6/15/2023 16:35:12'!
SHTextStyler removeSelector: #privateConvertAssignments!
