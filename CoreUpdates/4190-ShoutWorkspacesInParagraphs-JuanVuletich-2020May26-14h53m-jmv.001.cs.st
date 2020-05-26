'From Cuis 5.0 [latest update: #4189] on 26 May 2020 at 2:56:59 pm'!

!Workspace methodsFor: 'user interface support' stamp: 'jmv 5/26/2020 14:55:37'!
formatAndStyleIfNeededWith: anSHTextStyler
	| separator fragmentStart fragmentEnd done|
	(anSHTextStyler notNil and: [self shouldStyle: self actualContents with: anSHTextStyler])
		ifFalse: [^ self].

	actualContents _ actualContents optimizedForMutationSpeed.
	anSHTextStyler formatWorkspace: actualContents.
	separator _ String newLineString, String newLineString.
	fragmentStart _ 1.
	done _ false.
	[done] whileFalse: [
		fragmentEnd _ actualContents
			indexOfSubCollection: separator
			startingAt: fragmentStart
			ifAbsent: [done _ true. actualContents size].
		anSHTextStyler styleWorkspaceFrom: fragmentStart to: fragmentEnd.
		fragmentStart _ fragmentEnd+separator size ].
	self changed: #shoutStyled! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 5/26/2020 14:15:23'!
formatWorkspace: text
	"Do first stage of styling.
	Afterweards, call #styleWorkspaceFrom:to: as needed.
	Note: classOrMetaClass is assumed to be nil"

	self terminateBackgroundStylingProcess.
	formattedText _ text.
	self privateFormatAndConvert.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	
	parser ifNil: [ parser := SHParserST80 new ].
	parser
		workspace: workspace;
		classOrMetaClass: nil! !

!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 5/26/2020 14:23:29'!
styleWorkspaceFrom: start to: end
	"Style a copy of part of the model text. Then apply attributes to model text. Useful for Workspaces.
	Call #formatWorkspace: becore calling us.
	Note: classOrMetaClass is assumed to be nil.
	Note: after last call, do `textModel changed: #shoutStyled`"

	parser
		source: (formattedText asString copyFrom: start to: end).
	parser parse.
	parser ranges ifNotNil: [ :ranges |
		ranges do: [ :range |
			(self attributesFor: range rangeType) ifNotNil: [ :attributes |
				attributes do: [ :each |
					formattedText
						addAttribute: each
						from: range start +start-1
						to: range end +start-1 ]]]].! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 5/26/2020 14:22:37'!
formatAndStyle: text allowBackgroundStyleProcess: aBoolean
	"Do the styling on the model text.
	After finishing, tell model, by triggering #shoutStyled."
	self terminateBackgroundStylingProcess.
	formattedText _ text.
	self privateFormatAndConvert.
	(aBoolean and: [formattedText size > 4096])
		ifTrue: [
			formattedText size < 65536 ifTrue: [
				self styleInBackgroundProcess ]]
		ifFalse: [
			self privateStyle.
			textModel changed: #shoutStyled ]! !


!SHTextStylerST80 reorganize!
('styling' formatWorkspace: styleWorkspaceFrom:to:)
('private' attributesFor: convertAssignmentsToAnsi convertAssignmentsToLeftArrow parseSetWorkspace: privateFormatAndConvert privateStyle replaceStringForRangesWithType:with: setAttributesFromRanges:)
('accessing' classOrMetaClass: disableFormatAndConvert workspace:)
('initialization' initialize)
!

