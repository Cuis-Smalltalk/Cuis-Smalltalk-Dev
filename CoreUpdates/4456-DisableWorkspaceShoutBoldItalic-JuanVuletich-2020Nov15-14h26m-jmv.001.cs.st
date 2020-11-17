'From Cuis 5.0 [latest update: #4453] on 15 November 2020 at 2:54:41 pm'!

!TextEmphasis commentStamp: '<historical>' prior: 0!
A TextEmphasis, encodes a characteristic applicable to all fonts.  The encoding is as follows:
	1	bold
	2	italic
	4	underlined		
	8	struck out
	16 	Superscript
	32 	Subscript
	64 	with ST-80 Glyphs!


!TextModel methodsFor: 'user interface support' stamp: 'jmv 11/15/2020 14:54:14'!
allowStylingWithEmphasis
	"Default for Smalltalk methods"

	^true! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 11/15/2020 14:53:54'!
allowStylingWithEmphasis
	"Faster styling of large contents, as text metrics are not affected."

	^false! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 11/15/2020 14:51:24'!
styleWorkspaceFrom: start to: end allowEmphasis: aBoolean
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
					"For Workspaces, avoid attributes that affect text layout: very large contents would be slow."
					(aBoolean or: [each emphasisCode noMask: 3]) ifTrue: [
						formattedText
							addAttribute: each
							from: range start +start-1
							to: range end +start-1 ]]]]].! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 11/15/2020 14:47:17'!
formatAndStyleIfNeededWith: anSHTextStyler
	| separator fragmentStart fragmentEnd done|
	(anSHTextStyler notNil and: [self shouldStyle: self actualContents with: anSHTextStyler])
		ifFalse: [^ self].

	self styleByParagraphs ifFalse: [
		^super formatAndStyleIfNeededWith: anSHTextStyler ].

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
		anSHTextStyler styleWorkspaceFrom: fragmentStart to: fragmentEnd allowEmphasis: self allowStylingWithEmphasis.
		fragmentStart _ fragmentEnd+separator size ].
	self changed: #shoutStyled! !


!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 11/15/2020 14:47:31'!
stylerStyled

	model allowStylingWithEmphasis ifTrue: [
		self textComposition composeAll ].
	self editor recomputeSelection.	
	self updateFromTextComposition.
	self editor blinkParen.
	self scrollSelectionIntoView! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 11/15/2020 09:51:23'!
formatWorkspace: text
	"Do first stage of styling.
	Afterwards, call #styleWorkspaceFrom:to: as needed.
	Note: classOrMetaClass is assumed to be nil"

	self terminateBackgroundStylingProcess.
	formattedText _ text.
	self privateFormatAndConvert.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	
	parser ifNil: [ parser := SHParserST80 new ].
	parser
		workspace: workspace;
		classOrMetaClass: nil! !

!methodRemoval: SHTextStylerST80 #styleWorkspaceFrom:to: stamp: 'jmv 11/15/2020 14:42:26'!
SHTextStylerST80 removeSelector: #styleWorkspaceFrom:to:!
!methodRemoval: Workspace #allowEmphasis stamp: 'jmv 11/15/2020 14:47:24'!
Workspace removeSelector: #allowEmphasis!
