'From Cuis 4.1 of 12 December 2012 [latest update: #1762] on 17 July 2013 at 10:10:53.56446 am'!
'Description Please enter a description for this package.'!
!provides: 'StyledText' 1 0!
!requires: 'RTFTests' 1 nil!
!requires: 'RTFExporting' 1 nil!
!requires: 'ExtendedClipboard' 1 nil!
!requires: 'RTFImporting' 1 nil!
!classDefinition: #StyleSet category: #StyledText!
ActiveModel subclass: #StyleSet
	instanceVariableNames: 'paragraphStyles characterStyles volatileParaStyles volatileCharStyles'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText'!
!classDefinition: 'StyleSet class' category: #StyledText!
StyleSet class
	instanceVariableNames: ''!

!classDefinition: #StyledTextModel category: #StyledText!
TextModel subclass: #StyledTextModel
	instanceVariableNames: 'styleSet fileName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText'!
!classDefinition: 'StyledTextModel class' category: #StyledText!
StyledTextModel class
	instanceVariableNames: ''!

!classDefinition: #STECompleter category: #'StyledText-Completion'!
AutoCompleter subclass: #STECompleter
	instanceVariableNames: 'words'
	classVariableNames: 'EnglishDict'
	poolDictionaries: ''
	category: 'StyledText-Completion'!
!classDefinition: 'STECompleter class' category: #'StyledText-Completion'!
STECompleter class
	instanceVariableNames: ''!

!classDefinition: #CharacterStyle category: #StyledText!
Object subclass: #CharacterStyle
	instanceVariableNames: 'name familyName pointSize emphasis color kern'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText'!
!classDefinition: 'CharacterStyle class' category: #StyledText!
CharacterStyle class
	instanceVariableNames: ''!

!classDefinition: #ParagraphStyle category: #StyledText!
CharacterStyle subclass: #ParagraphStyle
	instanceVariableNames: 'alignment tabsArray firstIndent restIndent rightIndent spaceBefore spaceAfter listBulletPattern doesShout'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText'!
!classDefinition: 'ParagraphStyle class' category: #StyledText!
ParagraphStyle class
	instanceVariableNames: ''!

!classDefinition: #StyledTextEditor category: #StyledText!
TextEditor subclass: #StyledTextEditor
	instanceVariableNames: 'cmdShortcuts cmdActions'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText'!
!classDefinition: 'StyledTextEditor class' category: #StyledText!
StyledTextEditor class
	instanceVariableNames: ''!

!classDefinition: #FancyDraggeableButtonMorph category: #'StyledText-Morphic-Windows'!
DraggeableButtonMorph subclass: #FancyDraggeableButtonMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Morphic-Windows'!
!classDefinition: 'FancyDraggeableButtonMorph class' category: #'StyledText-Morphic-Windows'!
FancyDraggeableButtonMorph class
	instanceVariableNames: ''!

!classDefinition: #FancyButtonMorph category: #'StyledText-Morphic-Windows'!
PluggableButtonMorph subclass: #FancyButtonMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Morphic-Windows'!
!classDefinition: 'FancyButtonMorph class' category: #'StyledText-Morphic-Windows'!
FancyButtonMorph class
	instanceVariableNames: ''!

!classDefinition: #PluggableDropDownListMorph category: #'StyledText-Morphic-Windows'!
PluggableMorph subclass: #PluggableDropDownListMorph
	instanceVariableNames: 'listMorph getListSelector getIndexSelector setIndexSelector label downButton'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Morphic-Windows'!
!classDefinition: 'PluggableDropDownListMorph class' category: #'StyledText-Morphic-Windows'!
PluggableDropDownListMorph class
	instanceVariableNames: ''!

!classDefinition: #PluggableFilteringDropDownListMorph category: #'StyledText-Morphic-Windows'!
PluggableDropDownListMorph subclass: #PluggableFilteringDropDownListMorph
	instanceVariableNames: 'editorMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Morphic-Windows'!
!classDefinition: 'PluggableFilteringDropDownListMorph class' category: #'StyledText-Morphic-Windows'!
PluggableFilteringDropDownListMorph class
	instanceVariableNames: ''!

!classDefinition: #PluggableActOnReturnKeyListMorph category: #'StyledText-Morphic-Windows'!
PluggableListMorph subclass: #PluggableActOnReturnKeyListMorph
	instanceVariableNames: 'currentIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Morphic-Windows'!
!classDefinition: 'PluggableActOnReturnKeyListMorph class' category: #'StyledText-Morphic-Windows'!
PluggableActOnReturnKeyListMorph class
	instanceVariableNames: ''!

!classDefinition: #PluggableStyledTextMorph category: #'StyledText-Morphic-Windows'!
TextModelMorph subclass: #PluggableStyledTextMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Morphic-Windows'!
!classDefinition: 'PluggableStyledTextMorph class' category: #'StyledText-Morphic-Windows'!
PluggableStyledTextMorph class
	instanceVariableNames: ''!

!classDefinition: #FancyScrollBar category: #'StyledText-Morphic-Windows'!
ScrollBar subclass: #FancyScrollBar
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Morphic-Windows'!
!classDefinition: 'FancyScrollBar class' category: #'StyledText-Morphic-Windows'!
FancyScrollBar class
	instanceVariableNames: ''!

!classDefinition: #STEMainMorph category: #'StyledText-Morphic'!
LayoutMorph subclass: #STEMainMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Morphic'!
!classDefinition: 'STEMainMorph class' category: #'StyledText-Morphic'!
STEMainMorph class
	instanceVariableNames: ''!

!classDefinition: #ToolbarMorph category: #'StyledText-Morphic-Windows'!
LayoutMorph subclass: #ToolbarMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Morphic-Windows'!
!classDefinition: 'ToolbarMorph class' category: #'StyledText-Morphic-Windows'!
ToolbarMorph class
	instanceVariableNames: ''!

!classDefinition: #FilteringDDLEditorMorph category: #'StyledText-Morphic-Windows'!
OneLineEditorMorph subclass: #FilteringDDLEditorMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Morphic-Windows'!
!classDefinition: 'FilteringDDLEditorMorph class' category: #'StyledText-Morphic-Windows'!
FilteringDDLEditorMorph class
	instanceVariableNames: ''!

!classDefinition: #RTFStyledTextBuilder category: #'StyledText-RTF-importing'!
RTFTextBuilder subclass: #RTFStyledTextBuilder
	instanceVariableNames: 'paragraphStyleInUse characterStyleInUse'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-RTF-importing'!
!classDefinition: 'RTFStyledTextBuilder class' category: #'StyledText-RTF-importing'!
RTFStyledTextBuilder class
	instanceVariableNames: ''!

!classDefinition: #SampleListModel category: #'StyledText-Tests'!
Object subclass: #SampleListModel
	instanceVariableNames: 'sel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Tests'!
!classDefinition: 'SampleListModel class' category: #'StyledText-Tests'!
SampleListModel class
	instanceVariableNames: ''!

!classDefinition: #StyledTextBuilder category: #StyledText!
Object subclass: #StyledTextBuilder
	instanceVariableNames: 'styleDict text characterStyleStack textStream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText'!
!classDefinition: 'StyledTextBuilder class' category: #StyledText!
StyledTextBuilder class
	instanceVariableNames: ''!

!classDefinition: #StyledTextEditorTest category: #'StyledText-Tests'!
TestCase subclass: #StyledTextEditorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Tests'!
!classDefinition: 'StyledTextEditorTest class' category: #'StyledText-Tests'!
StyledTextEditorTest class
	instanceVariableNames: ''!

!classDefinition: #StyledTextModelTest category: #'StyledText-Tests'!
TestCase subclass: #StyledTextModelTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Tests'!
!classDefinition: 'StyledTextModelTest class' category: #'StyledText-Tests'!
StyledTextModelTest class
	instanceVariableNames: ''!

!classDefinition: #StyledTextTest category: #'StyledText-Tests'!
TestCase subclass: #StyledTextTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Tests'!
!classDefinition: 'StyledTextTest class' category: #'StyledText-Tests'!
StyledTextTest class
	instanceVariableNames: ''!

!classDefinition: #CharacterStyleReference category: #StyledText!
TextAttribute subclass: #CharacterStyleReference
	instanceVariableNames: 'characterStyle'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText'!
!classDefinition: 'CharacterStyleReference class' category: #StyledText!
CharacterStyleReference class
	instanceVariableNames: ''!

!classDefinition: #ParagraphStyleReference category: #StyledText!
TextAttribute subclass: #ParagraphStyleReference
	instanceVariableNames: 'paragraphStyle'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText'!
!classDefinition: 'ParagraphStyleReference class' category: #StyledText!
ParagraphStyleReference class
	instanceVariableNames: ''!

!classDefinition: #STETheme category: #'StyledText-Themes'!
Theme subclass: #STETheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledText-Themes'!
!classDefinition: 'STETheme class' category: #'StyledText-Themes'!
STETheme class
	instanceVariableNames: ''!


!StyleSet commentStamp: '<historical>' prior: 0!
My instances know some styles (and the keyboard shortcuts to apply them).!

!StyledTextModel commentStamp: 'jmv 3/13/2012 11:36' prior: 0!
A StyledText is a Text where every character has a ParagraphStyle. All the characters in a Paragraph (including the ending Character crCharacter) share the same ParagraphStyle.!

!CharacterStyle commentStamp: '<historical>' prior: 0!
A CharacterStyle comprises a font and color, and can be applied to any part of a Text via a CharacterStyleReference.

Instances are usually shared. They are not modified often, and any change will affect the style's many users.
They should not be copied, but new instances might be created.!

!ParagraphStyle commentStamp: '<historical>' prior: 0!
A ParagraphStyle comprises the formatting information for composing and displaying a unit (usually a paragraph) of text.
ParagraphStyle instances are shared. They are not modified often, and any change will affect the style's many users.
They should not be copied, but new instances might be created.

Each of my instances consists of...
	name						If available for general use in the AvailableParagraphStyles dictionary, it is also the key for access
	font						The default font to use
	alignment					An integer; text alignment, see ParagraphStyle alignment:
	tabsArray					An array of integers giving tab offsets in pixels
	color						Color of the text
	firstIndent					Left indent (margin) of the first line of a paragraph
	restIndent					Left indent (margin) for the rest of the paragraph
	rightIndent					Right indent (margin) of the paragraph
	paragraphSpacingBefore	Additional vertical spacing to add before a paragraph
	paragraphSpacingAfter		Additional verital spacing to add after a paragraph
	listBulletPattern			String pattern for bulleted lists. See the comment at #privateListBulletPattern:
!

!FancyDraggeableButtonMorph commentStamp: '<historical>' prior: 0!
for STE scrollbars!

!FancyButtonMorph commentStamp: '<historical>' prior: 0!
For STE drop down list and fancy scrollbars!

!PluggableDropDownListMorph commentStamp: '<historical>' prior: 0!
A widget that shows the current value, and can open the full list for user selection.!

!PluggableFilteringDropDownListMorph commentStamp: '<historical>' prior: 0!
A DropDownList that allows typing in, to filter visible items in the list.!

!PluggableStyledTextMorph commentStamp: '<historical>' prior: 0!
To be used with StyledTextEditors!

!FancyScrollBar commentStamp: '<historical>' prior: 0!
A Fancy Scrollbar for STE.!

!RTFStyledTextBuilder commentStamp: '<historical>' prior: 0!
Builds styled text!

!SampleListModel commentStamp: 'jmv 9/17/2009 09:46' prior: 0!
(PluggableDropDownListMorph 
	model: SampleListModel new 
	listGetter: #list 
	indexGetter: #sel 
	indexSetter: #sel:) openInWorld!

!StyledTextBuilder commentStamp: 'jmv 1/25/2011 14:28' prior: 0!
A StyledTextBuilder helps building StyledText instances (i.e. Texts whose only attributes are ParagraphStyles and CharacterStyles) in a convenient way with Smalltalk syntax. See class methods, class references and senders for examples.

Advantages of this approach:
- Text creation code is easy to read and write
- No need to explicitly reference StyledTextBuilder
- No need to add foreign protocol to String, Text or Array
- Supports nesting of character styles
- Takes advantage of Shout to make it easier to see the text and the format!

!CharacterStyleReference commentStamp: '<historical>' prior: 0!
A CharacterStyleReference encodes a CharacterStyle change applicable over a given range of text. Instances of CharacterStyleReference (and other TextAttributes) are usually volatile, and they are usually referenced only from the Text.!

!ParagraphStyleReference commentStamp: 'jmv 3/13/2012 11:45' prior: 0!
A ParagraphStyleReference encodes a ParagraphStyle change applicable over a given range of text. Instances of CharacterStyleReference (and other TextAttributes) are usually volatile, and they are usually referenced only from the Text.

Warning: TextAlignment and ParagraphStyleReference should always be applied to whole 'paragraphs' in the text. See #isParagraphAttribute

(
(Text string: 'THIS CLASS HAS NO COMMENT!!', String cStringr attribute: TextColor green),
(Text string: ('Heading of Level 1', String crString) attribute: (ParagraphStyleReference named: 'Heading 1')),
(Text string: ('Heading of Level 2', String crString) attribute: (ParagraphStyleReference named: 'Heading 2')),
(Text string: ('Heading of Level 3', String crString) attribute: (ParagraphStyleReference named: 'Heading 3')),
(Text string: ('This is some emphasized text. ', String crString) attribute: (ParagraphStyleReference named: 'Emphasized')),
(Text string: ('This is some normal text. ', String crString) attribute: (ParagraphStyleReference named: 'Normal')),
(Text string: ('This is some normal text. ', String crString) attribute: (ParagraphStyleReference named: 'Normal')),
(Text string: 'This text has no tyle set')
) edit

This example shows how attributes such as ParagraphStyleReference, that must be applied to whole paragraphs, are indeed done so when concatenating Texts
(
(Text string: 'This text has no style set', String crString),
(Text string: 'This is Heading 1', String crString attribute: (ParagraphStyleReference named: 'Heading 1')),
(Text string: 'no tyle set'),
(Text string: 'This is Heading 1', String crString attribute: (ParagraphStyleReference named: 'Heading 1')),
(Text string: 'no style set'),
(Text string: 'This is Heading 2', String crString attribute: (ParagraphStyleReference named: 'Heading 2')),
(Text string: 'This text has no tsyle set', String cStringr)
) edit!

!Text methodsFor: '*styledText' stamp: 'jmv 4/11/2011 19:37'!
asNonStyledText
	self usesAnyStyles ifFalse: [ ^self ].
	^self copy beNonStyledText! !

!Text methodsFor: '*styledText' stamp: 'jmv 8/10/2011 15:48'!
asStyledTextWith: aStyleSet
	"Next line is disabled, because even if already styled, we need to compatibilize styles with aStyleSet"
"	self isStyledText ifTrue: [ ^self ]."
	^self copy beStyledTextWith: aStyleSet! !

!Text methodsFor: '*styledText' stamp: 'jmv 4/11/2011 18:28'!
beNonStyledText
	"Modify the receiver so that it doesn't include any ParagraphStyle or CharacterStyle.
	Turn them into TextFontFamilyAndSize, TextEmphasis, TextColor, and TextAlignment as appropriate.
	Useful as an option when copying StyledText to the clipboard, as not all RTF editor might support the notion of style."

	runs mapValues: [ :attributes |
		Array streamContents: [ :strm |
			self withFormattingAttributeValues: attributes do: [
					:familyName :pointSize :emphasis :kern :color :alignment :characterStyle :paragraphStyle |
				familyName ifNotNil: [
					strm nextPut: (TextFontFamilyAndSize familyName: familyName pointSize: pointSize) ].	
				emphasis = 0 ifFalse: [
					strm nextPut: (TextEmphasis new emphasisCode: emphasis) ].
				color ifNotNil: [
					strm nextPut: (TextColor color: color) ].
				alignment = 0 ifFalse: [
					strm nextPut: (TextAlignment new alignment: alignment) ].
				kern = 0 ifFalse: [
					strm nextPut: (TextKern new kern: kern) ]].
			attributes do: [ :attribute |
				attribute isForFormatting ifFalse: [
					strm nextPut: attribute ]]
		]
	].
	runs coalesce! !

!Text methodsFor: '*styledText' stamp: 'jmv 3/14/2012 08:30'!
beStyledTextWith: aStyleSet
	"Modify the receiver so that it doesn't include any TextFontFamilyAndSize, TextEmphasis, TextColor, TextAlignment, TextKern. Turn all of them into ParagraphStyleReference or CharacterStyleReference as appropriate.
	Keep any TextAttribute that doesn't fit into the Styles: TextAction (and subclasses) and TextAnchor.
	Create new ParagraphStyle and CharacterStyle as needed."
	
	"1) break runs so that each Cr char is at the end of a run"
	| s prevParagraphEnd paragraphEnd newRunSegments thisParagraphStyle thisCharStyle newRunArray nonFormattingAttributes originalParagraphStyle |
	prevParagraphEnd _ 0.
	s _ self size.
	s = 0 ifTrue: [ ^self ].

	newRunSegments _ OrderedCollection new.
	[ prevParagraphEnd < s ] whileTrue: [
		paragraphEnd _ string indexOf: Character newLineCharacter startingAt: prevParagraphEnd+1 ifAbsent: [ s ].
		newRunSegments add: (runs copyFrom: prevParagraphEnd+1 to: paragraphEnd).
		prevParagraphEnd _ paragraphEnd ].
	
	"2) For each Cr, create a a Paragraph attribute for the current attributes at it"
	
	"3) For each run, take the Paragraph attribute.
	a) If it ends with a Cr, it is that one. Just replace all the relevant attributes with the ParagraphAttribute.
	b) If it doesn't end with a Cr, use the Paragraph attribute from the next run. Build a CharAttr.
		Replace the relevant attributes with the ParaAtt + the CharAtt"

	newRunArray _ nil.
	newRunSegments do: [ :runArray |
		self
			withFormattingAttributeValues: runArray last 
			do: [ :familyName :pointSize :emphasis :kern :color :alignment :characterStyle :paragraphStyle |
				originalParagraphStyle _ paragraphStyle.
				originalParagraphStyle ifNil: [
					originalParagraphStyle _ familyName
						ifNil: [ (aStyleSet defaultStyle) ]
						ifNotNil: [ ParagraphStyle new
							privateFamilyName: familyName pointSize: pointSize emphasis: emphasis color: color alignment: alignment ].
					kern = 0 ifFalse: [
						originalParagraphStyle privateKern: kern ]]].

		thisParagraphStyle _ aStyleSet equivalentOrSameNamePSTo: originalParagraphStyle orAddVolatile: 'Imported '.
		"thisParagraphStyle _ aStyleSet equivalentPSTo: thisParagraphStyle orAddVolatile: 'Imported '."

		runArray mapValues: [ :attributes |
			nonFormattingAttributes _ attributes reject: [ :attr | attr isForFormatting ].
			attributes = nonFormattingAttributes
				ifTrue: [ { ParagraphStyleReference for: thisParagraphStyle }, nonFormattingAttributes ]					"If only attribute is, for example, a TextAnchor (to a Form), then just add paragraph style"
				ifFalse: [
					self withFormattingAttributeValues: attributes do: [ :familyName :pointSize :emphasis :kern :color :alignment :characterStyle :paragraphStyle |
						thisCharStyle _ characterStyle.
						(thisCharStyle isNil and: [ familyName = originalParagraphStyle familyName and: [ pointSize = originalParagraphStyle pointSize and: [ 
								emphasis = originalParagraphStyle emphasis and: [ color = originalParagraphStyle color and: [kern = originalParagraphStyle kern ]]]]])
							ifTrue: [ { ParagraphStyleReference for: thisParagraphStyle }, nonFormattingAttributes ]
							ifFalse: [
								thisCharStyle ifNil: [
									thisCharStyle _ CharacterStyle new
										privateFamilyName: familyName pointSize: pointSize emphasis: emphasis color: color.
									kern = 0 ifFalse: [
										thisCharStyle privateKern: kern ]].

								thisCharStyle _ aStyleSet equivalentOrSameNameCSTo: thisCharStyle orAddVolatile: 'Imported '.
								"thisCharStyle _ aStyleSet equivalentCSTo: thisCharStyle orAddVolatile: 'Imported '."

								{ ParagraphStyleReference for: thisParagraphStyle.
								CharacterStyleReference for: thisCharStyle },
								nonFormattingAttributes ]]]].
			newRunArray _ newRunArray ifNil: [ runArray ] ifNotNil: [ newRunArray, runArray ]].
	self privateSetRuns: newRunArray.
	runs coalesce! !

!Text methodsFor: '*styledText' stamp: 'jmv 9/21/2011 11:00'!
commandForRemoveCharacterStylesIn: anInterval
	"Remove any CharacterStyles in anInterval."

	| start stop new old attributesToRemove |
	start _ anInterval first.
	stop _ anInterval last.
	
	attributesToRemove _ Set new.
	self attributesFrom: anInterval first to: anInterval last do: [ :attribute |
		attribute class == CharacterStyleReference ifTrue: [
			attributesToRemove add: attribute ]].

	old _ runs copyFrom: start to: stop.
	new _ old copy mapValues: [ :attributes | 
			attributes copyWithoutAll: attributesToRemove].
	^AttributesReplaceCommand
		old: old
		new: new
		start: start
		stop: stop! !

!Text methodsFor: '*styledText' stamp: 'jmv 4/11/2011 22:08'!
isStyledText
	runs do: [ :run |
		(run anySatisfy: [ :attribute | attribute class = ParagraphStyleReference])
			ifFalse: [ ^false ].
		run do: [ :attribute |
			(attribute isForFormatting and: [ attribute isStyle not ]) ifTrue: [ ^false ]]].
	^true! !

!Text methodsFor: '*styledText' stamp: 'jmv 12/29/2011 15:44'!
removeReferencesToCharacterStyle: oldCharacterStyle
	self
		removeAttributes: {(CharacterStyleReference for: oldCharacterStyle)}
		from: 1
		to: self size! !

!Text methodsFor: '*styledText' stamp: 'jmv 11/1/2011 11:30'!
replaceReferencesToStyle: oldParagraphOrCharacterStyle with: newParagraphOrCharacterStyle
	"Both arguments must be of the same kind (either para or char style)"
	runs runsAndValuesDo: [ :count :attributes |
		attributes do: [ :att |
			(att isStyle and: [ att style == oldParagraphOrCharacterStyle ])
				ifTrue: [ att style: newParagraphOrCharacterStyle ]]]! !

!Text methodsFor: '*styledText' stamp: 'jmv 4/11/2011 22:04'!
usesAnyStyles
	runs do: [ :run |
		run do: [ :attribute |
			attribute isStyle ifTrue: [ ^true ]]].
	^false! !

!Text methodsFor: '*styledText' stamp: 'jmv 4/11/2011 22:04'!
usesOnlyStyles
	runs do: [ :run |
		run do: [ :attribute |
			(attribute isForFormatting and: [ attribute isStyle not ]) ifTrue: [ ^false ]]].
	^true! !

!Text class methodsFor: '*styledText' stamp: 'jmv 1/25/2011 16:15'!
buildWithStyles: aDictionary contents: aBlock
	"Builds a StyledText, i.e. a Text whose only attributes are ParagraphStyles and CharacterStyles"
	| builder |
	builder _ StyledTextBuilder new styles: aDictionary.
	aBlock value: builder.
	^builder text! !

!SystemWindow class methodsFor: '*styledText' stamp: 'bp 12/21/2011 10:04'!
editFancierStyledText: aTextModel label: labelString
	| window |
	window _ SystemWindow new model: aTextModel.
	window setLabel: labelString.
	window layoutMorph
		addMorph: (PluggableStyledTextMorph withModel: aTextModel in: LayoutMorph newColumn)
		proportionalHeight: 1.
	^ window openInWorld! !

!SystemWindow class methodsFor: '*styledText' stamp: 'jmv 5/24/2011 08:24'!
editStyledText: aTextModel label: labelString
	| window |
	window _ SystemWindow new model: aTextModel.
	window setLabel: labelString.
	window layoutMorph
		addMorph: ((PluggableStyledTextMorph withModel: aTextModel)
			wrapFlag: true)
		proportionalHeight: 1.
	^ window openInWorld! !

!TextAttribute methodsFor: '*styledText' stamp: 'jmv 11/1/2011 11:32'!
isStyle
	^false! !

!TheWorldMenu methodsFor: '*styledText' stamp: 'jmv 5/24/2011 08:26'!
openFancierStyledTextEditor

	SystemWindow editFancierStyledText: StyledTextModel new label: 'Styled Text Editor'! !

!TheWorldMenu methodsFor: '*styledText' stamp: 'jmv 5/24/2011 08:17'!
openStyledTextEditor

	SystemWindow editStyledText: StyledTextModel new label: 'Styled Text Editor'! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 8/9/2011 11:03'!
autoCompletedStyle
	^self characterStyleNamed: 'Completed Text'! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 12/19/2011 14:05'!
characterStyleAt: index
	| i ii |
	index <= characterStyles size ifTrue: [
		^(characterStyles at: index) second ].
	volatileParaStyles ifNotNil: [
		i _ index - characterStyles size.
		ii _ 0.
		volatileCharStyles doWithIndex: [ :each :iii |
			each ifNotNil: [
				ii _ ii + 1.
				ii = i ifTrue: [ ^each ]]]].
	^nil! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 12/30/2011 09:51'!
characterStyleForShortcut: aCharacter

	characterStyles do: [ :pair |
		pair first = aCharacter ifTrue: [ ^pair second ]].
	^nil! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 12/20/2011 12:06'!
characterStyleIndexOf: aCharacterStyle
	| index ii |
	index _ characterStyles findFirst: [ :pair | pair second = aCharacterStyle ].
	index = 0 ifFalse: [ ^index ].

	volatileCharStyles ifNotNil: [
		ii _ 0.
		volatileCharStyles doWithIndex: [ :each :iii |
			each ifNotNil: [
				ii _ ii + 1.
				each = aCharacterStyle ifTrue: [ ^characterStyles size + ii ]]]].

	^0! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 8/11/2011 11:37'!
characterStyleNamed: aString

	| style |
	characterStyles ifNotNil: [
		characterStyles do: [ :pair |
			style _ pair second.
			style name = aString ifTrue: [ ^style ]]].
	^nil! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 8/11/2011 10:03'!
characterStyleNamedOrNew: aString

	^(self characterStyleNamed: aString) ifNil: [
		CharacterStyle new privateName: aString ]! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 12/19/2011 12:33'!
characterStyleNamesAndShortcuts
	^Array streamContents: [ :strm |
		characterStyles do: [ :pair |
			strm nextPut: pair second name, ' (', pair first asString, ')' ].
		volatileCharStyles ifNotNil: [
			volatileCharStyles do: [ :styleOrNil |
				styleOrNil ifNotNil: [
					strm nextPut: styleOrNil name ]]]]! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 8/9/2011 14:38'!
characterStyles
	^characterStyles! !

!StyleSet methodsFor: 'initialization examples' stamp: 'bp 12/9/2011 02:26'!
createDocumentationCharacterStyleSet
	"Build one of the many possible sets of Styles. Maybe other methods like this will be added."
	| emphasised className completedText nullStyle |
	emphasised _ self characterStyleNamedOrNew: 'Emphasized'.
	emphasised privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: Color blue.

	className _ self characterStyleNamedOrNew: 'Class Name'.
	className privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: Color magenta.

	completedText _ self characterStyleNamedOrNew: 'Completed Text'.
	completedText privateFamilyName: 'DejaVu' pointSize: 12 emphasis: 0 color: Color green.

	nullStyle _ CharacterStyle nullStyle.

	characterStyles _ {
		{$e. emphasised}.
		{$l. className}.
		{$p. completedText}.
		{$n. nullStyle}
	}.
	
	self triggerEvent: #stylesChanged! !

!StyleSet methodsFor: 'initialization examples' stamp: 'bp 12/9/2011 02:27'!
createDocumentationParagraphStyleSet
	"Build one of the many possible sets of Styles. Maybe other methods like this will be added."
	| figure indent heading1 heading2 heading3 text |
	
	indent _ 10.

	text _ self paragraphStyleNamedOrNew: 'Text'.
	text privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: indent + 20 restIndent: indent rightIndent: indent
		spaceBefore: 2 spaceAfter: 2.

	figure _ self paragraphStyleNamedOrNew: 'Figure'.
	figure privateFamilyName: 'DejaVu' pointSize: 5 emphasis: 0 color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: indent restIndent: indent rightIndent: indent
		spaceBefore: 0 spaceAfter: 0.
		
	heading1 _ self paragraphStyleNamedOrNew: 'Heading 1'.
	heading1 privateFamilyName: 'DejaVu' pointSize: 22 emphasis: AbstractFont boldCode color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: indent restIndent: indent rightIndent: indent
		spaceBefore: 20 spaceAfter: 10.

	heading2 _ self paragraphStyleNamedOrNew: 'Heading 2'.
	heading2 privateFamilyName: 'DejaVu' pointSize: 17 emphasis: AbstractFont boldCode color: Color red
		alignment: CharacterScanner leftFlushCode firstIndent: indent restIndent: indent rightIndent: indent
		spaceBefore: 24 spaceAfter: 10.

	heading3 _ self paragraphStyleNamedOrNew: 'Heading 3'.
	heading3 privateFamilyName: 'DejaVu' pointSize: 11 emphasis: AbstractFont boldCode color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: indent restIndent: indent rightIndent: indent
		spaceBefore: 22 spaceAfter: 3.

	paragraphStyles _ {
		{$0. text}.
		{$1. heading1}.
		{$2. heading2}.
		{$3. heading3}.
	}.
	
	self triggerEvent: #stylesChanged! !

!StyleSet methodsFor: 'initialization examples' stamp: 'bp 10/29/2011 16:47'!
createDramaCharacterStyleSet
	"Build one of the many possible sets of Styles. Maybe other methods like this will be added."
	| completedText nullStyle |

	completedText _ self characterStyleNamedOrNew: 'Completed Text'.
	completedText privateFamilyName: 'DejaVu' pointSize: 12 emphasis: 0 color: Color green.

	nullStyle _ CharacterStyle nullStyle.

	characterStyles _ {
		{$p.	completedText}.
		{$n.	nullStyle}
	}.
	
	self triggerEvent: #stylesChanged! !

!StyleSet methodsFor: 'initialization examples' stamp: 'bp 10/29/2011 16:47'!
createDramaParagraphStyleSet
	"Build one of the many possible sets of Styles. Maybe other methods like this will be added."
	| figure leftIndent act scene sceneDescription sceneInstruction speaker text |
	
	leftIndent _ 20.

	figure _ self paragraphStyleNamedOrNew: 'Figure'.
	figure privateFamilyName: 'DejaVu' pointSize: 5 emphasis: 0 color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: leftIndent restIndent: leftIndent rightIndent: 10
		spaceBefore: 0 spaceAfter: 0.
		
	act _ self paragraphStyleNamedOrNew: 'Act'.
	act privateFamilyName: 'DejaVu' pointSize: 22 emphasis: AbstractFont boldCode color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: leftIndent restIndent: leftIndent rightIndent: 0
		spaceBefore: 20 spaceAfter: 10.

	scene _ self paragraphStyleNamedOrNew: 'Scene'.
	scene privateFamilyName: 'DejaVu' pointSize: 17 emphasis: AbstractFont boldCode color: Color red
		alignment: CharacterScanner leftFlushCode firstIndent: leftIndent restIndent: leftIndent rightIndent: 0
		spaceBefore: 24 spaceAfter: 10.

	sceneDescription _ self paragraphStyleNamedOrNew: 'Scene Description'.
	sceneDescription privateFamilyName: 'DejaVu' pointSize: 11 emphasis: AbstractFont boldCode color: Color blue
		alignment: CharacterScanner leftFlushCode firstIndent: leftIndent restIndent: leftIndent rightIndent: 10
		spaceBefore: 2 spaceAfter: 2.

	sceneInstruction _ self paragraphStyleNamedOrNew: 'Scene Instruction'.
	sceneInstruction privateFamilyName: 'DejaVu' pointSize: 11 emphasis: AbstractFont italicCode color: Color blue
		alignment: CharacterScanner leftFlushCode firstIndent: leftIndent restIndent: leftIndent rightIndent: 10
		spaceBefore: 2 spaceAfter: 2.

	speaker _ self paragraphStyleNamedOrNew: 'Speaker'.
	speaker privateFamilyName: 'DejaVu' pointSize: 11 emphasis: AbstractFont boldCode color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: leftIndent restIndent: leftIndent rightIndent: 10
		spaceBefore: 22 spaceAfter: 3.

	text _ self paragraphStyleNamedOrNew: 'Text'.
	text privateFamilyName: 'DejaVu' pointSize: 11 emphasis: AbstractFont italicCode color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: leftIndent + 20 restIndent: leftIndent +20 rightIndent: 10
		spaceBefore: 2 spaceAfter: 2.

	paragraphStyles _ {
		{$f. figure}.
		{$a. act}.
		{$s. scene}.
		{$d. sceneDescription}.
		{$i. sceneInstruction}.
		{$k. speaker}.
		{$t. text}.
	}.
	
	self triggerEvent: #stylesChanged! !

!StyleSet methodsFor: 'initialization examples' stamp: 'bp 12/11/2011 01:36'!
createFeaturesCharacterStyleSet
	"Build one of the many possible sets of Styles. Maybe other methods like this will be added."
	| title featureType nullStyle completedText |

	title _ self characterStyleNamedOrNew: 'Title'.
	title privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: Color blue.

	featureType _ self characterStyleNamedOrNew: 'Feature Type'.
	featureType privateFamilyName: 'DejaVu' pointSize: 10 emphasis: AbstractFont boldCode color: Color red.

	completedText _ self characterStyleNamedOrNew: 'Completed Text'.
	completedText privateFamilyName: 'DejaVu' pointSize: 12 emphasis: 0 color: Color green.

	nullStyle _ CharacterStyle nullStyle.

	characterStyles _ {
		{$t. title}.
		{$y. featureType}.
		{$p. completedText}.
		{$n. nullStyle}
	}.
	
	self triggerEvent: #stylesChanged! !

!StyleSet methodsFor: 'initialization examples' stamp: 'bp 10/29/2011 15:41'!
createFeaturesParagraphStyleSet
	"Build one of the many possible sets of Styles. Maybe other methods like this will be added."
	| heading1 heading2 heading3 emphasized normal numbered alphabetic bulleted smalltalkCode  |

	normal _ self paragraphStyleNamedOrNew: 'Normal'.
	normal privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: 30 restIndent: 10 rightIndent: 10
		spaceBefore: 8 spaceAfter: 2.

	emphasized _ self paragraphStyleNamedOrNew: 'Emphasized'.
	emphasized privateFamilyName: 'DejaVu' pointSize: 10 emphasis: AbstractFont boldCode color: nil
		alignment: CharacterScanner justifiedCode firstIndent: 60 restIndent: 60 rightIndent: 60
		spaceBefore: 10 spaceAfter: 2.

	heading1 _ self paragraphStyleNamedOrNew: 'Heading 1'.
	heading1 privateFamilyName: 'DejaVu' pointSize: 22 emphasis: 0 color: nil
		alignment: CharacterScanner centeredCode firstIndent: 0 restIndent: 0 rightIndent: 0
		spaceBefore: 34 spaceAfter: 18.

	heading2 _ self paragraphStyleNamedOrNew: 'Heading 2'.
	heading2 privateFamilyName: 'DejaVu' pointSize: 17 emphasis: AbstractFont boldCode color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: 0 restIndent: 0 rightIndent: 0
		spaceBefore: 24 spaceAfter: 8.

	heading3 _ self paragraphStyleNamedOrNew: 'Heading 3'.
	heading3 privateFamilyName: 'DejaVu' pointSize: 14 emphasis: AbstractFont italicCode color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: 0 restIndent: 0 rightIndent: 0
		spaceBefore: 18 spaceAfter: 4.

	numbered _ self paragraphStyleNamedOrNew: 'Numbered List'.
	numbered privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: nil
		alignment: CharacterScanner justifiedCode firstIndent: 10 restIndent: 50 rightIndent: 10
		spaceBefore: 8 spaceAfter: 2;
			privateListBulletPattern: '%%%. '.

	alphabetic _ self paragraphStyleNamedOrNew: 'Alphabetic List'.
	alphabetic privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: nil
		alignment: CharacterScanner justifiedCode firstIndent: 10 restIndent: 30 rightIndent: 10
		spaceBefore: 8 spaceAfter: 2;
			privateListBulletPattern: 'z) '.

	bulleted _ self paragraphStyleNamedOrNew: 'Bulleted List'.
	bulleted privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: nil
		alignment: CharacterScanner justifiedCode firstIndent: 10 restIndent: 30 rightIndent: 10
		spaceBefore: 8 spaceAfter: 2;
			privateListBulletPattern: '° '.

	smalltalkCode _ self paragraphStyleNamedOrNew: 'Smalltalk code'.
	smalltalkCode privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: 10 restIndent: 10 rightIndent: 10
		spaceBefore: 0 spaceAfter: 0;
			doShout.

	paragraphStyles _ {
		{$0.	normal}.
		{$e.	emphasized}.
		{$1.	heading1}.
		{$2.	heading2}.
		{$3.	heading3}.
		{$4.	numbered}.
		{$5.	alphabetic}.
		{$6.	bulleted}.
		{$7.	smalltalkCode}
	}.
	
	self triggerEvent: #stylesChanged! !

!StyleSet methodsFor: 'initialization examples' stamp: 'jmv 8/11/2011 11:00'!
createSampleCharacterStyleSet
	"Build one of the many possible sets of Styles. Maybe other methods like this will be added."
	| green11Italic green14 red10Bold nullStyle completedText |

	green11Italic _ self characterStyleNamedOrNew: 'Green 11 Italic'.
	green11Italic privateFamilyName: 'DejaVu' pointSize: 11 emphasis: AbstractFont italicCode color: Color green.

	green14 _ self characterStyleNamedOrNew: 'Green 14'.
	green14 privateFamilyName: 'DejaVu' pointSize: 14 emphasis: 0 color: Color green.

	red10Bold _ self characterStyleNamedOrNew: 'Red 10 bold'.
	red10Bold privateFamilyName: 'DejaVu' pointSize: 10 emphasis: AbstractFont boldCode color: Color red.

	completedText _ self characterStyleNamedOrNew: 'Completed Text'.
	completedText privateFamilyName: 'DejaVu' pointSize: 12 emphasis: 0 color: Color green.

	nullStyle _ CharacterStyle nullStyle.

	characterStyles _ {
		{$u.	green11Italic}.
		{$i. 	green14}.
		{$o.	red10Bold}.
		{$p.	completedText}.
		{$n.	nullStyle}
	}.
	
	self triggerEvent: #stylesChanged! !

!StyleSet methodsFor: 'initialization examples' stamp: 'jmv 8/11/2011 11:00'!
createSampleParagraphStyleSet
	"Build one of the many possible sets of Styles. Maybe other methods like this will be added."
	| heading1 heading2 heading3 emphasized normal numbered alphabetic bulleted smalltalkCode  |

	normal _ self paragraphStyleNamedOrNew: 'Normal'.
	normal privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: nil
		alignment: CharacterScanner justifiedCode firstIndent: 30 restIndent: 10 rightIndent: 10
		spaceBefore: 8 spaceAfter: 2.

	emphasized _ self paragraphStyleNamedOrNew: 'Emphasized'.
	emphasized privateFamilyName: 'DejaVu' pointSize: 10 emphasis: AbstractFont boldCode color: nil
		alignment: CharacterScanner justifiedCode firstIndent: 60 restIndent: 60 rightIndent: 60
		spaceBefore: 10 spaceAfter: 2.

	heading1 _ self paragraphStyleNamedOrNew: 'Heading 1'.
	heading1 privateFamilyName: 'DejaVu' pointSize: 22 emphasis: 0 color: nil
		alignment: CharacterScanner centeredCode firstIndent: 0 restIndent: 0 rightIndent: 0
		spaceBefore: 34 spaceAfter: 18.

	heading2 _ self paragraphStyleNamedOrNew: 'Heading 2'.
	heading2 privateFamilyName: 'DejaVu' pointSize: 17 emphasis: AbstractFont boldCode color: nil
		alignment: CharacterScanner centeredCode firstIndent: 0 restIndent: 0 rightIndent: 0
		spaceBefore: 24 spaceAfter: 8.

	heading3 _ self paragraphStyleNamedOrNew: 'Heading 3'.
	heading3 privateFamilyName: 'DejaVu' pointSize: 14 emphasis: AbstractFont italicCode color: nil
		alignment: CharacterScanner centeredCode firstIndent: 0 restIndent: 0 rightIndent: 0
		spaceBefore: 18 spaceAfter: 4.

	numbered _ self paragraphStyleNamedOrNew: 'Numbered List'.
	numbered privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: nil
		alignment: CharacterScanner justifiedCode firstIndent: 10 restIndent: 50 rightIndent: 10
		spaceBefore: 8 spaceAfter: 2;
			privateListBulletPattern: '%%%. '.

	alphabetic _ self paragraphStyleNamedOrNew: 'Alphabetic List'.
	alphabetic privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: nil
		alignment: CharacterScanner justifiedCode firstIndent: 10 restIndent: 30 rightIndent: 10
		spaceBefore: 8 spaceAfter: 2;
			privateListBulletPattern: 'z) '.

	bulleted _ self paragraphStyleNamedOrNew: 'Bulleted List'.
	bulleted privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: nil
		alignment: CharacterScanner justifiedCode firstIndent: 10 restIndent: 30 rightIndent: 10
		spaceBefore: 8 spaceAfter: 2;
			privateListBulletPattern: '° '.

	smalltalkCode _ self paragraphStyleNamedOrNew: 'Smalltalk code'.
	smalltalkCode privateFamilyName: 'DejaVu' pointSize: 11 emphasis: 0 color: nil
		alignment: CharacterScanner leftFlushCode firstIndent: 10 restIndent: 10 rightIndent: 10
		spaceBefore: 0 spaceAfter: 0;
			doShout.

	paragraphStyles _ {
		{$0.	normal}.
		{$e.	emphasized}.
		{$1.	heading1}.
		{$2.	heading2}.
		{$3.	heading3}.
		{$4.	numbered}.
		{$5.	alphabetic}.
		{$6.	bulleted}.
		{$7.	smalltalkCode}
	}.
	
	self triggerEvent: #stylesChanged! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 1/13/2012 14:05'!
defaultStyle
	"Usually we include a 'Normal' style. If not, answer some style anyway.
	We might refine this!!"
	^(self paragraphStyleNamed: 'Normal') ifNil: [ self paragraphStyleAt: 1 ]! !

!StyleSet methodsFor: 'special' stamp: 'jmv 12/19/2011 12:38'!
equivalentCSTo: aCharacterStyle orAddVolatile: namePrefix
	"Do not match existing style by name: add as new style to set"

	| count nameToUse possibleName newCharacterStyle i |
	characterStyles do: [ :pair |
		(pair second isEquivalentTo: aCharacterStyle) ifTrue: [ ^ pair second ]].

	possibleName _ aCharacterStyle name isEmpty
		ifFalse: [ namePrefix, aCharacterStyle name ]
		ifTrue: [ namePrefix, aCharacterStyle shortDescription ].
	volatileCharStyles
		ifNil: [
			newCharacterStyle _ aCharacterStyle copy.
			newCharacterStyle privateName: possibleName.
			volatileCharStyles _ WeakArray with: newCharacterStyle ]
		ifNotNil: [
			volatileCharStyles do: [ :cs | (cs notNil and: [ cs isEquivalentTo: aCharacterStyle]) ifTrue: [ ^cs ]].
			newCharacterStyle _ aCharacterStyle copy.
			nameToUse _ possibleName.
			count _ 1.
			[ volatileCharStyles anySatisfy: [ :cs | cs notNil and: [ cs name =  nameToUse ]]] whileTrue: [
				count _ count +1.
				nameToUse _ possibleName, ' ', count printString ].
			newCharacterStyle privateName: nameToUse.
			i _ volatileCharStyles indexOf: nil.
			i = 0
				ifTrue: [ volatileCharStyles _ volatileCharStyles copyWith: newCharacterStyle]
				ifFalse: [ volatileCharStyles at: i put: newCharacterStyle ]].
	^newCharacterStyle! !

!StyleSet methodsFor: 'special' stamp: 'jmv 12/19/2011 10:58'!
equivalentOrSameNameCSTo: aCharacterStyle orAddVolatile: namePrefix

	"Match existing style by name."
	(self characterStyleNamed: aCharacterStyle name) ifNotNil: [ :namedTheSame |
		^ namedTheSame ].

	"Find equivalent, or add."
	^self equivalentCSTo: aCharacterStyle orAddVolatile: namePrefix! !

!StyleSet methodsFor: 'special' stamp: 'jmv 12/19/2011 10:58'!
equivalentOrSameNamePSTo: aParagraphStyle orAddVolatile: namePrefix

	"Match existing style by name."
	(self paragraphStyleNamed: aParagraphStyle name) ifNotNil: [ :namedTheSame |
		^ namedTheSame ].

	"Find equivalent, or add."
	^self equivalentPSTo: aParagraphStyle orAddVolatile: namePrefix! !

!StyleSet methodsFor: 'special' stamp: 'jmv 12/19/2011 12:37'!
equivalentPSTo: aParagraphStyle orAddVolatile: namePrefix
	"Do not match existing style by name: add as new style to set"
	
	| count nameToUse possibleName newParagraphStyle i |
	paragraphStyles do: [ :pair |
		(pair second isEquivalentTo: aParagraphStyle) ifTrue: [ ^ pair second ]].

	possibleName _ aParagraphStyle name isEmpty
		ifFalse: [ namePrefix, aParagraphStyle name ]
		ifTrue: [ namePrefix, aParagraphStyle shortDescription ].
	volatileParaStyles
		ifNil: [
			newParagraphStyle _ aParagraphStyle copy.
			newParagraphStyle privateName: possibleName.
			volatileParaStyles _ WeakArray with: newParagraphStyle ]
		ifNotNil: [
			volatileParaStyles do: [ :ps | (ps notNil and: [ ps isEquivalentTo: aParagraphStyle]) ifTrue: [ ^ps ]].
			newParagraphStyle _ aParagraphStyle copy.
			nameToUse _ possibleName.
			count _ 1.
			[ volatileParaStyles anySatisfy: [ :ps | ps notNil and: [ ps name =  nameToUse ]]] whileTrue: [
				count _ count +1.
				nameToUse _ possibleName, ' ', count printString ].
			newParagraphStyle privateName: nameToUse.
			i _ volatileParaStyles indexOf: nil.
			i = 0
				ifTrue: [ volatileParaStyles _ volatileParaStyles copyWith: newParagraphStyle]
				ifFalse: [ volatileParaStyles at: i put: newParagraphStyle ]].
	^newParagraphStyle! !

!StyleSet methodsFor: 'initialization examples' stamp: 'jmv 8/9/2011 15:55'!
initializeEmpty
	paragraphStyles _ #().
	characterStyles _ #()! !

!StyleSet methodsFor: 'initialization examples' stamp: 'jmv 8/11/2011 11:41'!
makeStylesMuchSmaller
	"Just an example

	| model text |
	model _ StyledTextModel new.
	text _ Text string: 'Just a text' attribute: (ParagraphStyleReference for: (model styleSet paragraphStyleNamed: 'Normal')).
	model contents: text.
	model styleSet makeStylesMuchSmaller.
	SystemWindow editFancierStyledText: model label: 'Styled Text Editor'
	"
	paragraphStyles do: [ :pair |
		pair second privatePointSize: 8 ].
	characterStyles do: [ :pair |
		pair second pointSize ifNotNil: [ :ps |
			pair second privatePointSize: 8]].

	self triggerEvent: #stylesChanged! !

!StyleSet methodsFor: 'initialization examples' stamp: 'jmv 8/11/2011 11:41'!
makeStylesSmaller
	"Just an example

	| model text |
	model _ StyledTextModel new.
	text _ Text string: 'Just a text' attribute: (ParagraphStyleReference for: (model styleSet paragraphStyleNamed: 'Normal')).
	model contents: text.
	model styleSet makeStylesSmaller.
	SystemWindow editFancierStyledText: model label: 'Styled Text Editor'
	"
	paragraphStyles do: [ :pair |
		pair second privatePointSize: 10 ].
	characterStyles do: [ :pair |
		pair second pointSize ifNotNil: [ :ps |
			pair second privatePointSize: 10]].

	self triggerEvent: #stylesChanged! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 12/19/2011 14:04'!
paragraphStyleAt: index
	| i ii |
	index <= paragraphStyles size ifTrue: [
		^(paragraphStyles at: index) second ].
	volatileParaStyles ifNotNil: [
		i _ index - paragraphStyles size.
		ii _ 0.
		volatileParaStyles doWithIndex: [ :each :iii |
			each ifNotNil: [
				ii _ ii + 1.
				ii = i ifTrue: [ ^each ]]]].
	^nil! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 12/30/2011 09:53'!
paragraphStyleForShortcut: aCharacter

	paragraphStyles do: [ :pair |
		pair first = aCharacter ifTrue: [ ^pair second ]].
	^nil! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 12/19/2011 14:08'!
paragraphStyleIndexOf: aParagraphStyle
	| index ii |
	index _ paragraphStyles findFirst: [ :pair | pair second = aParagraphStyle ].
	index = 0 ifFalse: [ ^index ].

	volatileParaStyles ifNotNil: [
		ii _ 0.
		volatileParaStyles doWithIndex: [ :each :iii |
			each ifNotNil: [
				ii _ ii + 1.
				each = aParagraphStyle ifTrue: [ ^paragraphStyles size + ii ]]]].

	^0! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 8/11/2011 11:36'!
paragraphStyleNamed: aString

	| style |
	paragraphStyles ifNotNil: [
		paragraphStyles do: [ :pair |
			style _ pair second.
			style name = aString ifTrue: [ ^style ]]].
	^nil! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 8/11/2011 10:02'!
paragraphStyleNamedOrNew: aString

	^(self paragraphStyleNamed: aString) ifNil: [
		ParagraphStyle new privateName: aString ]! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 12/19/2011 12:33'!
paragraphStyleNamesAndShortcuts
	^Array streamContents: [ :strm |
		paragraphStyles do: [ :pair |
			strm nextPut: pair second name, ' (', pair first asString, ')' ].
		volatileParaStyles ifNotNil: [
			volatileParaStyles do: [ :styleOrNil |
				styleOrNil ifNotNil: [
					strm nextPut: styleOrNil name ]]]]! !

!StyleSet methodsFor: 'accessing' stamp: 'jmv 8/9/2011 14:38'!
paragraphStyles
	^paragraphStyles! !

!StyleSet methodsFor: 'private' stamp: 'jmv 12/30/2011 10:38'!
useWeakArrays
	"An aux conversion method for old instances
	StyleSet allInstancesDo: [ :a | a useWeakArrays ]. Smalltalk garbageCollect
	"
	volatileParaStyles class == Array ifTrue: [
		volatileParaStyles _ WeakArray withAll: volatileParaStyles ].
	volatileCharStyles class == Array ifTrue: [
		volatileCharStyles _ WeakArray withAll: volatileCharStyles ].! !

!StyleSet class methodsFor: 'instance creation' stamp: 'jmv 8/9/2011 15:55'!
empty
	^self new initializeEmpty! !

!StyleSet class methodsFor: 'instance creation' stamp: 'bp 12/21/2011 10:21'!
features
	^self new
		createFeaturesParagraphStyleSet;
		createFeaturesCharacterStyleSet! !

!StyleSet class methodsFor: 'instance creation' stamp: 'jmv 12/20/2011 12:37'!
sample
	^self new
		createSampleParagraphStyleSet;
		createSampleCharacterStyleSet! !

!StyledTextModel methodsFor: 'accessing' stamp: 'jmv 8/11/2011 11:27'!
actualContents: aTextOrString
	"Merge styles appropriately. Warning: modifies the argument."
	super actualContents: (aTextOrString ifNotNil: [
		aTextOrString
			asText
			beStyledTextWith: styleSet])! !

!StyledTextModel methodsFor: 'as yet unclassified' stamp: 'jmv 7/14/2011 14:00'!
autoCompleter
	^STECompleter withModel: self! !

!StyledTextModel methodsFor: 'accessing' stamp: 'jmv 8/11/2011 11:27'!
basicActualContents: aTextOrString
	"Merge styles appropriately. Warning: modifies the argument."
	super basicActualContents: (aTextOrString ifNotNil: [
		aTextOrString
			asText
			beStyledTextWith: styleSet])! !

!StyledTextModel methodsFor: 'as yet unclassified' stamp: 'jmv 12/20/2010 14:53'!
editorClass
	^StyledTextEditor! !

!StyledTextModel methodsFor: 'accessing' stamp: 'jmv 4/30/2012 00:08'!
fileName: aString
	fileName _ aString! !

!StyledTextModel methodsFor: 'testing' stamp: 'jmv 12/21/2010 23:22'!
is: aSymbol
	^ aSymbol == #ShoutEnabled or: [ super is: aSymbol ]! !

!StyledTextModel methodsFor: 'undoable commands' stamp: 'jmv 9/22/2011 15:06'!
logUndoAndRemoveCharacterStylesIn: anInterval
	"Remove any char styles from selection"
	"This is a user command, and generates undo"

	| command |
	command _ actualContents commandForRemoveCharacterStylesIn: anInterval.
	undoRedoCommands
		nextPut: command;
		truncateAtPosition.	"To disable redo of previous commands, now invalidated."
	command doOn: self! !

!StyledTextModel methodsFor: 'Shout Styling' stamp: 'jmv 1/25/2011 16:15'!
privateStyleWith: anSHTextStyler
	self actualContents paragraphStyleChunksDo: [ :interval :paragraphStyle |
		paragraphStyle ifNotNil: [ paragraphStyle doesShout ifTrue: [ anSHTextStyler privateStyle: interval ]]]! !

!StyledTextModel methodsFor: 'undoable commands' stamp: 'jmv 12/29/2011 15:38'!
removeReferencesToCharacterStyle: oldCharacterStyle
	"Replace in all contents. Both arguments must be of the same kind (either para or char style)."
	"This is a user command, and generates undo"

	"Undo not yet implemented. Reasons:
	1) it would hold references to unused styles that we might want to die
	2) it would require a slower implementation"
	self flag: #jmv.

	actualContents removeReferencesToCharacterStyle: oldCharacterStyle! !

!StyledTextModel methodsFor: 'undoable commands' stamp: 'jmv 9/21/2011 11:35'!
replaceReferencesToStyle: oldParagraphOrCharacterStyle with: newParagraphOrCharacterStyle
	"Replace in all contents. Both arguments must be of the same kind (either para or char style)."
	"This is a user command, and generates undo"

	"Undo not yet implemented. Reasons:
	1) it would hold references to unused styles that we might want to die
	2) it would require a slower implementation"
	self flag: #jmv.

	actualContents replaceReferencesToStyle: oldParagraphOrCharacterStyle with: newParagraphOrCharacterStyle! !

!StyledTextModel methodsFor: 'file save' stamp: 'jmv 12/20/2012 12:21'!
save
	"Answer wether save was successful."
	"Note: to enable the use of StyledText in applications, where 'accept' or 'save' have other meanings than 'save to file', we need to merge this class with PluggableTextModel, and have the textProvider be the application."
	fileName ifNil: [
		fileName _ FillInTheBlankMorph
			request: 'File name?'
			initialAnswer: ''.
		fileName isEmpty ifTrue: [ ^false ]].

	self saveAs: fileName.
	^true! !

!StyledTextModel methodsFor: 'file save' stamp: 'bp 4/22/2012 13:38'!
saveAs: aName
	| refStream |
	fileName _ self class withExtension: aName.
	self flushUndoRedoCommands.
	refStream _ SmartRefStream forceNewFileNamed: fileName.
	[ refStream nextPut: self ] ensure: [ refStream close ]! !

!StyledTextModel methodsFor: 'Shout Styling' stamp: 'jmv 12/21/2010 23:48'!
shoutAboutToStyle: aSHTextStyler
	"This is a notification that aSHTextStyler is about to re-style its text."

	aSHTextStyler classOrMetaClass: nil.
	^true! !

!StyledTextModel methodsFor: 'accessing' stamp: 'jmv 8/9/2011 14:40'!
styleSet
	^styleSet! !

!StyledTextModel methodsFor: 'accessing' stamp: 'jmv 8/11/2011 11:31'!
styleSet: aStyleSet
	"All assignments to the styleSet ivar should call this method."
	styleSet ifNotNil: [ styleSet removeActionsWithReceiver: self ].
	styleSet _ aStyleSet.
	styleSet ifNotNil: [ styleSet when: #stylesChanged send: #styleSetChanged to: self ]! !

!StyledTextModel methodsFor: 'events' stamp: 'jmv 8/11/2011 11:33'!
styleSetChanged
	"Our style set (or some style in it) changed.
	Update text and any views."
	actualContents beStyledTextWith: styleSet.
	self triggerEvent: #stylesChanged! !

!StyledTextModel class methodsFor: 'as yet unclassified' stamp: 'jmv 7/17/2013 09:54'!
fromFileNamed: aFileName
	| fileName file model |
	fileName _ self withExtension: aFileName.
	file _ FileStream oldFileNamed: fileName.
	[ model _ (SmartRefStream on: file) next ] ensure: [ file close ].
	model fileName: fileName.
	"Crude way to fix old Color subInstances in files"
	"
	Color allInstancesDo: [ :c | c fix ].
	TranslucentColor allInstancesDo: [ :c | c fix ].
	"
	^model! !

!StyledTextModel class methodsFor: 'as yet unclassified' stamp: 'bp 12/21/2011 10:20'!
new
	^self styleSet: StyleSet sample! !

!StyledTextModel class methodsFor: 'as yet unclassified' stamp: 'bp 12/21/2011 10:19'!
styleSet: aStyleSet
	^super new
		styleSet: aStyleSet;
		yourself! !

!StyledTextModel class methodsFor: 'as yet unclassified' stamp: 'jmv 4/28/2012 19:10'!
withExtension: aName
	| suffix |
	suffix _ FileDirectory extensionDelimiter asString, 'object'.
	^(aName endsWith: suffix)
		ifTrue: [ aName ]
		ifFalse: [ aName , suffix ]! !

!STECompleter methodsFor: 'entries' stamp: 'jmv 7/14/2011 13:58'!
addEntriesTo: aStream
	| s count capitalize |
	s _ 400.
	count _ 0.
	"Capitalize options if user's word is capitalized"
	capitalize _ prefix notEmpty and: [ prefix first isUppercase ].
	words forPrefix: prefix keysAndValuesDo: [ :key :value |
		count = s
			ifTrue: [ ^self ].
		aStream nextPut: (capitalize ifTrue: [key capitalized] ifFalse: [key]).
		count _ count + 1 ]! !

!STECompleter methodsFor: 'initialization' stamp: 'jmv 7/14/2011 13:58'!
initialize
	words _ EnglishDict! !

!STECompleter methodsFor: 'entries' stamp: 'jmv 9/21/2011 11:14'!
selectedEntry
	^(Text
		string: (self entries at: menuMorph selected)
		attribute: (CharacterStyleReference new style: model styleSet autoCompletedStyle)), ' '! !

!STECompleter class methodsFor: 'class initialization' stamp: 'jmv 7/17/2013 10:10'!
initialize
	"
	STECompleter initialize
	An English dictionary with (at this time of writing) 166211 words, taking 7.5Mbytes.
	Load a dictionary from (for example)  ./scowl-7.1/final/ up to a certain level.
	"
	| level baseDirectory possibleDirectoryNames scowlName directory  strm |
	level _ 70.
	EnglishDict _ Trie new.
	baseDirectory _ (FileDirectory on: (CodePackage named: 'StyledText' createIfAbsent: false registerIfNew: false) fullFileName) containingDirectory.
	possibleDirectoryNames _ (baseDirectory directoryNames select: [ :any | any beginsWith: 'scowl']) asArray sort.
	possibleDirectoryNames ifEmpty: [
		Transcript newLine; show: 'No word list for English spell checker was found.'.
		^self ].
	scowlName _ possibleDirectoryNames last.
	directory _ ((baseDirectory directoryNamed: scowlName) directoryNamed: 'final').
	directory fileNames do: [ :filename |
		(filename prefixAndSuffix: FileDirectory extensionDelimiter) last asNumber <= level ifTrue: [
			strm _ directory oldFileOrNoneNamed: filename.
			filename print.
			[
				strm reset.
				[ strm atEnd ] whileFalse: [
					strm nextLine substrings do: [ :word | 
						EnglishDict add: word asLowercase ]
					]
				] ensure: [strm close]]].
	'Spell Checker importing done.' print.! !

!CharacterStyle methodsFor: 'accessing' stamp: 'jmv 8/26/2009 22:52'!
color
	^color! !

!CharacterStyle methodsFor: 'accessing' stamp: 'jmv 4/6/2011 16:18'!
emphasis
	^emphasis! !

!CharacterStyle methodsFor: 'accessing' stamp: 'jmv 4/6/2011 16:18'!
familyName
	^familyName! !

!CharacterStyle methodsFor: 'accessing' stamp: 'jmv 12/19/2011 13:40'!
font
	| base |
	base _ (AbstractFont familyName: familyName pointSize: pointSize) ifNil: [ AbstractFont default baseFont ].
	^base emphasized: emphasis! !

!CharacterStyle methodsFor: 'initialization' stamp: 'jmv 8/11/2011 10:37'!
initialize
	name _ ''.
	emphasis _ 0.
	kern _ 0! !

!CharacterStyle methodsFor: 'comparing' stamp: 'jmv 8/9/2011 15:50'!
isEquivalentTo: aStyle
	"Compare all 'functional' attributes. I.e. ignore name."
	self class == aStyle class ifFalse: [ ^false ].

	^ pointSize = aStyle pointSize and: [
		emphasis = aStyle emphasis and: [
			color = aStyle color and: [
				kern = aStyle kern and: [
					familyName = aStyle familyName ]]]]! !

!CharacterStyle methodsFor: 'testing' stamp: 'jmv 8/9/2011 10:44'!
isNullStyle
	^ name = '-none-'! !

!CharacterStyle methodsFor: 'accessing' stamp: 'jmv 4/7/2011 11:18'!
kern
	^kern! !

!CharacterStyle methodsFor: 'accessing' stamp: 'jmv 8/26/2009 22:52'!
name
	^name! !

!CharacterStyle methodsFor: 'accessing' stamp: 'jmv 4/6/2011 16:18'!
pointSize
	^pointSize! !

!CharacterStyle methodsFor: 'accessing' stamp: 'jmv 8/26/2009 22:52'!
printOn: aStream
	super printOn: aStream.
	aStream space; nextPutAll: self name! !

!CharacterStyle methodsFor: 'private' stamp: 'jmv 8/11/2011 10:48'!
privateFamilyName: otherString pointSize: aNumber emphasis: otherNumber color: aColor
	"To be used from CharacterStyle instance creation methods.
	If used on existing instances, existing text will be modified, but in any case, senders are responsible
	for updating any text editor using us, or any text that should start using us (i.e. users of a StyleSet to which we are added)."

	familyName _ otherString.
	pointSize _ aNumber.
	emphasis _ otherNumber.
	color _ aColor.
"	kern _ 0"! !

!CharacterStyle methodsFor: 'private' stamp: 'jmv 8/11/2011 10:48'!
privateKern: anInteger
	"To be used from CharacterStyle instance creation methods.
	If used on existing instances, existing text will be modified, but in any case, senders are responsible
	for updating any text editor using us, or any text that should start using us (i.e. users of a StyleSet to which we are added)."
	kern _ anInteger! !

!CharacterStyle methodsFor: 'private' stamp: 'jmv 8/11/2011 10:48'!
privateName: aString
	"To be used from CharacterStyle instance creation methods.
	If used on existing instances, existing text will be modified, but in any case, senders are responsible
	for updating any text editor using us, or any text that should start using us (i.e. users of a StyleSet to which we are added)."
	name _ aString! !

!CharacterStyle methodsFor: 'private' stamp: 'jmv 8/11/2011 10:48'!
privatePointSize: aNumber
	"To be used from CharacterStyle instance creation methods.
	If used on existing instances, existing text will be modified, but in any case, senders are responsible
	for updating any text editor using us, or any text that should start using us (i.e. users of a StyleSet to which we are added)."
	pointSize _ aNumber! !

!CharacterStyle methodsFor: 'accessing' stamp: 'jmv 4/7/2011 11:11'!
shortDescription
	"note: Does not use all state. Two different instances could answer the same shortDescription."
	^String streamContents: [ :strm | self shortPrintOn: strm ]! !

!CharacterStyle methodsFor: 'printing' stamp: 'jmv 4/12/2011 09:52'!
shortPrintOn: strm
	familyName ifNotNil: [
		strm nextPutAll: familyName ].
	pointSize ifNotNil: [
		strm nextPut: $ .
		pointSize printOn: strm].
	(emphasis anyMask: AbstractFont boldCode) ifTrue: [
		strm nextPutAll: ' bold' ].
	(emphasis anyMask: AbstractFont italicCode) ifTrue: [
		strm nextPutAll: ' italic' ].
	(emphasis anyMask: AbstractFont underlinedCode) ifTrue: [
		strm nextPutAll: ' underlined' ].
	(emphasis anyMask: AbstractFont struckThroughCode) ifTrue: [
		strm nextPutAll: ' struckThrough' ].
	(emphasis anyMask: AbstractFont outlineCode) ifTrue: [
		strm nextPutAll: ' outline' ].
	color ifNotNil: [
		strm nextPut: $ .
		color shortPrintOn: strm ]! !

!CharacterStyle class methodsFor: 'instance creation' stamp: 'jmv 8/9/2011 10:45'!
nullStyle

	^self new
		privateName: '-none-'! !

!ParagraphStyle methodsFor: 'accessing'!
alignment
	"Answer the code for the current setting of the alignment."

	^alignment! !

!ParagraphStyle methodsFor: 'accessing' stamp: 'jmv 1/25/2011 13:32'!
doNotShout
	doesShout _ false! !

!ParagraphStyle methodsFor: 'accessing' stamp: 'jmv 1/25/2011 13:32'!
doShout
	doesShout _ true! !

!ParagraphStyle methodsFor: 'accessing' stamp: 'jmv 1/25/2011 13:32'!
doesShout
	^doesShout! !

!ParagraphStyle methodsFor: 'accessing'!
firstIndent
	"Answer the horizontal indenting of the first line of a paragraph in the 
	style of the receiver."

	^firstIndent! !

!ParagraphStyle methodsFor: 'initialization' stamp: 'jmv 8/11/2011 08:08'!
initialize
	super initialize.
	firstIndent _ restIndent _ rightIndent _ spaceBefore _ spaceAfter _ 0.
	listBulletPattern _ nil.
	doesShout _ false.
	alignment _ 0.
	tabsArray _ ParagraphStyle defaultTabsArray! !

!ParagraphStyle methodsFor: 'comparing' stamp: 'jmv 8/9/2011 15:50'!
isEquivalentTo: aStyle
	(super isEquivalentTo: aStyle) ifFalse: [ ^false ].

	^alignment = aStyle alignment and: [
		doesShout = aStyle doesShout and: [
			firstIndent = aStyle firstIndent and: [
				restIndent = aStyle restIndent and: [
					rightIndent = aStyle rightIndent and: [
						spaceBefore = aStyle spaceBefore and: [
							spaceAfter = aStyle spaceAfter and: [
								listBulletPattern = aStyle listBulletPattern and: [
									tabsArray = aStyle tabsArray ]]]]]]]]! !

!ParagraphStyle methodsFor: 'accessing' stamp: 'jmv 9/29/2009 09:39'!
isListStyle
	^listBulletPattern notNil! !

!ParagraphStyle methodsFor: 'accessing' stamp: 'jmv 9/29/2009 09:15'!
listBulletPattern
	"Answer the string patter to be used to display bullets at the start of each line."

	^listBulletPattern! !

!ParagraphStyle methodsFor: 'tabs and margins' stamp: 'jmv 6/29/2010 10:03'!
nextTabXFrom: anX leftMargin: leftMargin rightMargin: rightMargin 
	"Tab stops are distances from the left margin. Set the distance into the 
	argument, anX, normalized for the paragraph's left margin."

	| normalizedX tabX |

	"Doing columns (i.e. using tabsArray) only makes sense if left flush. Otherwise, just answer a wider space"

	alignment = CharacterScanner leftFlushCode ifFalse: [
		^anX + self tabWidth min: rightMargin ].

	normalizedX _ anX - leftMargin.
	1 to: tabsArray size do: 
		[:i | (tabX _ tabsArray at: i) > normalizedX 
				ifTrue: [^leftMargin + tabX min: rightMargin]].
	^rightMargin! !

!ParagraphStyle methodsFor: 'private' stamp: 'jmv 8/11/2011 10:49'!
privateFamilyName: aString pointSize: aNumber emphasis: otherNumber color: aColor alignment: code
	"To be used from ParagraphStyle instance creation methods.
	If used on existing instances, existing text will be modified, but in any case, senders are responsible
	for updating any text editor using us, or any text that should start using us (i.e. users of a StyleSet to which we are added)."
	familyName _ aString.
	pointSize _ aNumber.
	emphasis _ otherNumber.
	color _ aColor.
"	kern _ 0."
	alignment _ code.
"	tabsArray _ ParagraphStyle defaultTabsArray."
"	firstIndent _ fi.
	restIndent _ ri.
	rightIndent _ rri.
	spaceBefore _ sb.
	spaceAfter _ sa."
"	listBulletPattern 
	doesShout"! !

!ParagraphStyle methodsFor: 'private' stamp: 'jmv 8/11/2011 10:49'!
privateFamilyName: aString pointSize: aNumber emphasis: otherNumber color: aColor alignment: code firstIndent: fi restIndent: ri rightIndent: rri spaceBefore: sb spaceAfter: sa
	"To be used from ParagraphStyle instance creation methods.
	If used on existing instances, existing text will be modified, but in any case, senders are responsible
	for updating any text editor using us, or any text that should start using us (i.e. users of a StyleSet to which we are added)."
	familyName _ aString.
	pointSize _ aNumber.
	emphasis _ otherNumber.
	color _ aColor.
"	kern _ 0."
	alignment _ code.
"	tabsArray _ ParagraphStyle defaultTabsArray."
	firstIndent _ fi.
	restIndent _ ri.
	rightIndent _ rri.
	spaceBefore _ sb.
	spaceAfter _ sa.
"	listBulletPattern 
	doesShout"! !

!ParagraphStyle methodsFor: 'private' stamp: 'jmv 8/11/2011 10:49'!
privateListBulletPattern: aString 
	"If this is notNil, then this style is a Bullet List Style
	Some examples are:
		'%. ' -> '1. ' '2. ' etc.
		'z) ' -> 'a) ' 'b) ' etc.
		'Z- ' -> 'A- ' 'B- ' etc
	"
	"To be used from ParagraphStyle instance creation methods.
	If used on existing instances, existing text will be modified, but in any case, senders are responsible
	for updating any text editor using us, or any text that should start using us (i.e. users of a StyleSet to which we are added)."

	listBulletPattern _ aString! !

!ParagraphStyle methodsFor: 'accessing'!
restIndent
	"Answer the indent for all but the first line of a paragraph in the style 
	of the receiver."

	^restIndent! !

!ParagraphStyle methodsFor: 'accessing'!
rightIndent
	"Answer the right margin indent for the lines of a paragraph in the style 
	of the receiver."

	^rightIndent! !

!ParagraphStyle methodsFor: 'printing' stamp: 'jmv 4/7/2011 11:11'!
shortPrintOn: strm
	"note: Does not use all state. Two different instances could print the same."

	super shortPrintOn: strm.

	alignment > 0 ifTrue: [
		strm nextPut: $ .
		strm nextPutAll: (#(right centered justified) at: alignment) ]! !

!ParagraphStyle methodsFor: 'accessing' stamp: 'jmv 4/7/2011 11:15'!
spaceAfter
	^spaceAfter! !

!ParagraphStyle methodsFor: 'accessing' stamp: 'jmv 4/7/2011 11:14'!
spaceBefore
	^spaceBefore! !

!ParagraphStyle methodsFor: 'tabs and margins' stamp: 'jmv 7/27/2009 13:05'!
tabWidth
	"Answer the width of a tab."

	^ParagraphStyle defaultTab! !

!ParagraphStyle methodsFor: 'accessing' stamp: 'jmv 4/7/2011 13:44'!
tabsArray
	^tabsArray! !

!ParagraphStyle class methodsFor: 'constants' stamp: 'jmv 7/27/2009 13:05'!
defaultTab
	^24! !

!ParagraphStyle class methodsFor: 'constants' stamp: 'jmv 7/27/2009 13:07'!
defaultTabsArray
	^#(24 48 72 96 120 144 168 192 216 240 264 288 312 336 360 384 408 432 456 480 504 528 552 576 600 624 648 672 696 720 744 768 792 816 840 864 888 912 936 960 984 1008 1032 1056 1080 1104 1128 1152 1176 1200 1224 1248 1272 1296 1320 1344 1368 1392 1416 1440 1464 1488 1512 1536 1560)! !

!StyledTextEditor methodsFor: 'private' stamp: 'jmv 3/14/2012 08:25'!
addAttributesForPasting: replacement
	| start stop answer paragraphStyle |
	(replacement is: #Text)
		ifTrue: [
			start _ self startIndex.
			stop _ self stopIndex.

			"Version previous to 12/19/2011"
			"If we are pasting a text that doesn't bring a ParagraphStyleReference in the last paragraph (because it doesn't end with a NewLine), and we need it,
			but it won't be provided by our text, as we are at the end, then add any paragraph attribute from emphasisHere.
			Note: If we are replacing all the current text (or we have no current text), then just leave whatever comes with replacement,
				asuming that the pasted text knows better than our default for empty text
				(unless it brings no  paragraph style at the end. In that case, add emphasisHere anyway)"
"			(replacement notEmpty and: [
				replacement last isLineSeparator not and: [ 
					stop > model textSize and: [
						start > 1 or: [(replacement paragraphStyleOrNilAt: replacement size + 1) isNil]
					]
				]
			])".


			"If we are pasting a text that maybe doesn't bring a relevant ParagraphStyleReference in the last paragraph (because it doesn't include a NewLine),
			but it won't be provided by our text, as we are at the end of it, then add any paragraph attribute from emphasisHere."
			answer _ ((replacement includes: Character newLineCharacter) not and: [  stop > model textSize and: [
				"Note: Do this only if the current paragraph is not empty, meaning that it is already using the current ParagraphStyle for some text."
				(start > 1 and: [ (model actualContents at: start-1) isLineSeparator not ])
			]])
				ifTrue: [ (replacement, (Text string: String newLineString attributes: emphasisHere)) copyFrom: 1 to: replacement size ]
				ifFalse: [ replacement ].

			"In any case, ensure ParagraphStyle. (we could reach this point without a ParagraphStyle, for example, by choosing autocompletion on an empty text,
			i.e. Open new STE, type 'Aardv' open completion and select 'Aardvark')"
			emphasisHere do: [ :each |
				each forParagraphStyleReferenceDo: [ :s | paragraphStyle _ s ]].
			answer runs mapValues: [ :attributes |
				(attributes anySatisfy: [ :att | att class == ParagraphStyleReference ])
					ifTrue: [ attributes ]
					ifFalse: [ attributes copyWith: (ParagraphStyleReference for: paragraphStyle) ]].

			^answer ]

		ifFalse: [
			^Text string: replacement attributes: emphasisHere ]! !

!StyledTextEditor methodsFor: 'private' stamp: 'jmv 6/13/2012 18:06'!
buildCmdShortcuts
	cmdShortcuts _ self class cmdShortcuts copy.
	model styleSet paragraphStyles do: [ :pair |
		cmdShortcuts at: pair first asciiValue + 1 put: #changeCurrentStyle: ].
	model styleSet characterStyles do: [ :pair |
		cmdShortcuts at: pair first asciiValue + 1 put: #changeCharacterStyle: ]! !

!StyledTextEditor methodsFor: 'editing keys' stamp: 'jmv 9/21/2011 10:54'!
changeCharacterStyle: aKeyboardEvent
	"This is a user command, and generates undo"

	(model styleSet characterStyleForShortcut: aKeyboardEvent keyCharacter)
		ifNil: [ self removeCharacterStyles ]
		ifNotNil: [ :style |
			style isNullStyle
				ifTrue: [ self removeCharacterStyles ]
				ifFalse: [ self applyAttribute: (CharacterStyleReference for: style) ]].
	^true! !

!StyledTextEditor methodsFor: 'editing keys' stamp: 'jmv 9/19/2011 17:22'!
changeCurrentStyle: aKeyboardEvent
	"This is a user command, and generates undo"

	(model styleSet paragraphStyleForShortcut: aKeyboardEvent keyCharacter)
		ifNotNil: [ :style |
			self applyAttribute: (ParagraphStyleReference for: style) ].
	^true! !

!StyledTextEditor methodsFor: 'clipboard access' stamp: 'jmv 8/11/2011 11:27'!
clipboardStringOrText

	| clipContents |
	clipContents _ Clipboard retrieveStringOrText.
	^(clipContents is: #Text)
		ifTrue: [	
			"Merge styles appropriately, but in a copy."
			clipContents asStyledTextWith: model styleSet ]
		ifFalse: [ clipContents]! !

!StyledTextEditor methodsFor: 'typing support' stamp: 'jmv 6/13/2012 18:06'!
cmdShortcuts
	"We have keyboard shortcuts on a per-instance basis."
	^cmdShortcuts! !

!StyledTextEditor methodsFor: 'accessing' stamp: 'jmv 6/13/2012 18:30'!
help
	"
	Editor help
	SimpleEditor help
	CellStyleEditor help
	TextEditor help
	SmalltalkEditor help
	"
	| allSpecs |
	allSpecs _ self class cmdShortcutsSpec", self class basicCmdShortcutsSpec".
	^String streamContents: [ :strm |
		model styleSet paragraphStyles do: [ :pair |
			strm nextPutAll: ('Cmd-', pair first asString, String tab, String tab, String tab, 'Paragraph Style: ', pair second name).
			strm newLine ].
		model styleSet characterStyles do: [ :pair |
			strm nextPutAll: ('Cmd-', pair first asString, String tab, String tab, String tab, 'Character Style: ', pair second name).
			strm newLine ].
		allSpecs do: [ :triplet | | c |
			c _ triplet first = Character space
				ifFalse: [ triplet first asString, String tab ]
				ifTrue: [ 'Space'].
			strm nextPutAll: ('Cmd-', c, String tab, String tab, triplet third).
			strm newLine ].
		]! !

!StyledTextEditor methodsFor: 'model access' stamp: 'jmv 6/13/2012 18:07'!
model: aModel
	super model: aModel.
	"All assignments to the styleSet ivar should call this method."
	model ifNotNil: [ model removeActionsWithReceiver: self ].
	model _ aModel.
	model ifNotNil: [ model when: #stylesChanged send: #someStyleChanged to: self ].
	emphasisHere _ { ParagraphStyleReference for: model styleSet defaultStyle }.
	self buildCmdShortcuts! !

!StyledTextEditor methodsFor: 'commands' stamp: 'jmv 3/11/2011 16:55'!
quit: aKeyboardEvent
	morph hasUnacceptedEdits ifTrue: [
		(self confirm: 'Save changes before quitting?' orCancel: [^ true]) ifTrue: [
			self save: aKeyboardEvent ]].
	Smalltalk 
		snapshot: false
		andQuit: true.
	^true! !

!StyledTextEditor methodsFor: 'commands' stamp: 'jmv 12/29/2011 15:22'!
removeCharacterStyles
	"Let user remove character styles for the current selection."
	"This is a user command, and generates undo"
	| anythingDone |
	emphasisHere _ emphasisHere reject: [ :att |
		att class == CharacterStyleReference ].

	anythingDone _ false.
	self selectionIntervalsDo: [ :interval |
		interval notEmpty ifTrue: [
			anythingDone _ true.
			model logUndoAndRemoveCharacterStylesIn: interval.
			paragraph recomposeFrom: interval first to: interval last delta: 0 ]].

	anythingDone ifTrue: [
		self recomputeSelection.	"Needed so visible selection is updated to reflect new visual extent of selection"
		self userHasEdited ].

	"Even if nothing done, emphasisHere might have changed"
	morph possiblyChanged! !

!StyledTextEditor methodsFor: 'commands' stamp: 'jmv 12/29/2011 15:39'!
replaceAllCharacterStyle
	"Let user add or change character styles for the current selection, or for text to be typed."
	"This is a user command, and generates undo"
	| styles current menuList |
	current _ self currentCharacterStyleOrNil.
	styles _ model styleSet characterStyles collect: [ :pair | pair second ].
	menuList _ model styleSet characterStyles collect: [ :pair |
		((pair second = current or: [ pair second isNullStyle and: [current isNil]])
				ifTrue: [ '<on>' ]
				ifFalse: [ '<off>' ]),  
			pair second name, ' (', pair first asString, ')' ].
	((SelectionMenu labelList: menuList lines: #() selections: styles) startUp)
		ifNotNil: [ :style |
			style isNullStyle ifTrue: [
				model removeReferencesToCharacterStyle: current.
				self someStyleChanged.
				^true ].
			style = current
				ifFalse: [
					model replaceReferencesToStyle: current with: style.
					self someStyleChanged ] ].
	^ true! !

!StyledTextEditor methodsFor: 'commands' stamp: 'jmv 9/19/2011 17:10'!
replaceAllCurrentStyle
	"Let user change styles for the current selection or pragraph."
	"This is a user command, and generates undo"
	| styles current menuList |

	current _ self currentParagraphStyle.
	styles _ model styleSet paragraphStyles collect: [ :pair | pair second ].
	menuList _ model styleSet paragraphStyles collect: [ :pair |
		(pair second = current
				ifTrue: [ '<on>' ]
				ifFalse: [ '<off>' ]),  
			pair second name, ' (', pair first asString, ')' ].
	((SelectionMenu labelList: menuList lines: #() selections: styles) startUp)
		ifNotNil: [ :style |
			style = current
				ifFalse: [
					model replaceReferencesToStyle: current with: style.
					self someStyleChanged ]].
	^ true! !

!StyledTextEditor methodsFor: 'commands' stamp: 'jmv 1/6/2012 14:24'!
save: aKeyboardEvent
	"Save keystroke"
	model save
		ifTrue: [ morph hasUnacceptedEdits: false ].
	^true! !

!StyledTextEditor methodsFor: 'selecting' stamp: 'jmv 11/18/2011 14:08'!
selectAllWithCurrentCharStyle

	self selectAllWithStyle: self currentCharacterStyleOrNil! !

!StyledTextEditor methodsFor: 'selecting' stamp: 'jmv 11/18/2011 14:08'!
selectAllWithCurrentStyle

	self selectAllWithStyle: self currentParagraphStyle! !

!StyledTextEditor methodsFor: 'selecting' stamp: 'jmv 12/10/2011 13:44'!
selectAllWithStyle: aParagraphOrCharacterStyle

	| i startIndexes stopIndexes |
	i _ 1.
	startIndexes _ OrderedCollection new.
	stopIndexes _ OrderedCollection new.
	model actualContents runs runsAndValuesDo: [ :count :attributes |
		attributes do: [ :att |
			(att isStyle and: [ att style == aParagraphOrCharacterStyle ])
				ifTrue: [
					startIndexes add: i.
					stopIndexes add: i+count ]].
		i _ i + count.].
	selectionStartBlocks _ startIndexes collect: [ :index | (paragraph characterBlockForIndex: index) ].
	selectionStopBlocks _ stopIndexes collect: [ :index | (paragraph characterBlockForIndex: index) ].
	self storeSelectionInParagraph! !

!StyledTextEditor methodsFor: 'commands' stamp: 'jmv 12/29/2011 15:27'!
selectCharacterStyle
	"Let user add or change character styles for the current selection, or for text to be typed."
	"This is a user command, and generates undo"

	| styles current menuList |
	current _ self currentCharacterStyleOrNil.
	styles _ model styleSet characterStyles collect: [ :pair | pair second ].
	menuList _ model styleSet characterStyles collect: [ :pair |
		((pair second = current or: [ pair second isNullStyle and: [current isNil]])
				ifTrue: [ '<on>' ]
				ifFalse: [ '<off>' ]),  
			pair second name, ' (', pair first asString, ')' ].
	((SelectionMenu labelList: menuList lines: #() selections: styles) startUp)
		ifNotNil: [ :style |
			style isNullStyle ifTrue: [
				self removeCharacterStyles.
				^true ].
			style = current
				ifFalse: [ self applyAttribute: (CharacterStyleReference for: style) ]
				ifTrue: [ self unapplyAttribute: (CharacterStyleReference for: style) ] ].
	^ true! !

!StyledTextEditor methodsFor: 'commands' stamp: 'jmv 9/15/2009 09:27'!
selectCharacterStyle: aKeyboardEvent
	"Let user add or change character styles for the current selection, or for text to be typed."

	^self selectCharacterStyle! !

!StyledTextEditor methodsFor: 'commands' stamp: 'jmv 9/19/2011 17:22'!
selectCurrentStyle
	"Let user change styles for the current selection or pragraph."
	"This is a user command, and generates undo"

	| styles current menuList |
	current _ self currentParagraphStyle.
	styles _ model styleSet paragraphStyles collect: [ :pair | pair second ].
	menuList _ model styleSet paragraphStyles collect: [ :pair |
		(pair second = current
				ifTrue: [ '<on>' ]
				ifFalse: [ '<off>' ]),  
			pair second name, ' (', pair first asString, ')' ].
	((SelectionMenu labelList: menuList lines: #() selections: styles) startUp)
		ifNotNil: [ :style |
			style = current
				ifFalse: [ self applyAttribute: (ParagraphStyleReference for: style) ]
				ifTrue: [ self unapplyAttribute: (ParagraphStyleReference for: style) ] ].
	^ true! !

!StyledTextEditor methodsFor: 'commands' stamp: 'jmv 9/15/2009 09:26'!
selectCurrentStyle: aKeyboardEvent
	"Let user change styles for the current selection or pragraph."

	^self selectCurrentStyle! !

!StyledTextEditor methodsFor: 'typing support' stamp: 'jmv 1/13/2012 12:36'!
setEmphasisHereFromTextForward: f

	model isTextEmpty
		ifTrue: [
			"default emphasisHere already set, but reflect it if needed"
			morph possiblyChanged ]
		ifFalse: [ super setEmphasisHereFromTextForward: f ]! !

!StyledTextEditor methodsFor: 'notifications' stamp: 'jmv 6/13/2012 18:07'!
someStyleChanged
	paragraph composeAll.
	self recomputeSelection.
	morph updateFromParagraph.

	"Update keyboard shortcuts"
	self buildCmdShortcuts! !

!StyledTextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/13/2012 18:02'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$q 	#quit:							'Quit')

		#(		$s 	#save: 						'Save')
		#(		$j 	#selectCurrentStyle: 		'Select Paragraph Style')
		#(		$k 	#selectCharacterStyle:	'Select Character Style')
	)! !

!StyledTextEditor class methodsFor: 'class initialization' stamp: 'jmv 11/4/2011 11:05'!
initialize
	"
	StyledTextEditor initialize
	"
	super initialize! !

!StyledTextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/13/2012 18:16'!
initializeMenu
	"Initialize the yellow button pop-up menu and corresponding messages."

	"
	Editor initialize
	"
	menu _ SelectionMenu fromArray: {
		{'Help...'.									#openHelp}.
		#-.
		{'Find...(f)'.									#find}.
		{'Find Again (g)'.							#findAgain}.
		{'Use Selection for Find (h)'.				#setSearchString}.
		#-.
		{'Undo - multiple (z)'.						#undo}.
		{'Redo - multiple (Z)'.						#redo}.
		{'Undo / Redo history'.						#offerUndoHistory}.
		{'Forget Undo / Redo history'.				#flushUndoRedoCommands}.
		#-.
		{'Copy (c)'.									#copySelection}.
		{'Cut (x)'.									#cut}.
		{'Paste (v)'.									#paste}.
		{'Paste without Format'.					#pasteString}.
		{'Paste...'.									#pasteRecent}.
		#-.
		{'Select text with Paragraph Style'.			#selectAllWithCurrentStyle}.
		{'Select text with Character Style'.			#selectAllWithCurrentCharStyle}.
		#-.
		{'Change Paragraph Style...'.				#selectCurrentStyle}.
		{'Change Character Style...'.				#selectCharacterStyle}.
		{'Remove Character Style'.					#removeCharacterStyles}.
		{'Replace all uses of Paragraph Style...'.	#replaceAllCurrentStyle}.
		{'Replace all uses of Character Style...'.		#replaceAllCharacterStyle}.
	}! !

!FancyDraggeableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2012 22:14'!
drawOn: aCanvas

	self drawSTELookOn: aCanvas! !

!FancyDraggeableButtonMorph methodsFor: 'drawing' stamp: 'jmv 2/14/2013 13:16'!
drawSTELookOn: aCanvas

	aCanvas
		roundRect: (((0@0 extent: extent) insetBy: (owner morphBoundsInWorld isWide ifTrue: [0@4] ifFalse: [4@0])) )
		color: (Color black)
		radius: 4.
	aCanvas
		roundRect: (((-1@0 extent: extent) insetBy: (owner morphBoundsInWorld isWide ifTrue: [0@4] ifFalse: [5@0 corner: 4@0])) )
		color: (self isPressed ifTrue: [Color red] ifFalse: [Color gray: 0.86])
		radius: 4.! !

!FancyDraggeableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2012 22:37'!
iconColor

	^ self isPressed
		ifTrue: [ Color red ]
		ifFalse: [
			self mouseIsOver
				ifTrue: [ Color gray: 0.75 ]
				ifFalse: [ Color white ]].! !

!FancyButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2012 22:15'!
drawOn: aCanvas

	self isRoundButton
		ifTrue: [ icon ifNil: [ self drawRoundGradientLookOn: aCanvas ]]
		ifFalse: [
			self drawSTELookOn: aCanvas].

	icon ifNotNil: [
		self drawInconOn: aCanvas ].! !

!FancyButtonMorph methodsFor: 'drawing' stamp: 'jmv 2/14/2013 13:14'!
drawSTELookOn: aCanvas

	aCanvas image: (FormCanvas steButtonForm: extent) at:0@0! !

!FancyButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2012 22:37'!
iconColor

	^ self isPressed
		ifTrue: [ Color red ]
		ifFalse: [
			self mouseIsOver
				ifTrue: [ Color gray: 0.75 ]
				ifFalse: [ Color white ]].! !

!FancyButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 4/12/2012 22:17'!
updateDownButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowWithGradientOfDirection: #down.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		actWhen: #buttonStillDown;		"to enable multiple action if held down"
		redrawNeeded! !

!FancyButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 4/12/2012 22:17'!
updateLeftButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowWithGradientOfDirection: #left.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		actWhen: #buttonStillDown;		"to enable multiple action if held down"
		redrawNeeded! !

!FancyButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 4/12/2012 22:17'!
updateRightButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowWithGradientOfDirection: #right.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		actWhen: #buttonStillDown;		"to enable multiple action if held down"
		redrawNeeded! !

!FancyButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 4/12/2012 22:17'!
updateUpButtonImage
	"update the receiver's as a upButton. put a new image inside"

	icon _ FormCanvas arrowWithGradientOfDirection: #up.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		actWhen: #buttonStillDown;		"to enable multiple action if held down"
		redrawNeeded! !

!PluggableDropDownListMorph methodsFor: 'private' stamp: 'jmv 2/25/2013 15:21'!
basicOpenList
	| xtraWidth xtraHeight bounds |
	bounds _ self morphBoundsInWorld.
	listMorph _ PluggableActOnReturnKeyListMorph
		model: self
		listGetter: #getList
		indexGetter: #getIndex
		indexSetter: #setIndex:.
	listMorph
		color: Color white;
		morphWidth: self morphWidth;
		morphHeight: 4;
		borderWidth: 1;
		borderColor: (Color black alpha: 0.3);
		morphPosition: bounds bottomLeft;
		autoDeselect: false.
	self world addMorph: listMorph.
	listMorph updateList.
	xtraWidth _ listMorph hLeftoverScrollRange + 4.
	xtraWidth > 0 ifTrue: [
		listMorph morphWidth: listMorph morphWidth + xtraWidth ].
	xtraHeight _ listMorph vLeftoverScrollRange + 4.
	xtraHeight > 0 ifTrue: [
		listMorph morphHeight: (listMorph morphHeight + xtraHeight min: 100) ]! !

!PluggableDropDownListMorph methodsFor: 'private' stamp: 'jmv 9/10/2010 15:31'!
closeList
	listMorph ifNotNil: [
		listMorph delete.
		listMorph _ nil ]! !

!PluggableDropDownListMorph methodsFor: 'drawing' stamp: 'jmv 2/14/2013 13:29'!
drawBasicLookOn: aCanvas

	aCanvas
		fillRectangle: (0@0 extent: extent)
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.
	self drawLabelOn: aCanvas ! !

!PluggableDropDownListMorph methodsFor: 'drawing' stamp: 'jmv 2/14/2013 13:33'!
drawLabelOn: aCanvas 

	| f |
	f _ Preferences standardButtonFont.
	aCanvas drawString: label at: 0@(extent y // 2) + (8@ f height negated // 2) font: f color: Color black! !

!PluggableDropDownListMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2012 22:19'!
drawOn: aCanvas

	"Theme current steButtons"
	true
		ifTrue: [ self drawSTELookOn: aCanvas ]
		ifFalse: [ self drawBasicLookOn: aCanvas ]! !

!PluggableDropDownListMorph methodsFor: 'drawing' stamp: 'jmv 2/14/2013 13:31'!
drawSTELookOn: aCanvas
	| gh |
"sin gradiente el borde!!"
	gh _ extent y-8 max: extent y//2.
	aCanvas
		roundRect: (0@0 extent: extent)
		color: (Color gray: 0.4)
		radius: 4
		gradientTop: 0.9
		gradientBottom: 0.6
		gradientHeight: gh.
	aCanvas
		roundRect: (1@1 extent: extent-2)
		color: (Color gray: 0.95)
		radius: 4
		gradientTop: 0.99
		gradientBottom: 0.96
		gradientHeight: gh.
	self drawLabelOn: aCanvas ! !

!PluggableDropDownListMorph methodsFor: 'model' stamp: 'jmv 9/9/2010 15:12'!
getIndex
	^model ifNil: [ 0 ] ifNotNil: [ model perform: getIndexSelector ]! !

!PluggableDropDownListMorph methodsFor: 'accessing' stamp: 'jmv 4/6/2011 19:03'!
getLabel
	| i |
	i _ self getIndex.
	label _ i = 0
		ifTrue: [ '-none-' ]
		ifFalse: [ self getList at: i ].! !

!PluggableDropDownListMorph methodsFor: 'model' stamp: 'jmv 9/16/2009 13:45'!
getList
	^model perform: getListSelector! !

!PluggableDropDownListMorph methodsFor: 'testing' stamp: 'jmv 9/9/2010 14:25'!
handlesMouseDown: evt
	"So our #mouseDown: method is called"
	^ true! !

!PluggableDropDownListMorph methodsFor: 'testing' stamp: 'jmv 9/9/2010 14:25'!
handlesMouseOver: anEvent
	"So our #mouseLeave: method is called"
	^ true! !

!PluggableDropDownListMorph methodsFor: 'initialization' stamp: 'jmv 12/20/2012 13:10'!
initialize
	| icon |
	super initialize.
	self color: Color white.
	self borderColor: Color black.
	self getLabel.
"	self extent: 120 @ 20."
	icon _ "Theme current steButtons" true
		ifFalse: [ FormCanvas arrowOfDirection: #down size: ScrollBar scrollbarThickness ]
		ifTrue: [ FormCanvas arrowWithGradientOfDirection: #down ].
	downButton _ FancyButtonMorph new.
	downButton
		model: self;
		roundButtonStyle: false;
		icon: icon;
		actWhen: #buttonDown;
		action: #openOrCloseList.
	self addMorph: downButton.! !

!PluggableDropDownListMorph methodsFor: 'testing' stamp: 'jmv 1/27/2011 16:11'!
isListOpen
	^ listMorph notNil! !

!PluggableDropDownListMorph methodsFor: 'accessing' stamp: 'jmv 9/10/2010 08:37'!
label
	^ label! !

!PluggableDropDownListMorph methodsFor: 'layout' stamp: 'jmv 12/20/2012 12:57'!
layoutSubmorphs
	| e innerBounds |
	"innerBounds _ self innerBounds".
	innerBounds _ self morphBoundsInWorld insetBy: borderWidth.
	e _ innerBounds height.
	downButton morphBoundsInWorld: (innerBounds bottomRight - e extent: e)! !

!PluggableDropDownListMorph methodsFor: 'initialization' stamp: 'jmv 9/16/2009 11:29'!
model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel

	self model: anObject.
	getListSelector _ getListSel.
	getIndexSelector _ getSelectionSel.
	setIndexSelector _ setSelectionSel.! !

!PluggableDropDownListMorph methodsFor: 'model' stamp: 'jmv 6/3/2011 14:43'!
modelChanged
	self getLabel.
	self changed: self! !

!PluggableDropDownListMorph methodsFor: 'events' stamp: 'jmv 12/20/2012 13:24'!
mouseLeave: evt
	super mouseLeave: evt.
	(listMorph isNil or: [ (listMorph morphBoundsInWorld containsPoint: evt eventPosition) not])
		ifTrue: [
			"Do the call even if the list is not there, as this also clears selection in subclass with entry field"
			self closeList ]! !

!PluggableDropDownListMorph methodsFor: 'events' stamp: 'jmv 12/27/2012 15:02'!
mouseLeaveList: evt
	(self morphBoundsInWorld containsPoint: evt eventPosition)
		ifFalse: [ self closeList ]! !

!PluggableDropDownListMorph methodsFor: 'private' stamp: 'jmv 2/14/2013 12:41'!
openList
	self basicOpenList.
	self world activeHand newKeyboardFocus: listMorph! !

!PluggableDropDownListMorph methodsFor: 'private' stamp: 'jmv 8/16/2010 16:01'!
openOrCloseList
	self isListOpen
		ifFalse: [ self openList ]
		ifTrue: [ self closeList ]! !

!PluggableDropDownListMorph methodsFor: 'events' stamp: 'jmv 6/3/2011 14:44'!
setIndex: index
	model perform: setIndexSelector with: index.
	"self changed: #getIndex." "No chance to actually see it, it is closed too quickly"
	self getLabel.
	self changed: self.
	self closeList! !

!PluggableDropDownListMorph class methodsFor: 'instance creation' stamp: 'jmv 9/16/2009 11:30'!
model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel

	^self new
		model: anObject
		listGetter: getListSel
		indexGetter: getSelectionSel
		indexSetter: setSelectionSel! !

!PluggableFilteringDropDownListMorph methodsFor: 'private' stamp: 'jmv 12/20/2012 13:22'!
closeList
	"Also clear the selection in the entry field"
	(listMorph notNil and: [ listMorph hasKeyboardFocus ]) ifTrue: [
		self world activeHand newKeyboardFocus: editorMorph ].
	super closeList! !

!PluggableFilteringDropDownListMorph methodsFor: 'drawing' stamp: 'jmv 9/10/2010 08:57'!
drawLabelOn: aCanvas
	"Not needed. Our label is a submorph"! !

!PluggableFilteringDropDownListMorph methodsFor: 'events' stamp: 'jmv 12/20/2010 15:00'!
editorClass
	^TextEditor! !

!PluggableFilteringDropDownListMorph methodsFor: 'accessing' stamp: 'jmv 7/5/2011 09:02'!
filter
	^editorMorph contents withBlanksTrimmed! !

!PluggableFilteringDropDownListMorph methodsFor: 'filtering' stamp: 'jmv 9/10/2010 15:56'!
filter: entry with: filter

	^filter, '*' match: entry! !

!PluggableFilteringDropDownListMorph methodsFor: 'model' stamp: 'jmv 4/6/2011 19:03'!
getIndex
	"answer the index in the filtered list"
	| i filter |
	i _ super getIndex.
	i = 0 ifFalse: [
		filter _ self filter.
		(filter notEmpty and: [ filter ~= '-none-' ]) ifTrue: [
			i _ self getList indexOf: ((model perform: getListSelector) at: i) ]].
	^i! !

!PluggableFilteringDropDownListMorph methodsFor: 'accessing' stamp: 'jmv 3/16/2011 17:28'!
getLabel
	super getLabel.
	editorMorph ifNotNil: [ editorMorph contents: label ]! !

!PluggableFilteringDropDownListMorph methodsFor: 'model' stamp: 'jmv 8/10/2011 15:17'!
getList
	| answer filter |
	answer _ super getList.
	filter _ self filter.
	(filter notEmpty and: [ filter ~= '-none-' ]) ifTrue: [ | filtered |
		filtered _ (answer select: [ :str | self filter: str with: filter ]).
		(filtered includes: filter) ifFalse: [
			answer _ filtered ]].
	^answer! !

!PluggableFilteringDropDownListMorph methodsFor: 'initialization' stamp: 'jmv 12/27/2012 15:06'!
initialize
	| labelFont |
	super initialize.
	labelFont _ AbstractFont familyName: 'DejaVu' aroundPointSize: 10.
	editorMorph _ FilteringDDLEditorMorph contents: self label font: labelFont emphasis: 1.
	editorMorph keyboardFocusWatcher: self.
	self addMorph: editorMorph! !

!PluggableFilteringDropDownListMorph methodsFor: 'events' stamp: 'jmv 3/13/2012 10:41'!
keyStrokeInList: aKeyboardEvent
	"Send to the text anything but Up, Down, Return and Escape"
	(aKeyboardEvent isReturnKey or: [
		(#(27 30 31) includes: aKeyboardEvent keyValue)]) ifFalse: [
			editorMorph keyStroke: aKeyboardEvent ]! !

!PluggableFilteringDropDownListMorph methodsFor: 'events' stamp: 'jmv 3/13/2012 10:59'!
keyStrokeInText: aKeyboardEvent

	"Handle Return and Escape separatedly"
	aKeyboardEvent isReturnKey ifTrue: [ ^self returnInEditor ].
	aKeyboardEvent keyValue = 27 ifTrue: [ ^self closeList ].

	"Send to the list only Up and Down,"
	self shouldOpenList
		ifTrue: [ self basicOpenList ]
		ifFalse: [
			self shouldCloseList ifTrue: [ self closeList ]].
	listMorph ifNotNil: [
		(#(30 31 ) includes: aKeyboardEvent keyValue)
			ifTrue: [ listMorph keyStroke: aKeyboardEvent ].
		listMorph verifyContents ]! !

!PluggableFilteringDropDownListMorph methodsFor: 'layout' stamp: 'jmv 12/20/2012 13:11'!
layoutSubmorphs
	| b innerBounds |
	super layoutSubmorphs.
	"innerBounds _ self innerBounds".
	innerBounds _ self morphBoundsInWorld insetBy: borderWidth.
	b _ innerBounds insetBy: 8@4.
	editorMorph morphBoundsInWorld: (b topLeft extent: b extent - (downButton morphWidth@0))! !

!PluggableFilteringDropDownListMorph methodsFor: 'events' stamp: 'jmv 12/29/2011 14:50'!
lostFocus: aMorph
	(self isListOpen and: [ listMorph hasKeyboardFocus not ]) ifTrue: [
		self closeList ]! !

!PluggableFilteringDropDownListMorph methodsFor: 'private' stamp: 'jmv 11/4/2010 15:42'!
openList

	super openList.
	editorMorph selectAll! !

!PluggableFilteringDropDownListMorph methodsFor: 'model' stamp: 'jmv 3/13/2012 10:26'!
returnInEditor
	| filter all selected |
	self isListOpen ifTrue: [
		^ listMorph returnPressed ].

	filter _ self filter.
	filter isEmpty ifTrue: [ ^ self ].
	all _ model perform: getListSelector.
	selected _ all
		detect: [ :any | self filter: any with: filter ]
		ifNone: [ ^ self ].
	^ super setIndex: (super getList indexOf: selected)! !

!PluggableFilteringDropDownListMorph methodsFor: 'model' stamp: 'jmv 3/16/2011 09:10'!
setIndex: index
	| i filter |
	i _ index.
	filter _ self filter.
	filter notEmpty ifTrue: [
		i _ (model perform: getListSelector) indexOf: ( self getList at: i ) ].
	super setIndex: i! !

!PluggableFilteringDropDownListMorph methodsFor: 'events' stamp: 'jmv 3/16/2011 09:27'!
shouldCloseList
	"True if no matches to show, or if there is no  typed text"
	^self isListOpen and: [
		"Filter empty does not mean we shouldn't show the list. Show it without filtering!!"
		"self filter isEmpty or: [ "
			self getList isEmpty 
		"]"
	]! !

!PluggableFilteringDropDownListMorph methodsFor: 'events' stamp: 'jmv 3/16/2011 09:24'!
shouldOpenList
	"True if list not open, but it makes sense to show it, because there is more than one entry that matches the typed text. "
	^self isListOpen not and: [
		"Open full list if filter empty. Why not???"
		"self filter notEmpty and: [ "
			self getList notEmpty "
		]" 
	]! !

!PluggableActOnReturnKeyListMorph methodsFor: 'model access' stamp: 'jmv 3/13/2012 10:57'!
changeModelSelection: anInteger
	"On regular PluggableListMorphs this method is called when a selection is made.
	But we don't want to update the model when the arrow keys are pressed, we'll wait for <Return>
	However we do act immediately on mouse click.
	
	Just store the current index."
	currentIndex _ anInteger.
	self selectionIndex: currentIndex! !

!PluggableActOnReturnKeyListMorph methodsFor: 'geometry' stamp: 'jmv 4/14/2013 22:00'!
clippingRect
	"Return the bounds to which any submorphs should be clipped if the property is set"
	"Should be a region, like our shadow"
	| r |
	r _ super clippingRect.
	^r origin extent: r extent - 4! !

!PluggableActOnReturnKeyListMorph methodsFor: 'drawing' stamp: 'jmv 2/25/2013 15:18'!
drawOn: aCanvas
	"We draw our shadow outside our strict bounds..."

	aCanvas
		roundRect: (4@4 extent: extent-4)
		color: (Color black alpha: 0.13)
		radius: 4.
	aCanvas
		roundRect: (0@0 extent: extent-4)
		color: (Color gray: 0.4)
		radius: 4.
	aCanvas
		roundRect: (1@1 extent: extent-2-4)
		color: (Color white)
		radius: 4! !

!PluggableActOnReturnKeyListMorph methodsFor: 'model access' stamp: 'jmv 3/13/2012 10:10'!
getCurrentSelectionIndex
	^currentIndex ifNil: [ super getCurrentSelectionIndex ]! !

!PluggableActOnReturnKeyListMorph methodsFor: 'events' stamp: 'jmv 12/27/2012 15:02'!
keyStroke: event 

	event isReturnKey ifTrue: [
		self returnPressed.
		^self ].
	super keyStroke: event.
	
	"We know our model is a PluggableDropDownListMorph"
	model keyStrokeInList: event! !

!PluggableActOnReturnKeyListMorph methodsFor: 'events' stamp: 'jmv 2/14/2013 12:46'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Do update model right away"
	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self realChangeModelSelection! !

!PluggableActOnReturnKeyListMorph methodsFor: 'events' stamp: 'jmv 12/27/2012 15:02'!
mouseLeave: evt
	"We know our model is a PluggableDropDownListMorph"
	model mouseLeaveList: evt! !

!PluggableActOnReturnKeyListMorph methodsFor: 'model access' stamp: 'jmv 3/13/2012 10:10'!
realChangeModelSelection
	"Change the model's selected item index to be anInteger."
	setIndexSelector ifNotNil: [
		model
			perform: setIndexSelector
			with: currentIndex ].
	currentIndex _ nil! !

!PluggableActOnReturnKeyListMorph methodsFor: 'events' stamp: 'jmv 3/13/2012 10:10'!
returnPressed
	currentIndex
		ifNotNil: [ self realChangeModelSelection ]
		ifNil: [ self delete ]! !

!PluggableActOnReturnKeyListMorph methodsFor: 'initialization' stamp: 'jmv 4/12/2012 22:05'!
scrollBarClass
	^FancyScrollBar! !

!PluggableActOnReturnKeyListMorph methodsFor: 'geometry' stamp: 'jmv 2/25/2013 15:19'!
updateScrollBarsBounds
	
	| t |
	hideScrollBars ifTrue: [^self].
	t _ self scrollBarClass scrollbarThickness.
	scrollBar
		morphPosition: extent x - t - borderWidth - 4 @ borderWidth;
		morphExtent: t @ (self vScrollBarHeight-4).
	hScrollBar
		morphPosition: borderWidth @ (extent y - t - borderWidth - 4);
		morphExtent: self hScrollBarWidth - 4@t! !

!PluggableActOnReturnKeyListMorph methodsFor: 'updating' stamp: 'jmv 3/13/2012 10:10'!
verifyContents
	super verifyContents.
	"ok???"
	self selectionIndex = 0 ifTrue: [
		self changeModelSelection: 1 ]! !

!PluggableActOnReturnKeyListMorph methodsFor: 'geometry' stamp: 'jmv 2/25/2013 15:26'!
viewableExtent

	^self focusIndicatorExtent - (self xtraBorder * 2) - 4! !

!PluggableStyledTextMorph methodsFor: 'services' stamp: 'jmv 8/3/2011 17:04'!
characterStyleNamesAndShortcuts
	^model styleSet characterStyleNamesAndShortcuts! !

!PluggableStyledTextMorph methodsFor: 'services' stamp: 'jmv 11/16/2011 17:12'!
currentCharacterStyleIndex
	| cs |
	cs _ self textMorph editor currentCharacterStyleOrNil.
	cs ifNil: [ ^0 ].
	^(model styleSet characterStyleIndexOf: cs)! !

!PluggableStyledTextMorph methodsFor: 'services' stamp: 'jmv 11/16/2011 17:13'!
currentCharacterStyleIndex: index
	"This is a user command, and generates undo"
	(model styleSet characterStyleAt: index)
		ifNil: [
			self textMorph editor removeCharacterStyles ]
		ifNotNil: [ :style |
			style isNullStyle
				ifTrue: [ self textMorph editor removeCharacterStyles ]
				ifFalse: [ self textMorph editor applyAttribute: (CharacterStyleReference for: style) ]].
	self textMorph updateFromParagraph! !

!PluggableStyledTextMorph methodsFor: 'services' stamp: 'jmv 11/16/2011 17:12'!
currentParagraphStyleIndex
	^model styleSet paragraphStyleIndexOf: self textMorph editor currentParagraphStyle! !

!PluggableStyledTextMorph methodsFor: 'services' stamp: 'jmv 11/16/2011 17:13'!
currentParagraphStyleIndex: index
	"This is a user command, and generates undo"
	| style |
	style _ model styleSet paragraphStyleAt: index.
	self textMorph editor applyAttribute: (ParagraphStyleReference for: style).
	self textMorph updateFromParagraph! !

!PluggableStyledTextMorph methodsFor: 'services' stamp: 'jmv 8/3/2011 17:03'!
paragraphStyleNamesAndShortcuts
	^model styleSet paragraphStyleNamesAndShortcuts! !

!PluggableStyledTextMorph methodsFor: 'notifications' stamp: 'jmv 9/18/2009 14:06'!
possiblyChanged
	self triggerEvent: #possiblyChanged! !

!PluggableStyledTextMorph methodsFor: 'initialization' stamp: 'jmv 4/12/2012 22:06'!
scrollBarClass
	^FancyScrollBar! !

!PluggableStyledTextMorph class methodsFor: 'class initialization' stamp: 'jmv 11/4/2011 10:39'!
initialize
	"
	PluggableStyledTextMorph initialize
	"
	TheWorldMenu addExtraOpenCommands: {
		{'Styled Text Editor' . {#theWorldMenu . #openFancierStyledTextEditor}. 'A window for composing styled text' }.
"		{'Styled Text Editor' . {#theWorldMenu . #openStyledTextEditor}. 'A window for composing styled text' }."
	}.
	Theme current class beCurrent! !

!PluggableStyledTextMorph class methodsFor: 'class initialization' stamp: 'jmv 4/12/2013 20:25'!
withModel: aStyledTextModel in: aLayoutMorph
	| topRow paragraphStyleList characterStyleList textMorph m topRowHeight labelFont topRowElementsWidth |
	textMorph _ self withModel: aStyledTextModel.
	textMorph
		borderWidth: 0;
		drawKeyboardFocusIndicator: false;
		wrapFlag: true.
	aLayoutMorph separation: 0.
	topRow _ ToolbarMorph newRow separation: 10@0.

	topRowHeight _ 32.
	topRow
		color: (Form 
			verticalGradient: topRowHeight
			topColor:  (Color r: 189 g: 214 b: 199 range: 255)
			bottomColor:  (Color r: 115 g: 134 b: 125 range: 255))..
"		borderWidth: topRowBorderWidth;"
"		borderColor: (Color r: 80 g: 80 b: 80 range: 255)."

	labelFont _ AbstractFont familyName: 'DejaVu' aroundPointSize: 10.
	m _ StringMorph contents: 'Paragraph Style'.
	m font: labelFont emphasis: 1; color: Color white.
	topRowElementsWidth _ m morphWidth.
	topRow
		addMorph: m
		layoutSpec: (((LayoutSpec morphHeightFixedWidth: topRowElementsWidth) minorDirectionPadding: #center) minorDirectionPadding: #center).
	paragraphStyleList _ PluggableFilteringDropDownListMorph
			model: textMorph
			listGetter: #paragraphStyleNamesAndShortcuts
			indexGetter: #currentParagraphStyleIndex
			indexSetter: #currentParagraphStyleIndex:.
	paragraphStyleList borderWidth: 0"; height: ddlHeight".
	topRow
		addMorph: paragraphStyleList
		layoutSpec: (((LayoutSpec morphHeightFixedWidth: topRowElementsWidth+50) minorDirectionPadding: #center) minorDirectionPadding: #center).
	textMorph when: #possiblyChanged send: #modelChanged to: paragraphStyleList.

	m _ RectangleLikeMorph new.
	m color: Color transparent.
	topRow addMorph: m layoutSpec: (LayoutSpec fixedWidth: 8).

	m _ StringMorph contents: 'Character Style'.
	m font: labelFont emphasis: 1; color: Color white.
	topRow
		addMorph: m
		layoutSpec: (((LayoutSpec morphHeightFixedWidth: topRowElementsWidth) minorDirectionPadding: #center) minorDirectionPadding: #center).
	characterStyleList _ PluggableFilteringDropDownListMorph 
			model:textMorph
			listGetter: #characterStyleNamesAndShortcuts
			indexGetter: #currentCharacterStyleIndex
			indexSetter: #currentCharacterStyleIndex:.
	characterStyleList borderWidth: 0"; height: ddlHeight". 
	topRow
		addMorph: characterStyleList
		layoutSpec: (((LayoutSpec morphHeightFixedWidth: topRowElementsWidth+50) minorDirectionPadding: #center) minorDirectionPadding: #center).
	textMorph when: #possiblyChanged send: #modelChanged to: characterStyleList.

	aLayoutMorph
		addMorph: topRow layoutSpec: (LayoutSpec fixedHeight: topRowHeight);
		addMorph: textMorph layoutSpec: (LayoutSpec new).

	^aLayoutMorph! !

!FancyScrollBar methodsFor: 'initialize' stamp: 'jmv 4/12/2012 22:12'!
buttonClass
	^FancyButtonMorph! !

!FancyScrollBar methodsFor: 'drawing' stamp: 'jmv 2/14/2013 13:19'!
drawOn: aCanvas

	aCanvas
		roundRect: (0@0 extent: extent)
		color: (Color gray: 0.4)
		radius: 4.
	aCanvas
		roundRect: (1@1 extent: extent-2)
		color: (Color gray: 0.95)
		radius: 4! !

!FancyScrollBar methodsFor: 'initialize' stamp: 'jmv 4/12/2012 22:23'!
sliderClass
	^FancyDraggeableButtonMorph! !

!FancyScrollBar methodsFor: 'scrolling' stamp: 'jmv 3/5/2013 08:20'!
sliderGrabbedAt: handPositionRelativeToSlider

	grabPosition _ handPositionRelativeToSlider.
	sliderShadow
		morphBoundsInWorld: (slider morphBoundsInWorld insetBy: (extent x > extent y ifTrue: [0@3] ifFalse: [3@0]));
		show! !

!FancyScrollBar class methodsFor: 'constants' stamp: 'jmv 4/12/2012 22:27'!
scrollbarThickness

	^super scrollbarThickness + 10! !

!STEMainMorph methodsFor: 'initialization' stamp: 'jmv 5/24/2011 09:02'!
initialize
	super initialize.
	self step; startStepping! !

!STEMainMorph methodsFor: 'stepping' stamp: 'jmv 12/20/2012 12:24'!
step

	"My dimensions are constrained live."
	| r |
	owner isWorldMorph ifTrue: [
		r _ owner morphBoundsInWorld.
		self morphBoundsInWorld = r ifFalse: [
			self morphBoundsInWorld: r]]! !

!STEMainMorph methodsFor: 'stepping' stamp: 'jmv 5/24/2011 09:04'!
stepTime
	^ 0  "every cycle"! !

!ToolbarMorph methodsFor: 'as yet unclassified' stamp: 'jmv 4/12/2013 20:18'!
clipsSubmorphs
	^true! !

!ToolbarMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2013 20:23'!
drawOn: aCanvas
	aCanvas
		fillRectangle: (0@0 extent: self morphExtent)
		tilingWith: color
		sourceRect: color boundingBox
		rule: Form paint! !

!FilteringDDLEditorMorph methodsFor: 'events' stamp: 'jmv 12/27/2012 15:00'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	super keyStroke: aKeyboardEvent.

	"We know that our owner is a PluggableFilteringDropDownListMorph"
	owner keyStrokeInText: aKeyboardEvent! !

!RTFStyledTextBuilder methodsFor: 'private' stamp: 'jmv 8/11/2011 10:44'!
addToText: aString specialAttributes: nonFormattingAttributesOrNil
	"nonFormattingAttributesOrNil should only contains attributes that answer false to #isForFormatting"
	| attributes emphasis ps cs |

	ps _ self paragraphStyle.
	emphasis _ 0.
	bold ifTrue: [ emphasis _ emphasis + 1 ].
	italic ifTrue: [ emphasis _ emphasis + 2 ].
	underline ifTrue: [ emphasis _ emphasis + 4 ].
	attributes _ Array streamContents: [ :strm |
		strm nextPut: (ParagraphStyleReference for: ps).
		(fontFamilyName = ps familyName and: [ fontPointSize = ps pointSize and: [ emphasis = ps emphasis and: [ currentFgColor = ps color "and: [kern = thisParagraphStyle kern ]"]]]) ifFalse: [
			cs _ CharacterStyle new
				privateFamilyName: fontFamilyName pointSize: fontPointSize emphasis: emphasis color: currentFgColor.
			strm nextPut: (CharacterStyleReference for: cs) ].
		nonFormattingAttributesOrNil ifNotNil: [ strm nextPutAll: nonFormattingAttributesOrNil ]].
	textStream nextPutAllString: aString withAttributes: attributes! !

!RTFStyledTextBuilder methodsFor: 'building-paragraph' stamp: 'jmv 3/13/2012 16:57'!
finishParagraph

	textStream
		nextPutAllString: String newLineString
		withAttributes: { ParagraphStyleReference for: self paragraphStyle }.
	paragraphStyleInUse _ nil.	"So we can build a new one"! !

!RTFStyledTextBuilder methodsFor: 'private' stamp: 'jmv 8/11/2011 10:40'!
paragraphStyle
	"We need a paragraph style to be actually used.
	If we don't have it, take whatever is in construction and use it."
	| emphasis |
	paragraphStyleInUse ifNil: [
		"Build it"
		emphasis _ 0.
		bold ifTrue: [ emphasis _ emphasis + 1 ].
		italic ifTrue: [ emphasis _ emphasis + 2 ].
		underline ifTrue: [ emphasis _ emphasis + 4 ].
		paragraphStyleInUse _ ParagraphStyle new.
		paragraphStyleInUse
			privateFamilyName: fontFamilyName pointSize: fontPointSize emphasis: emphasis color: currentFgColor
			alignment: align firstIndent: firstIndent+leftIndent restIndent: leftIndent
			"nil means that we didn't get any value from rtf. therefore, the +72 is not needed"
			rightIndent:  (rightIndent ifNil: [ 0 ] ifNotNil: [ rightIndent +72 ])
			spaceBefore: spaceBefore spaceAfter: spaceAfter ].
	^paragraphStyleInUse! !

!SampleListModel methodsFor: 'as yet unclassified' stamp: 'jmv 9/16/2009 10:35'!
initialize
	sel _ 3! !

!SampleListModel methodsFor: 'as yet unclassified' stamp: 'jmv 9/16/2009 14:39'!
list
	^#('first' 'second' 'third' 'first' 'second' 'third''first' 'second' 'third''first' 'second' 'third''first' 'second' 'third''first' 'second' 'third''first' 'second' 'third''first' 'second' 'third''first' 'second' 'third')! !

!SampleListModel methodsFor: 'as yet unclassified' stamp: 'jmv 9/16/2009 10:09'!
sel
^sel! !

!SampleListModel methodsFor: 'as yet unclassified' stamp: 'jmv 9/16/2009 10:35'!
sel:x
sel _ x.
self changed: #list

"
(PluggableListMorph
	on: SampleListModel new
	list: #list
	selected: #sel
	changeSelected: #sel:) autoDeselect: false; openInWorld
"! !

!StyledTextBuilder methodsFor: 'building' stamp: 'jmv 1/25/2011 14:32'!
, aString
	"Add aString to the Text we are building, using current style."
	textStream
		nextPutAllString: aString
		withAttributes:
			(characterStyleStack isEmpty
				ifTrue: [ #() ]
				ifFalse: [ {CharacterStyleReference for: (styleDict at: characterStyleStack last)} ])! !

!StyledTextBuilder methodsFor: 'building' stamp: 'jmv 3/13/2012 16:57'!
/ aParagraphStyleKey
	"Finish the current paragraph (adding a CR). Apply a ParagraphStyle to it. "
	textStream
		nextPutAllString: String newLineString
		withAttributes: {ParagraphStyleReference for: (styleDict at: aParagraphStyleKey)}! !

!StyledTextBuilder methodsFor: 'building' stamp: 'jmv 1/25/2011 14:31'!
< aCharacterStyleKey
	"Add a new CharacterStyle to the stack. This is the new current style.
	aKey is for accessing the styles dictionary that was given on setup."
	characterStyleStack addLast: aCharacterStyleKey! !

!StyledTextBuilder methodsFor: 'building' stamp: 'jmv 1/25/2011 14:32'!
> aString
	"We finished using the CharacterStyle that was last set. Remove it from the stack, to go on with the previous one. Then, add aString to the text being built."
	characterStyleStack removeLast.
	self , aString! !

!StyledTextBuilder methodsFor: 'building' stamp: 'jmv 1/25/2011 14:31'!
>/ aParagraphStyleKey
	"Stop using the CharacterStyle that was added last.
	Finish the current paragraph (adding a CR). Apply a ParagraphStyle to it. 
	The effect is the same as sending
		> ''
		/ aParagraphStyleKey"
	characterStyleStack removeLast.
	self / aParagraphStyleKey! !

!StyledTextBuilder methodsFor: 'building' stamp: 'jmv 1/25/2011 14:31'!
>< aCharacterStyleKey
	"Stop using the CharacterStyle that was added last, and start using a new one.
	The effect is the same as sending
		> ''
		< aCharacterStyleKey
	but the implementation is optimized"
	characterStyleStack at: characterStyleStack size put: aCharacterStyleKey! !

!StyledTextBuilder methodsFor: 'accessing' stamp: 'jmv 1/25/2011 14:32'!
styles: aDictionary
	styleDict _ aDictionary.
	characterStyleStack _ OrderedCollection new.
	textStream _ TextStream on: (Text string: (String new: 400))! !

!StyledTextBuilder methodsFor: 'accessing' stamp: 'jmv 9/9/2010 09:05'!
text
	^textStream contents! !

!StyledTextBuilder class methodsFor: 'examples' stamp: 'jmv 8/10/2011 10:44'!
example1
	"
	StyledTextBuilder example1
	"
	| model heading3 green14 red10Bold green11Italic t |
	model _ StyledTextModel new.
	heading3 _ model styleSet paragraphStyleNamed: 'Heading 3'.
	green14 _ model styleSet characterStyleNamed: 'Green 14'.
	red10Bold _ model styleSet characterStyleNamed: 'Red 10 bold'.
	green11Italic _ model styleSet characterStyleNamed: 'Green 11 Italic'.
	t _ Text
			buildWithStyles: ({
				#H3 -> heading3.
				#g14 -> green14.
				#r10b -> red10Bold.
				#g11i -> green11Italic
				} as: Dictionary)
			contents: [ :builder |
				builder , 'Starts as Heading 3. But later gets' < #g14 , ' big and green' , ' and stays like that. Then' >< #r10b , ' red & bold and inside this,' < #g11i , ' green and italic' > ' for a while. Later' > ' back to Heading 3.' / #H3].

	model contents: t.
	SystemWindow editFancierStyledText: model label: 'Styled Text Editor'! !

!StyledTextBuilder class methodsFor: 'examples' stamp: 'jmv 8/10/2011 10:43'!
example2
	"
	StyledTextBuilder example2
	"
	|model heading1 heading2 heading3 emphasized normal green14 red10Bold green11Italic t |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading2 _ model styleSet paragraphStyleNamed: 'Heading 2'.
	heading3 _ model styleSet paragraphStyleNamed: 'Heading 3'.
	emphasized _ model styleSet paragraphStyleNamed: 'Emphasized'.
	normal _ model styleSet paragraphStyleNamed: 'Normal'.
	green14 _ model styleSet characterStyleNamed: 'Green 14'.
	red10Bold _ model styleSet characterStyleNamed: 'Red 10 bold'.
	green11Italic _ model styleSet characterStyleNamed: 'Green 11 Italic'.
	t _ Text
			buildWithStyles: ({
				#H1 -> heading1.
				#H2 -> heading2.
				#H3 -> heading3.
				#E -> emphasized.
				#N -> normal.
				#g14 -> green14.
				#r10b -> red10Bold.
				#g11i -> green11Italic
				} as: Dictionary)
			contents: [ :builder |
				builder,
					'This is the main title' / #H1,
					'This is the title of level 2' / #H2,
					'This is title 3' / #H3,
					'Then, we have some normal text. But later gets' < #g14 , ' big and green' , ' and stays like that. Then' >< #r10b , ' red & bold and inside this,' < #g11i , ' green and italic' > ' for a while.' > ' Lets make this look like a paragraph, i.e. add enough text so that we''l be using more than one display line. Is this enough? Who knows... Let''s add a bit more. Maybe this is enough.' / #N,
					'This is an emphasized paragraph. It uses a Paragraph Style, not a Character Style, and therefore it has the same style all over the paragraph' / #E,
					'This' < #r10b, ' is' > ' again a' < #g14, ' title 2' >/ #H2,
					'And now another paragraph. This one may be smaller than previous ones' / #N ].
	model contents: t.
	SystemWindow editFancierStyledText: model label: 'Styled Text Editor'! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 2/14/2013 12:29'!
testClickAndHalf
	"
	StyledTextEditorTest new testClickAndHalf
	"
	| hand ev morph textModelMorph |
	hand _ HandMorph new.
	ev _ MouseButtonEvent new 
		setType: #mouseDown
		position: 10@10
		which: 4
		buttons: 4
		hand: hand
		stamp: nil.
	textModelMorph _ TextModelMorph withModel: (StyledTextModel new contents: '').
	textModelMorph openInWorld: PasteUpMorph someInstance.
	morph _ textModelMorph textMorph.
	morph world activeHand newKeyboardFocus: morph.
	morph mouseButton1Down: ev localPosition: 10@10.
	self assert: ((hand instVarNamed: 'mouseClickState') instVarNamed: 'clickAndHalfSelector') notNil description: 'Click-n-half should be handled'.
	textModelMorph delete! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 2/14/2013 12:29'!
testClickAndHalfSelection
	"
	StyledTextEditorTest new testClickAndHalfSelection
	"
	| morph model style text hand point1 |
	model _ StyledTextModel new.
	style _ model styleSet paragraphStyleNamed: 'Normal'.
	text _ Text string: 'This is a test.' attribute: (ParagraphStyleReference for: style).
	model contents: text.
	morph _ (PluggableStyledTextMorph withModel: model) openInWorld: PasteUpMorph someInstance.
	morph world activeHand  newKeyboardFocus: morph textMorph.

	point1 _ 60@20.
	hand _ HandMorph new.
	morph mouseButton1Down:
		(MouseButtonEvent new 
			setType: #mouseDown position: point1
			which: 4 buttons: 4 hand: hand stamp: nil)
			localPosition: point1.
	morph mouseButton1Up:
		(MouseButtonEvent new 
			setType: #mouseUp position: point1
			which: 4 buttons: 4 hand: hand stamp: nil)
			localPosition: point1.
	morph textMorph clickAndHalf:
		(MouseButtonEvent new 
			setType: nil position: point1
			which: 4 buttons: 4 hand: hand stamp: nil)
			localPosition: point1.
	self assert: morph editor selectionInterval = (1 to: 4) description: 'Click-n-half does not set selection properly.'.
	morph delete! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 2/14/2013 12:44'!
testDropDownKeyboardNavigation
	"
	StyledTextEditorTest new testDropDownKeyboardNavigation
	"
	| model style text window editorMorph listMorph |
	model _ StyledTextModel new.
	style _ model styleSet paragraphStyleNamed: 'Normal'.
	text _ Text string: 'x' attribute: (ParagraphStyleReference for: style).
	model contents: text.
	window _ SystemWindow editFancierStyledText: model label: 'Styled Text Editor'.
	window world ifNil: [ window openInWorld: PasteUpMorph someInstance ].
	editorMorph _ window findDeepSubmorphThat: [:m | m class = PluggableStyledTextMorph ] ifAbsent: nil.
	editorMorph _ editorMorph textMorph.
	listMorph _ window
		findDeepSubmorphThat: [ :any |
			any class = PluggableFilteringDropDownListMorph and: [
				(any instVarNamed: 'getListSelector') = #paragraphStyleNamesAndShortcuts ]]
		 ifAbsent: nil.
	listMorph openOrCloseList.
	(listMorph instVarNamed: 'editorMorph') contents: 'Norm'.
	self shouldnt: [ (listMorph instVarNamed: 'listMorph') verifyContents ] raise: MessageNotUnderstood.
	listMorph openOrCloseList.

	window delete! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 16:57'!
testEmpasizeScanner
	"Create a few instances of styled text (i.e. Text using various styles)
	and assert their properties
	StyledTextEditorTest new testEmpasizeScanner
	"
	
	| model style unstyled heading1 paragraph canvas scanner 
	boundsInWorld leftInRun line |
	unstyled _ Text string: 'Non-styled part. should use no paragraph style', String newLineString.
	model _ StyledTextModel new.
	style _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1 _ Text string: 'This should have the "Heading 1" style', String newLineString attribute: (ParagraphStyleReference for: style).

	paragraph _ Paragraph new.
	paragraph setModel: (model contents: unstyled, heading1); extentForComposing: 300@300.
	paragraph composeAll.
	
	canvas _ Display getCanvas.
	scanner _ MorphicScanner new 
			text: paragraph paragraphText
			foreground: Color black
			ignoreColorChanges: false.
	scanner canvas: canvas.
	
	self 
		assert: (scanner instVarNamed: 'paragraphStyle') isNil
		description: 'Incorrect initial value text style in scanner'.
	
"	canvas display: paragraph using: scanner in: (10@10 extent: 300@300)."
	boundsInWorld _ (10@10 extent: 300@300).
	leftInRun _ 0.
	(paragraph lineIndexForPoint: 0@0)
		to: (paragraph lineIndexForPoint: boundsInWorld extent)
		do: [ :i |
			line _ paragraph lines at: i.
			paragraph
				displaySelectionInLine: line
				on: canvas
				paragraphTopLeft: boundsInWorld topLeft
				selectionColor: Color blue.
			leftInRun _ scanner displayLine: line paragraphTopLeft: boundsInWorld topLeft leftInRun: leftInRun  ].
	
	self 
		assert: (scanner instVarNamed: 'paragraphStyle') = style 
		description: 'Incorrect initial value text style in scanner'.
		
	Display restore! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 12/5/2011 17:09'!
testEmptyLine
	"
	StyledTextEditorTest new testEmptyLine
	"
	
	| model t style morph block oldFont |

	model _ StyledTextModel new.
	style _ model styleSet paragraphStyleNamed: 'Normal'.
	t _ Text
			buildWithStyles: ({
				#N -> style.
				} as: Dictionary)
			contents: [ :builder |
				builder , 'Hi' / #N ].
	model contents: t.
	morph _ (TextModelMorph withModel: model) textMorph.
	block _ morph paragraph characterBlockAtPoint: 100@100.
	self assert: block width = 0.

	oldFont _ style font.
	style _ model styleSet paragraphStyleNamed: 'Emphasized'.
	morph editor pointIndex: model actualContents size + 1.
	morph editor markIndex: model actualContents size + 1.
	morph editor applyAttribute: (ParagraphStyleReference for: style).
	morph stylerStyled.
	self deny: morph editor lastFont = oldFont.
	self assert: morph editor lastFont = style font.
	block _ morph paragraph characterBlockAtPoint: 100@100.
	self assert: block width = 0.
	self assert: block left = style firstIndent.! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 12/5/2011 17:02'!
testEmptyText
	"
	StyledTextEditorTest new testEmptyText
	"
	
	| model morph block style oldFont |

	model _ StyledTextModel new contents: '' asText.
	morph _ (TextModelMorph withModel: model) textMorph.
	block _ morph paragraph characterBlockAtPoint: 0@0.
	style _ model styleSet paragraphStyleNamed: 'Normal'.
	self assert: block width = 0.
	self assert: block left = style firstIndent.

	oldFont _ style font.
	style _ model styleSet paragraphStyleNamed: 'Emphasized'.
	morph editor applyAttribute: (ParagraphStyleReference for: style).
	morph editor pointIndex: model actualContents size + 1.
	morph stylerStyled.
	self deny: morph editor lastFont = oldFont.
	self assert: morph editor lastFont = style font.
	block _ morph paragraph characterBlockAtPoint: 0@0.
	self assert: block width = 0.
	self assert: block left = style firstIndent.! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 2/14/2013 12:15'!
testEmptyTextClick
	"
	StyledTextEditorTest new testEmptyTextClick
	"
	
	| model morph block style oldFont evt |

	model _ StyledTextModel new contents: '' asText.
	morph _ (TextModelMorph withModel: model) textMorph.
	block _ morph paragraph characterBlockAtPoint: 0@0.
	style _ model styleSet paragraphStyleNamed: 'Normal'.
	self assert: block width = 0.
	self assert: block left = style firstIndent.

	oldFont _ style font.
	style _ model styleSet paragraphStyleNamed: 'Emphasized'.
	morph editor applyAttribute: (ParagraphStyleReference for: style).
	morph editor pointIndex: model actualContents size + 1.
	
	evt _ MouseEvent new setType: #mouseMove position: 100@100 buttons: 0 hand: nil.
	morph editor mouseButton1Down: evt localPosition: 100@100.
	morph editor mouseButton1Up: evt
			localPosition: 100@100.
	
	self deny: morph editor lastFont = oldFont.
	self assert: morph editor lastFont = style font.
	block _ morph paragraph characterBlockAtPoint: 0@0.
	self assert: block width = 0.
	self assert: block left = style firstIndent.! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 16:57'!
testEmptyTrailingLine
	"Test that the special case of an artificial empty last line behaves as normal lines.
	StyledTextEditorTest new testEmptyTrailingLine
	"
	
	| emptyText notEmptyText model style paragraph canvas form1 form2 |
	emptyText _ Text string: ''.
	model _ StyledTextModel new.
	style _ model styleSet paragraphStyleNamed: 'Normal'.
	notEmptyText _ Text string: ' ', String newLineString attribute: (ParagraphStyleReference for: style).
	form1 _ Form extent: 100@50 depth: 32.
	form1 fillWhite.
	form2 _ Form extent: 100@50 depth: 32.
	form2 fillWhite.

	paragraph _ Paragraph new.

	canvas _ form1 getCanvas.
	paragraph setModel: (model contents: emptyText); extentForComposing: 300@300.
	paragraph composeAll.
	paragraph selectionStartBlocks: {paragraph defaultCharacterBlock} selectionStopBlocks: {paragraph defaultCharacterBlock}.
	canvas paragraph: paragraph bounds:  (10@10 extent: 300@300) color: Color black selectionColor: Color blue.

	canvas _ form2 getCanvas.
	paragraph  setModel: (model contents: notEmptyText); extentForComposing: 300@300.
	paragraph composeAll.
	paragraph selectionStartBlocks: {paragraph defaultCharacterBlock} selectionStopBlocks: {paragraph defaultCharacterBlock}.
	canvas paragraph: paragraph bounds:  (10@10 extent: 300@300) color: Color black selectionColor: Color blue.

	form1 addDeltasFrom: form2.
	self assert: form1 primCountBits = 0 description: 'Incorrect insertion point mark for empty text.'! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 16:57'!
testEmptyTrailingLineNumberedStyle
	"Test that the special case of an artificial empty last line behaves as normal lines.
	StyledTextEditorTest new testEmptyTrailingLineNumberedStyle
	"
	
	| emptyText emptyTextModel notEmptyText model style paragraph canvas form1 form2 editor inner |
	emptyText _ Text string: ''.
	model _ StyledTextModel new.
	style _ model styleSet paragraphStyleNamed: 'Numbered List'.
	notEmptyText _ Text string: ' ', String newLineString attribute: (ParagraphStyleReference for: style).
	form1 _ Form extent: 100@50 depth: 32.
	form1 fillWhite.
	form2 _ Form extent: 100@50 depth: 32.
	form2 fillWhite.

	paragraph _ Paragraph new.

	canvas _ form1 getCanvas.
	editor _ TextEditor new.
	emptyTextModel _ model contents: emptyText.
	paragraph setModel: emptyTextModel; extentForComposing: 300@300.
	paragraph composeAll.
	inner _ InnerTextMorph new.
	editor morph: inner.
	inner privateOwner: TextModelMorph new.
	editor model: emptyTextModel.
	editor instVarNamed: 'paragraph' put: paragraph.
	paragraph editor: editor.
	editor resetState.
	editor instVarNamed: 'emphasisHere' put: ({ParagraphStyleReference for: style}).
	"Set model again, so editor emhasisHere is considered."
	paragraph setModel: (model contents: emptyText); extentForComposing: 300@300.
	paragraph composeAll.
	paragraph selectionStartBlocks: {paragraph defaultCharacterBlock} selectionStopBlocks: {paragraph defaultCharacterBlock}.
	canvas paragraph: paragraph bounds:  (10@10 extent: 300@300) color: Color black selectionColor: Color blue.

	canvas _ form2 getCanvas.
	paragraph setModel: (model contents: notEmptyText); extentForComposing: 300@300.
	paragraph composeAll.
	paragraph selectionStartBlocks: {paragraph defaultCharacterBlock} selectionStopBlocks: {paragraph defaultCharacterBlock}.
	canvas paragraph: paragraph bounds:  (10@10 extent: 300@300) color: Color black selectionColor: Color blue.

	form1 addDeltasFrom: form2.
	self assert: form1 primCountBits = 0 description: 'Incorrect insertion point mark for empty text.'! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 16:58'!
testKeepParagraphStyleOnDeleteAll
	"
	StyledTextEditorTest new testKeepParagraphStyleOnDeleteAll
	"
	| editor paragraph model heading1 heading1Text |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading', String newLineString attribute: (ParagraphStyleReference for: heading1).	"Include a cr, so our paragraphStyle is applied to resulting paragraph"

	model contents: heading1Text.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor selectAll.
	self assert: editor currentParagraphStyle = heading1 description: 'Please check this test'.
	editor cut.
	self assert: editor currentParagraphStyle = heading1 description: 'Paragraph style should not be lost because of deletion'! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 6/9/2011 16:32'!
testRecentClipping
	"
	StyledTextEditorTest new testRecentClipping
	"
	Clipboard default: Clipboard new.
	self shouldnt: [ Clipboard chooseRecentClipping ] raise: Error! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 16:58'!
testSelectStyle
	"Create a few instances of styled text (i.e. Text using various styles)
	and assert their properties
	StyledTextEditorTest new testSelectStyle
	"
	
	| model normal heading1 heading2 unstyled heading1Text editor paragraph text |
	model _ StyledTextModel new.
	normal _ model styleSet paragraphStyleNamed: 'Normal'.
	unstyled _ Text string: 'Part with "Normal" style', String newLineString attribute: (ParagraphStyleReference for: normal).
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading2 _ model styleSet paragraphStyleNamed: 'Heading 2'.
	heading1Text _ Text string: 'This should have the "Heading 1" style', String newLineString attribute: (ParagraphStyleReference for: heading1).
	text _ unstyled, heading1Text.

	model contents: text.
	text _ model actualContents.	"Might have created a new instance"
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.
	
	editor pointIndex: 2.
	editor setEmphasisHereFromText.
	self
		assert: (editor instVarNamed: 'emphasisHere') first style = normal
		description: 'Incorrect style at insertion point'.

	editor markIndex: 26 pointIndex: 28.
	editor setEmphasisHereFromText.
	self
		assert: (editor instVarNamed: 'emphasisHere') first style = heading1
		description: 'Incorrect style at insertion point'.
	
	editor applyAttribute: (ParagraphStyleReference for: heading2).
	editor setEmphasisHereFromText.
	self
		assert: (editor instVarNamed: 'emphasisHere') first style = heading2
		description: 'Should have new style at selection point'.
	
	self
		assert: (text paragraphStyleOrNilAt: 1) = normal
		description: 'Should have no style at beginning of text'.
	self
		assert: (text paragraphStyleOrNilAt: unstyled size - 1) = normal
		description: 'Should have no style in previous line'.
	self
		assert: (text paragraphStyleOrNilAt: unstyled size) = normal
		description: 'Should have no style in previous line'.
	self
		assert: (text paragraphStyleOrNilAt: unstyled size + 1) = heading2
		description: 'Should have new style affected line'.! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 2/14/2013 12:33'!
testShiftClickSelection
	"
	StyledTextEditorTest new testShiftClickSelection
	"
	| morph model hand editor point1 point2 p textMorph text style |
	model _ StyledTextModel new.
	style _ model styleSet paragraphStyleNamed: 'Normal'.
	text _ Text string: 'This is a test.' attribute: (ParagraphStyleReference for: style).
	model contents: text.
	morph _ (PluggableStyledTextMorph withModel: model) openInWorld: PasteUpMorph someInstance.
	morph world activeHand newKeyboardFocus: morph textMorph.

	point1 _ 64@20.
	point2 _ 110@20.
	hand _ HandMorph new.
	textMorph _ morph textMorph.
	p _  textMorph morphPosition.
	morph mouseButton1Down:
		(MouseButtonEvent new 
			setType: #mouseDown position: point1
			which: 4 buttons: 4 hand: hand stamp: nil)
			localPosition: point1-p.
	morph mouseButton1Up:
		(MouseButtonEvent new 
			setType: #mouseUp position: point1
			which: 4 buttons: 4 hand: hand stamp: nil)
			localPosition: point1-p.
	morph mouseButton1Down:
		(MouseButtonEvent new 
			setType: #mouseDown position: point2
			which: 4 buttons: 12 hand: hand stamp: nil)
			localPosition: point2-p.
	morph mouseButton1Up:
		(MouseButtonEvent new 
			setType: #mouseUp position: point2
			which: 4 buttons: 12 hand: hand stamp: nil)
			localPosition: point2-p.
	editor _ textMorph editor.
	self assert: (editor startBlock containsPoint: (point1 - p)) description: 'Incorrect selection when shift-click'.
	self assert: (editor stopBlock left > (point2 - p)) description: 'Incorrect selection when shift-click'.
	morph delete! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 16:58'!
testStylesInPaste1
	"
	StyledTextEditorTest new testStylesInPaste1
	"
	| heading1 heading1Text simpleText editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading', String newLineString attribute: (ParagraphStyleReference for: heading1).	"Include a cr, so our paragraphStyle is applied to resulting paragraph"
	simpleText _ 'This is a text without style' asText.

	initialText _ simpleText copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	Clipboard storeObject: heading1Text. editor paste.		"Extends heading 1 to beginning, runs must be compacted"
	concatenation _ editor text.
	self assert: concatenation runs runs size = 1 description: 'Should apply the "Heading 1" paragraph style to the whole text'.
	self assert: concatenation runs first size = 1 description: 'Should apply just the "Heading 1" paragraph style to the whole text'.
	self assert: concatenation runs first first style == heading1 description: 'Should apply the "Heading 1" paragraph style to the whole text'! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 08:56'!
testStylesInPaste2
	"
	StyledTextEditorTest new testStylesInPaste2
	"
	| heading1 heading1Text simpleText editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading', String newLineString attribute: (ParagraphStyleReference for: heading1).	"Include a cr, so our paragraphStyle is applied to resulting paragraph"
	simpleText _ 'This is a text without style' asText.

	initialText _ simpleText copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	Clipboard storeObject: String newLineString. editor paste.	
	Clipboard storeObject: heading1Text. editor paste.		"Extends heading 1 to beginning, runs must be compacted"
	concatenation _ editor text.
	self assert: concatenation runs runs size = 2 description: 'Should have 2 parts with different attributes'.
"	self assert: concatenation runs runs first = (simpleText size + 1) description: 'Should apply an empty paragraph style to the first part'."
	self assert: concatenation runs runs second = heading1Text size description: 'Should apply the "Heading 1" paragraph style to the second part'.
"	self assert: concatenation runs values first size = 1 description: 'Should apply an empty paragraph style to the first part'."
"	self assert: (concatenation runs values first first style isEquivalentTo: ParagraphStyle new) description: 'Should apply an empty paragraph style to the first part'."
	self assert: concatenation runs values second size = 1 description: 'Should apply the "Heading 1" paragraph style to the second part'.
	self assert: concatenation runs values second first style == heading1 description: 'Should apply the "Heading 1" paragraph style to the second part'! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 12/19/2011 12:19'!
testStylesInPaste3
	"
	StyledTextEditorTest new testStylesInPaste3
	"
	| heading1Text simpleText editor paragraph concatenation initialText model heading1 |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading' attribute: (ParagraphStyleReference for: heading1).
	simpleText _ 'This is a text without style' asText.

	initialText _ heading1Text copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	Clipboard storeObject: simpleText. editor paste.		"Extends style to beginning. I.e., lose the style, style comes from paragraph end"
	concatenation _ editor text.
	self assert: concatenation runs runs size = 1 description: 'Should apply no character style to the whole text'.
	self assert: concatenation runs first first style == heading1 description: 'Should apply just the "Heading 1" paragraph style to the whole text'! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 16:59'!
testStylesInPaste4
	"
	StyledTextEditorTest new testStylesInPaste4
	"
	| heading1Text heading1 simpleText editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading' attribute: (ParagraphStyleReference for: heading1).
	simpleText _ 'This is a text without style' asText.

	initialText _ heading1Text copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	"Enter styled text. Then press return and paste text without style.
	The paragraph style in use should be that of the first part."
	Clipboard storeObject: String newLineString. editor paste.
	Clipboard storeObject: simpleText. editor paste.
	concatenation _ editor text.
	self assert: concatenation runs runs size = 2 description: 'Should use imported RTF style'.
	self assert: concatenation runs runs first = (heading1Text size +1) description: 'Should apply the "Heading 1" paragraph style to the first part'.
	self assert: concatenation runs runs second = simpleText size description: 'Should apply the "Normal" paragraph style to the second part'.
	self assert: concatenation runs values first first style name = 'Heading 1'.
	self assert: concatenation runs values second first style name = 'Normal'! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 16:58'!
testStylesInPaste5
	"
	StyledTextEditorTest new testStylesInPaste5
	"
	| heading1Text heading1 simpleText editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading' attribute: (ParagraphStyleReference for: heading1).
	simpleText _ 'This is a text without style' asText.

	initialText _ heading1Text copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	"Enter styled text. Then press return and paste a simple string.
	The paragraph style in use should be that of the first part."
	Clipboard storeObject: String newLineString. editor paste.
	Clipboard storeObject: simpleText asString. editor paste.
	concatenation _ editor text.
	self assert: concatenation runs runs size = 1 description: 'Should apply same attributes to the whole text'.
	self assert: concatenation runs runs first = concatenation size description: 'Should apply the "Heading 1" paragraph style to all the text'.! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 16:59'!
testStylesInPaste6
	"
	StyledTextEditorTest new testStylesInPaste6
	"
	| heading1 heading1Text heading2 heading2Text editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading 1' attribute: (ParagraphStyleReference for: heading1).
	heading2 _ model styleSet paragraphStyleNamed: 'Heading 2'.
	heading2Text _ Text string: 'This is a text without style', String newLineString attribute: (ParagraphStyleReference for: heading2).

	initialText _ heading1Text copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	Clipboard storeObject: String newLineString. editor paste.
	"This is an almost trivial case. just check that both parts are as they were at the beginning"	
	Clipboard storeObject: heading2Text. editor paste.
	concatenation _ editor text.
	self assert: concatenation runs runs size = 2 description: 'Should have 2 parts with different attributes'.
	self assert: concatenation runs runs first = (heading1Text size + 1) description: 'Should apply the "Heading 1" paragraph style to the first part'.
	self assert: concatenation runs runs second = heading2Text size description: ''.
	self assert: concatenation runs values first size = 1 description: 'Should apply the "Heading 1" paragraph style to the first part'.
	self assert: concatenation runs values first first style == heading1 description: 'Should apply the "Heading 1" paragraph style to the first part'.
	self deny: concatenation runs values second first style == heading1 description: 'Should not apply the "Heading 1" paragraph style to the second part'! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 12/6/2011 10:09'!
testStylesInPaste7
	"
	StyledTextEditorTest new testStylesInPaste7
	"
	| heading1 heading1Text simpleText editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading' attribute: (ParagraphStyleReference for: heading1).	"Do not include a cr"
	simpleText _ 'This is a text without style' asText.

	initialText _ simpleText copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor selectAll.
	Clipboard storeObject: heading1Text. editor paste.		"Extends heading 1 to beginning, runs must be compacted"
	concatenation _ editor text.
	self assert: concatenation runs runs size = 1 description: 'Should apply the "Heading 1" paragraph style to the whole text, as no part of original text is left.'.
	self assert: concatenation runs first size = 1 description: 'Should apply just the "Heading 1" paragraph style to the whole text, as no part of original text is left.'.
	self assert: concatenation runs first first style == heading1 description: 'Should apply the "Heading 1" paragraph style to the whole text, as no part of original text is left.'! !

!StyledTextEditorTest methodsFor: 'tests' stamp: 'jmv 1/13/2012 13:35'!
testStylesInPaste8
	"
	Not exactly a Paste operation, but #replaceSelectionWith: , as done by the autocompletter
	StyledTextEditorTest new testStylesInPaste8
	"
	| simpleText editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	simpleText _ (Text
		string: 'Aardvark'
		attribute: (CharacterStyleReference new style: model styleSet autoCompletedStyle)), ' '.

	initialText _ '' asText.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	editor replaceSelectionWith: simpleText.
	concatenation _ editor text.
	self
		assert: (concatenation runs values allSatisfy: [ :attrs | attrs anySatisfy: [ :att | att class == ParagraphStyleReference ]])
		description: 'Must have ParagraphStyle everywhere!!'! !

!StyledTextModelTest methodsFor: 'as yet unclassified' stamp: 'bp 4/22/2012 12:14'!
tearDown
	FileDirectory default deleteFileNamed: 'StyledTextModelTest.object'! !

!StyledTextModelTest methodsFor: 'as yet unclassified' stamp: 'bp 4/22/2012 13:15'!
testFromFileNamed
	| model1 model2 model3 |
	model1 _ StyledTextModel new contents: 'Some text' asText.
	model1 saveAs: 'StyledTextModelTest'.
	model2 _ StyledTextModel fromFileNamed: 'StyledTextModelTest.object'.
	self assert: model1 actualContents equals: model2 actualContents.
	model3 _ StyledTextModel fromFileNamed: 'StyledTextModelTest'.
	self assert: model1 actualContents equals: model3 actualContents! !

!StyledTextModelTest methodsFor: 'as yet unclassified' stamp: 'bp 4/22/2012 12:19'!
testSave
	| model |
	model _ StyledTextModel new.
	self deny: (FileDirectory default fileExists: 'StyledTextModelTest.object').
	model saveAs: 'StyledTextModelTest'.
	self assert: (FileDirectory default fileExists: 'StyledTextModelTest.object')! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 12/20/2011 12:41'!
testAsStyledAsNonStyledSample5
	"
	StyledTextTest new testAsStyledAsNonStyledSample5Bis
	"
	| styled unstyled model style |
	unstyled _ RTFConversionTest textSample5.
	styled _ unstyled asStyledTextWith: StyleSet sample.
	model _ StyledTextModel new.
	style _ model styleSet paragraphStyleNamed: 'Heading 1'.
	styled addAttribute: (ParagraphStyleReference for: style).
	self assert: (unstyled attributesAt: 7) first class = TextAnchor.	"Or test is broken"
	self assert: ((styled attributesAt: 7) anySatisfy: [ :att | att class = TextAnchor])! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 16:59'!
testCharacterStyleConcatenation
	"
	self new testCharacterStyleConcatenation
	"
	| model normal normalText1 red10Bold simpleColoredText normalText3 concatenation5 concatenation6 |
	model _ StyledTextModel new.
	normal _ model styleSet paragraphStyleNamed: 'Normal'.
	normalText1 _ Text string: 'This is just ' attribute: (ParagraphStyleReference for: normal).
	red10Bold _ model styleSet characterStyleNamed: 'Red 10 bold'.
	simpleColoredText _ Text string: 'colored' attribute: (CharacterStyleReference for: red10Bold).
	normalText3 _ Text string: ' normal text', String newLineString attribute: (ParagraphStyleReference for: normal).	"End with a cr, so our paragraph style is applied all over the resulting paragraph"
	concatenation5 _ normalText1, simpleColoredText, normalText3.

	self assert: concatenation5 runs runs size = 3 description: 'Should have 3 parts with distinct CharacterStyles'.
	self assert: concatenation5 runs runs first = normalText1 size description: 'Should apply no character style to the first part'.
	self assert: concatenation5 runs values first size = 1 description: 'Should apply no character style to the first part'.

	self assert: concatenation5 runs runs second = simpleColoredText size description: 'Should apply "Red 10 bold" style to the second part'.
	self assert: concatenation5 runs values second size = 2 description: 'Should apply "Red 10 bold" style to the second part'.
	self assert: concatenation5 runs values second second style == red10Bold description: 'Should apply "Red 10 bold" style to the second part'.

	self assert: concatenation5 runs runs third = normalText3 size description: 'Should apply no character style to the third part'.
	self assert: concatenation5 runs values third size = 1 description: 'Should apply no character style to the third part'.

	concatenation6 _ simpleColoredText, normalText3.
	self assert: concatenation6 runs runs size = 2 description: 'Should have 2 parts with distinct CharacterStyles'.

	self assert: concatenation6 runs runs first = simpleColoredText size description: 'Should apply "Red 10 bold" style to the second part'.
	self assert: concatenation6 runs values first size = 2 description: 'Should apply "Red 10 bold" style to the second part'.
	self assert: concatenation6 runs values first second style == red10Bold description: 'Should apply "Red 10 bold" style to the second part'.

	self assert: concatenation6 runs runs second = normalText3 size description: 'Should apply no character style to the third part'.
	self assert: concatenation6 runs values second size = 1 description: 'Should apply no character style to the third part'! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 11/1/2011 09:49'!
testOldInstanceDeserialization
	"
	StyledTextTest new testOldInstanceDeserialization
	"
	| newInstance oldFormat |
	"Evaluated in an image without 'undoRedoCommands' ivar"
	"
	oldInstance _ StyledTextModel withText: 'This is a text'.
	(SmartRefStream streamedRepresentationOf: oldInstance) inspect.
	"
	oldFormat _ '      class structure	   
Dictionary      0	   AssociationObject        ?SequenceableCollection        ?StyledTextModel       	actionMapactualContents ?Point       xy ?RunArray       runsvalues	lastIndexlastRun
lastOffsetcanJoinMessage ?ActiveModel       
   ¥ ?Symbol        ?ArrayedCollection        ?Number        ?	TextModel       
   ¥
   ° ?Text       stringruns ?	Rectangle       origincorner ?ProtoObject        ?	Magnitude        ?MessageSend       receiverselector	arguments ?String        ?LargePositiveInteger        ?
Collection        ?Array        ?Integer       superclasses        0 ?
   L
  " ?
   c
  Ì ?
   Š
  ² ?
   Î
   L ?
   î
  { ?
  D
   L ?
  d
  ’ ?
  {
   c ?
  œ
  ? ?
  ²
  D ?
  ×
  { ?
  ù
   L ?
  "nil ?
  ?
   L ?
  X
   L ?
  ’
  { ?
  ¨
  û ?
  Ì
   L ?
  æ
  { ?
  û
  œ Š×This is a text î             X
  ecanJoin:and:    !!

'.
	newInstance _ SmartRefStream unStream: oldFormat.
	self shouldnt: [ newInstance undoRedoCommands ] raise: Exception.
	! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 09:49'!
testSample1ToAndFromRTFClipboardStyle
	"
	StyledTextTest new testSample1ToAndFromRTFClipboardStyle
	"
	| text text2 |
	text _ RTFConversionTest textSample1.

	"This will use a refStreamed object"
	Clipboard default storeObject: text.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	self assert: text2 runs = text runs.

	"This will also test storing and retrieving RTF data from platform Clipboard, and RTF conversion"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: text rtfString dataFormat: 'public.rtf'.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	self assert: text2 asNonStyledText runs = text runs.! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 09:50'!
testSample2ToAndFromRTFClipboardStyle
	"
	StyledTextTest new testSample2ToAndFromRTFClipboardStyle
	"
	| text text2 |
	text _ RTFConversionTest textSample2.

	"This will use a refStreamed object"
	Clipboard default storeObject: text.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	self assert: text2 runs = text runs.

	"This will also test storing and retrieving RTF data from platform Clipboard, and RTF conversion"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: text rtfString dataFormat: 'public.rtf'.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	self assert: text2 asNonStyledText runs = text runs.! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 09:50'!
testSample3ToAndFromRTFClipboardStyle
	"
	StyledTextTest new testSample3ToAndFromRTFClipboardStyle
	"
	| text text2 |
	text _ RTFConversionTest textSample3.

	"This will use a refStreamed object"
	Clipboard default storeObject: text.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	self assert: text2 runs = text runs.

	"This will also test storing and retrieving RTF data from platform Clipboard, and RTF conversion"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: text rtfString dataFormat: 'public.rtf'.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	self assert: text2 asNonStyledText runs = text runs.! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 09:50'!
testSample4ToAndFromRTFClipboardStyle
	"
	StyledTextTest new testSample4ToAndFromRTFClipboardStyle
	"
	| text text2 |
	text _ RTFConversionTest textSample4.

	"This will use a refStreamed object"
	Clipboard default storeObject: text.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	self assert: text2 runs = text runs.

	"This will also test storing and retrieving RTF data from platform Clipboard, and RTF conversion"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: text rtfString dataFormat: 'public.rtf'.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	"kern and struckThrough not supported in rtf yet..."
"	self assert: text2 asNonStyledText runs = text runs."
"	self assert: text2 usesOnlyStyles description: 'If not using the RTFStyledTextBuilder, next assert makes no sense.'."
	"kern and struckThrough not supported in rtf yet..."
"	self assert: text2 runs = text asStyledText runs"! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 09:50'!
testSample5ToAndFromRTFClipboardStyle
	"
	StyledTextTest new testSample5ToAndFromRTFClipboardStyle
	"
	| text text2 |
	text _ RTFConversionTest textSample5.

	"This will use a refStreamed object"
	Clipboard default storeObject: text.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	self assert: text2 runs = text runs.

	"This will also test storing and retrieving RTF data from platform Clipboard, and RTF conversion"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: text rtfString dataFormat: 'public.rtf'.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	self assert: text2 asNonStyledText runs = text runs.! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 09:50'!
testSample6ToAndFromRTFClipboardStyle
	"
	StyledTextTest new testSample6ToAndFromRTFClipboardStyle
	"
	| text text2 |
	text _ RTFConversionTest textSample6.

	"This will use a refStreamed object"
	Clipboard default storeObject: text.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	self assert: text2 runs = text runs.

	"This will also test storing and retrieving RTF data from platform Clipboard, and RTF conversion"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: text rtfString dataFormat: 'public.rtf'.
	text2 _ Clipboard default retrieveObject.
	self assert: text2 = text.
	self assert: text2 asNonStyledText runs = text runs.! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 17:01'!
testStyledText
	"Create a few instances of styled text (i.e. Text using various styles)
	and assert their properties
	StyledTextTest new testStyledText
	"
	
	| model heading1 heading2 heading3 normal unstyledText heading1Text heading2Text heading3Text normalText multiText |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading2 _ model styleSet paragraphStyleNamed: 'Heading 2'.
	heading3 _ model styleSet paragraphStyleNamed: 'Heading 3'.
	normal _ model styleSet paragraphStyleNamed: 'Normal'.
	unstyledText _ Text string: 'Non-styled part. should use no style', String newLineString.
	heading1Text _ Text string: ('This should have the "Heading 1" style', String newLineString) attribute: (ParagraphStyleReference for: heading1).
	heading2Text _ Text string: ('This should have the "Heading 2" style', String newLineString) attribute: (ParagraphStyleReference for: heading2).
	heading3Text _ Text string: ('This should have the "Heading 3" style', String newLineString) attribute: (ParagraphStyleReference for: heading3).
	normalText _ Text string: ('This should have the "Normal" style', String newLineString) attribute: (ParagraphStyleReference for: normal).
	multiText _ unstyledText, heading1Text, heading2Text, heading3Text, normalText, unstyledText.

	self
		assert: multiText string = (unstyledText string, heading1Text string, heading2Text string, heading3Text string, normalText string, unstyledText string)
		description: 'It seems concatenating texts breaks the string'.
	self
		assert: (unstyledText paragraphStyleOrNilAt: 3) isNil
		description: 'initialStyle failing'.
	self
		assert: (multiText paragraphStyleOrNilAt: 3) isNil
		description: 'initialStyle failing'.
	self
		assert: (heading1Text paragraphStyleOrNilAt: 3) = heading1
		description: 'initialStyle failing'.
	self
		assert: (multiText paragraphStyleOrNilAt: 60) = heading1
		description: 'Incorrect ParagraphStyle in styled text'.
	self
		assert: (multiText paragraphStyleOrNilAt: multiText size) isNil
		description: 'Incorrect ParagraphStyle in styled text'! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 17:01'!
testStyledTextAndString
	"Create a few instances of styled text (i.e. Text using various styles)
	and assert their properties
	StyledTextTest new testStyledTextAndString
	"
	
	| model style heading1 multi string |
	string _ 'Just a String '.
	model _ StyledTextModel new.
	style _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1 _ Text string: 'This should have the "Heading 1" style', String newLineString attribute: (ParagraphStyleReference for: style).
	multi _ string, heading1.
	self
		assert: multi class = Text
		description: 'Concatenation of String and Text should answer Text'.
	self
		assert: multi string = (string, heading1 string)
		description: 'It seems concatenating texts breaks the string'.
	self
		assert: (heading1 paragraphStyleOrNilAt: 3) = style
		description: 'ParagraphStyle not properly set'.
	self
		assert: (multi paragraphStyleOrNilAt: 3) = style
		description: 'ParagraphStyle not properly set'.! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/13/2012 17:01'!
testStyledTextBuilder
	"
	Test building with nested CharacterStyles
	self new testStyledTextBuilder
	"
	| model heading3 green14 red10Bold green11Italic t |
	model _ StyledTextModel new.
	heading3 _ model styleSet paragraphStyleNamed: 'Heading 3'.
	green14 _ model styleSet characterStyleNamed: 'Green 14'.
	red10Bold _ model styleSet characterStyleNamed: 'Red 10 bold'.
	green11Italic _ model styleSet characterStyleNamed: 'Green 11 Italic'.
	t _ Text
			buildWithStyles: ({
				#H3 -> heading3.
				#g14 -> green14.
				#r10b -> red10Bold.
				#g11i -> green11Italic
				} as: Dictionary)
			contents: [ :builder |
				builder , 'Starts as Heading 3. But later gets' < #g14 , ' big and green' , ' and stays like that. Then' >< #r10b , ' red & bold and inside this,' < #g11i , ' green and italic' > ' for a while. Later' > ' back to Heading 3.' / #H3].

	"
	t inspect.
	t edit.
	"
	
	self assert: t string = ('Starts as Heading 3. But later gets big and green and stays like that. Then red & bold and inside this, green and italic for a while. Later back to Heading 3.', String newLineString) description: 'Broken string built.'.

	self assert: (t paragraphStyleOrNilAt: 12) name = 'Heading 3' description: 'Wrong paragraph style'.
	self assert: (t characterStyleOrNilAt: 2) isNil description: 'Should not have a char style yet'.
	self assert: (t characterStyleOrNilAt: 40) name = 'Green 14' description: 'Wrong charStyle'.
	self assert: (t characterStyleOrNilAt: 75) name = 'Green 14' description: 'Wrong charStyle'.
	self assert: (t characterStyleOrNilAt: 76) name = 'Red 10 bold' description: 'Wrong charStyle'.
	self assert: (t characterStyleOrNilAt: 103) name = 'Red 10 bold' description: 'Wrong charStyle'.
	self assert: (t characterStyleOrNilAt: 104) name = 'Green 11 Italic' description: 'Wrong charStyle'.
	self assert: (t characterStyleOrNilAt: 120) name = 'Green 11 Italic' description: 'Wrong charStyle'.
	self assert: (t characterStyleOrNilAt: 121) name = 'Red 10 bold' description: 'Wrong charStyle'.
	self assert: (t characterStyleOrNilAt: 139) name = 'Red 10 bold' description: 'Wrong charStyle'.
	self assert: (t characterStyleOrNilAt: 140) isNil description: 'Should not have a char style any longer'.
	self assert: (t characterStyleOrNilAt: t size) isNil description: 'Should not have a char style any longer'.! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 8/10/2011 10:43'!
testStyledTextBuilder2
	"
	Test several paragraphs, with various ParagraphStyles
	self new testStyledTextBuilder2
	"
	|model heading1 heading2 heading3 emphasized normal green14 red10Bold green11Italic t |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading2 _ model styleSet paragraphStyleNamed: 'Heading 2'.
	heading3 _ model styleSet paragraphStyleNamed: 'Heading 3'.
	emphasized _ model styleSet paragraphStyleNamed: 'Emphasized'.
	normal _ model styleSet paragraphStyleNamed: 'Normal'.
	green14 _ model styleSet characterStyleNamed: 'Green 14'.
	red10Bold _ model styleSet characterStyleNamed: 'Red 10 bold'.
	green11Italic _ model styleSet characterStyleNamed: 'Green 11 Italic'.
	t _ Text
			buildWithStyles: ({
				#H1 -> heading1.
				#H2 -> heading2.
				#H3 -> heading3.
				#E -> emphasized.
				#N -> normal.
				#g14 -> green14.
				#r10b -> red10Bold.
				#g11i -> green11Italic
				} as: Dictionary)
			contents: [ :builder |
				builder,
					'This is the main title' / #H1,
					'This is the title of level 2' / #H2,
					'This is title 3' / #H3,
					'Then, we have some normal text. But later gets' < #g14 , ' big  and green' , ' and stays like that. Then' >< #r10b , ' red & bold and inside this,' < #g11i , ' green and italic' > ' for a while.' > ' Lets make this look like a paragraph, i.e. add enough text so that we''l be using more than one display line. Is this enough? Who knows... Let''s add a bit more. Maybe this is enough.' / #N,
					'This is an emphasized paragraph. It uses a Paragraph Style, not a Character Style, and therefore it has the same style all over the paragraph' / #E,
					'This' < #r10b, ' is' > ' again a' < #g14, ' title 2' >/ #H2,
					'And now another paragraph. This one may be smaller than previous ones' / #N ].

	"
	model contents: t.
	SystemWindow editFancierStyledText: model label: 'Styled Text Editor'
	"
	
	self assert: t string =  'This is the main title
This is the title of level 2
This is title 3
Then, we have some normal text. But later gets big  and green and stays like that. Then red & bold and inside this, green and italic for a while. Lets make this look like a paragraph, i.e. add enough text so that we''l be using more than one display line. Is this enough? Who knows... Let''s add a bit more. Maybe this is enough.
This is an emphasized paragraph. It uses a Paragraph Style, not a Character Style, and therefore it has the same style all over the paragraph
This is again a title 2
And now another paragraph. This one may be smaller than previous ones
' description: 'Broken string built.'.

	self assert: (t paragraphStyleOrNilAt: 10) name = 'Heading 1' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 10) isNil description: 'Should not have a char style yet'.
	self assert: (t paragraphStyleOrNilAt: 30) name = 'Heading 2' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 30) isNil description: 'Should not have a char style yet'.
	self assert: (t paragraphStyleOrNilAt: 60) name = 'Heading 3' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 60) isNil description: 'Should not have a char style yet'.
	self assert: (t paragraphStyleOrNilAt: 70) name = 'Normal' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 70) isNil description: 'Should not have a char style yet'.
	self assert: (t paragraphStyleOrNilAt: 120) name = 'Normal' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 120) name = 'Green 14' description: 'Wrong charStyle'.
	self assert: (t paragraphStyleOrNilAt: 160) name = 'Normal' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 160) name = 'Red 10 bold' description: 'Wrong charStyle'.
	self assert: (t paragraphStyleOrNilAt: 190) name = 'Normal' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 190) name = 'Green 11 Italic' description: 'Wrong charStyle'.
	self assert: (t paragraphStyleOrNilAt: 210) name = 'Normal' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 210) name = 'Red 10 bold' description: 'Wrong charStyle'.
	self assert: (t paragraphStyleOrNilAt: 220) name = 'Normal' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 220) isNil description: 'Should not have a char style any longer'.
	self assert: (t paragraphStyleOrNilAt: 400) name = 'Emphasized' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 400) isNil description: 'Should not have a char style'.
	self assert: (t paragraphStyleOrNilAt: 539) name = 'Heading 2' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 539) isNil description: 'Should not have a char style yet'.
	self assert: (t paragraphStyleOrNilAt: 543) name = 'Heading 2' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 543) name = 'Red 10 bold' description: 'Wrong charStyle'.
	self assert: (t paragraphStyleOrNilAt: 546) name = 'Heading 2' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 546) isNil description: 'Should not have a char style'.
	self assert: (t paragraphStyleOrNilAt: 554) name = 'Heading 2' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: 554) name = 'Green 14' description: 'Wrong charStyle'.
	self assert: (t paragraphStyleOrNilAt: t size) name = 'Normal' description: 'Wrong paragraphStyle'.
	self assert: (t characterStyleOrNilAt: t size) isNil description: 'Should not have a char style any longer'.! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 09:53'!
testStylesInPaste1
	"
	self new testStylesInPaste1
	"
	| heading1 heading1Text simpleText editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading', String newLineString attribute: (ParagraphStyleReference for: heading1).	"Include a cr, so our paragraphStyle is applied to resulting paragraph"
	simpleText _ 'This is a text without style' asText.

	initialText _ simpleText copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: heading1Text rtfString dataFormat: 'public.rtf'.
	editor paste.		"Extends heading 1 to beginning, runs must be compacted"
	concatenation _ editor text.
	self assert: concatenation runs runs size = 1 description: 'Should apply the "Heading 1" paragraph style to the whole text'.
	self assert: concatenation runs first size = 1 description: 'Should apply just the "Heading 1" paragraph style to the whole text'.
	self assert: concatenation runs first first style == heading1 description: 'Should apply the "Heading 1" paragraph style to the whole text'! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 09:52'!
testStylesInPaste2
	"
	self new testStylesInPaste2
	"
	| heading1 heading1Text simpleText editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading', String newLineString attribute: (ParagraphStyleReference for: heading1).	"Include a cr, so our paragraphStyle is applied to resulting paragraph"
	simpleText _ 'This is a text without style' asText.

	initialText _ simpleText copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	Clipboard storeObject: String newLineString. editor paste.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: heading1Text rtfString dataFormat: 'public.rtf'.
	editor paste.		"Extends heading 1 to beginning, runs must be compacted"
	concatenation _ editor text.
	self assert: concatenation runs runs size = 2 description: 'Should have 2 parts with different attributes'.
"	self assert: concatenation runs runs first = (simpleText size + 1) description: 'Should apply an empty paragraph style to the first part'."
	self assert: concatenation runs runs second = heading1Text size description: 'Should apply the "Heading 1" paragraph style to the second part'.
"	self assert: concatenation runs values first size = 1 description: 'Should apply an empty paragraph style to the first part'."
"	self assert: (concatenation runs values first first style isEquivalentTo: ParagraphStyle new) description: 'Should apply an empty paragraph style to the first part'."
	self assert: concatenation runs values second size = 1 description: 'Should apply the "Heading 1" paragraph style to the second part'.
	self assert: concatenation runs values second first style == heading1 description: 'Should apply the "Heading 1" paragraph style to the second part'! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 09:56'!
testStylesInPaste3
	"
	self new testStylesInPaste3
	"
	| heading1Text simpleText editor paragraph concatenation initialText model heading1 |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading' attribute: (ParagraphStyleReference for: heading1).
	simpleText _ 'This is a text without style' asText.

	initialText _ heading1Text copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: simpleText rtfString dataFormat: 'public.rtf'.
	editor paste.		"Extends style to beginning. I.e., lose the style, style comes from paragraph end"
	concatenation _ editor text.
	self assert: concatenation runs runs size = 1 description: 'Should apply no character style to the whole text'.
	self assert: concatenation runs first first style == heading1 description: 'Should apply just the "Heading 1" paragraph style to the whole text'! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 09:52'!
testStylesInPaste4
	"
	self new testStylesInPaste4
	"
	| heading1Text heading1 simpleText editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading' attribute: (ParagraphStyleReference for: heading1).
	simpleText _ 'This is a text without style' asText.

	initialText _ heading1Text copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	Clipboard storeObject: String newLineString. editor paste.
	"Now we give an text without styles to the Clipboard, and we test that it does include some paragrpah style when it comes back"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: simpleText rtfString dataFormat: 'public.rtf'.
	editor paste.
	concatenation _ editor text.
	self assert: concatenation runs runs size = 2 description: 'Should use imported RTF style'.
	self assert: concatenation runs runs first = (heading1Text size +1) description: 'Should apply the "Heading 1" paragraph style to the first part'.
	self assert: concatenation runs runs second = simpleText size description: 'Should apply the "Imported " paragraph style to the second part'.
	self assert: concatenation runs values first first style name = 'Heading 1'.
	self assert: concatenation runs values second first style name = 'Imported '! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 2/14/2013 12:19'!
testStylesInPaste5
	"
	self new testStylesInPaste5
	"
	| heading1Text heading1 simpleText editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading' attribute: (ParagraphStyleReference for: heading1).
	simpleText _ 'This is a text without style' asText.

	initialText _ heading1Text copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	Clipboard storeObject: String newLineString. editor paste.
	"Now we give an text without styles to the Clipboard, and we test that it does include some paragraph style when it comes back"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: simpleText asString asUtf8 dataFormat: 'public.utf8-plain-text'.
	editor paste.
	concatenation _ editor text.
	self assert: concatenation runs runs size = 1 description: 'Should apply same attributes to the whole text'.
	self assert: concatenation runs runs first = concatenation size description: 'Should apply the "Heading 1" paragraph style to all the text'.! !

!StyledTextTest methodsFor: 'tests' stamp: 'jmv 3/14/2012 09:51'!
testStylesInPaste6
	"
	self new testStylesInPaste6
	"
	| heading1 heading1Text simpleText editor paragraph concatenation initialText model |
	model _ StyledTextModel new.
	heading1 _ model styleSet paragraphStyleNamed: 'Heading 1'.
	heading1Text _ Text string: 'This is the heading' attribute: (ParagraphStyleReference for: heading1).
	simpleText _ ('This is a text without style', String newLineString) asText.

	initialText _ heading1Text copy.
	model contents: initialText.
	editor _ StyledTextEditor new model: model.
	paragraph _ Paragraph new.
	paragraph setModel: model; extentForComposing: 300@300.
	paragraph composeAll.
	editor morph: TextModelMorph new textMorph.
	editor paragraph: paragraph; resetState.

	editor deselectAndPlaceCursorAt: initialText size+1.
	Clipboard storeObject: String newLineString. editor paste.
	"Now we give an text without styles to the Clipboard, and we test that it does include some paragrpah style when it comes back"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: simpleText rtfString dataFormat: 'public.rtf'.
	editor paste.
	concatenation _ editor text.
	self assert: concatenation runs runs size = 2 description: 'Should have 2 parts with different attributes'.
	self assert: concatenation runs runs first = (heading1Text size + 1) description: 'Should apply the "Heading 1" paragraph style to the first part'.
	self assert: concatenation runs runs second = simpleText size description: 'Should apply no paragraph style to the second part'.
	self assert: concatenation runs values first size = 1 description: 'Should apply the "Heading 1" paragraph style to the first part'.
	self assert: concatenation runs values first first style == heading1 description: 'Should apply the "Heading 1" paragraph style to the first part'.
	self deny: concatenation runs values second first style == heading1 description: 'Should not apply the "Heading 1" paragraph style to the second part'! !

!CharacterStyleReference methodsFor: 'comparing' stamp: 'jmv 9/21/2011 11:14'!
= other 
	^ (other class == self class) 
		and: [other style isEquivalentTo: characterStyle]! !

!CharacterStyleReference methodsFor: 'scanning' stamp: 'jmv 1/25/2011 14:26'!
dominates: other
	"See what happens with ParagraphStyleReference, CharacterStyleReference, TextFontReference, TextEmphasis and TextAlignment
	So far, no one dominates the other. This means they are all applied. We don't specify an order though. This might need refinement"
	^ other class == self class! !

!CharacterStyleReference methodsFor: 'iterating' stamp: 'jmv 9/1/2009 15:36'!
forCharacterStyleReferenceDo: aBlock
	aBlock value: characterStyle! !

!CharacterStyleReference methodsFor: 'comparing' stamp: 'jmv 4/11/2011 21:33'!
hash
	"Rather cheap. Can be improved."
	^ characterStyle shortDescription hash! !

!CharacterStyleReference methodsFor: 'testing' stamp: 'jmv 4/11/2011 19:35'!
isStyle
	^true! !

!CharacterStyleReference methodsFor: 'printing' stamp: 'jmv 4/6/2011 18:12'!
printOn: strm
	super printOn: strm.
	strm nextPutAll: ': '.
	characterStyle shortPrintOn: strm! !

!CharacterStyleReference methodsFor: 'accessing' stamp: 'jmv 9/21/2011 11:08'!
style
	^characterStyle! !

!CharacterStyleReference methodsFor: 'accessing' stamp: 'jmv 9/21/2011 11:08'!
style: aCharacterStyle
	characterStyle _ aCharacterStyle! !

!CharacterStyleReference methodsFor: 'rtf exporting' stamp: 'jmv 4/12/2011 09:59'!
writeRTFStartOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute start. Return number of characters to skip (usually 0)"
	
	"familyName and pointSize"
	| kern quarterPoints twips |
	characterStyle pointSize ifNotNil: [ :ps |
		aStream nextPutAll: '\fs'; nextPutAll: ((ps * Text pointSizeConversionFactor ) rounded * 2) asString; space ].

	"emphasis"
	(characterStyle emphasis allMask: AbstractFont boldCode) ifTrue: [
		aStream nextPutAll: '\b ' ].
	(characterStyle emphasis allMask: AbstractFont italicCode) ifTrue: [
		aStream nextPutAll: '\i ' ].
	(characterStyle emphasis allMask: AbstractFont underlinedCode) ifTrue: [
		aStream nextPutAll: '\ul ' ].
	(characterStyle emphasis allMask: AbstractFont struckThroughCode) ifTrue: [
		aStream nextPutAll: '\strike \strikec0 ' ].
	(characterStyle emphasis allMask: AbstractFont outlineCode) ifTrue: [
		aStream nextPutAll: '\outl\strokewidth60 ' ].

	"color"
	characterStyle color ifNotNil: [ :c |
		aStream nextPutAll: '\cf'; nextPutAll: (colorArray indexOf: c) asString; space ].
	
	"kern"
	kern _ characterStyle kern.
	kern = 0 ifFalse: [
		quarterPoints _ kern * 4.	"This maybe needs tweaking!!"
		twips _ kern * 12.
		aStream nextPutAll:
			'\kerning1\expnd', quarterPoints asString,
			'\expndtw', twips asString; space ].

	^0! !

!CharacterStyleReference methodsFor: 'rtf exporting' stamp: 'jmv 4/12/2011 09:59'!
writeRTFStopOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute stop.
	Do it in the inverse order of writeRTFStartOn:colorTable:fontTable:"

	"kern"
	aStream nextPutAll: '\kerning1\expnd0\expndtw0 ' .

	"color"
	aStream nextPutAll: '\cf0 '.

	"emphasis"
	(characterStyle emphasis allMask: AbstractFont outlineCode) ifTrue: [
		aStream nextPutAll: '\outl0\strokewidth0 ' ].
	(characterStyle emphasis allMask: AbstractFont struckThroughCode) ifTrue: [
		aStream nextPutAll: '\strike0\striked0 ' ].
	(characterStyle emphasis allMask: AbstractFont underlinedCode) ifTrue: [
		aStream nextPutAll: '\ulnone ' ].
	(characterStyle emphasis allMask: AbstractFont italicCode) ifTrue: [
		aStream nextPutAll: '\i0 ' ].
	(characterStyle emphasis allMask: AbstractFont boldCode) ifTrue: [
		aStream nextPutAll: '\b0 ' ].

	"familyName and pointSize"
	characterStyle pointSize ifNotNil: [ :ps |
		aStream nextPutAll: '\fs0 ' ]! !

!CharacterStyleReference class methodsFor: 'instance creation' stamp: 'jmv 9/21/2011 11:14'!
for: aCharacterStyle
	| attribute |
self assert: aCharacterStyle notNil.
	attribute _ self new.
	attribute style: aCharacterStyle.
	^attribute! !

!ParagraphStyleReference methodsFor: 'comparing' stamp: 'jmv 9/21/2011 11:12'!
= other 
	^ (other class == self class) 
		and: [other style isEquivalentTo: paragraphStyle]! !

!ParagraphStyleReference methodsFor: 'scanning' stamp: 'jmv 11/29/2011 13:50'!
dominates: other
	"See what happens with ParagraphStyleReference, CharacterStyleReference, TextFontReference, TextEmphasis and TextAlignment
	So far, no one dominates the other. This means they are all applied. We don't specify an order though. This might need refinement"

	"Leave TextAnchors and such alone!!"
	other isForFormatting ifFalse: [ ^false ].

	^ other class == self class
		or: [
			"Any ParagraphStyle precludes the use of regular CharacterAttributes.
			This restriction is not usually needed, as the StyledTextEditor doesn't allow the use of regular attributes.
			But it was needed to add this, because the 'Smalltalk Code' ParagraphStyle uses shout to decorate text, and this adds regular Attributes
			such as color, bold, italic... Later, if we remove that ParagraphStyle (to use a regular one), we need to ensure that all those attributes
			are removed"
			other isStyle not
				or: [
					"A ParahraphStyle that does Shout (auto code formatting) precludes the use of CharacterStyles and other CharacterAttributes"
					paragraphStyle doesShout and: [ other isParagraphAttribute not ] ] ]! !

!ParagraphStyleReference methodsFor: 'iterating' stamp: 'jmv 1/25/2011 13:42'!
forParagraphStyleReferenceDo: aBlock
	aBlock value: paragraphStyle! !

!ParagraphStyleReference methodsFor: 'comparing' stamp: 'jmv 4/11/2011 21:33'!
hash
	"Rather cheap. Can be improved."
	^ paragraphStyle shortDescription hash! !

!ParagraphStyleReference methodsFor: 'testing' stamp: 'jmv 9/1/2009 23:02'!
isParagraphAttribute
	"Attributes that answer true will always be applied to whole paragraphs, i.e. starting at the position after a cr (or 1) and ending at a cr (or text size)"
	
	^true! !

!ParagraphStyleReference methodsFor: 'testing' stamp: 'jmv 4/11/2011 19:35'!
isStyle
	^true! !

!ParagraphStyleReference methodsFor: 'printing' stamp: 'jmv 4/7/2011 17:06'!
printOn: strm
	super printOn: strm.
	strm nextPutAll: ': ('.
	paragraphStyle name printOn: strm.
	strm nextPutAll: ') '.
	paragraphStyle shortPrintOn: strm! !

!ParagraphStyleReference methodsFor: 'accessing' stamp: 'jmv 9/21/2011 11:08'!
style
	^paragraphStyle! !

!ParagraphStyleReference methodsFor: 'accessing' stamp: 'jmv 9/21/2011 11:08'!
style: aParagraphStyle
	paragraphStyle _ aParagraphStyle! !

!ParagraphStyleReference methodsFor: 'rtf exporting' stamp: 'jmv 4/12/2011 09:59'!
writeRTFStartOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute start. Return number of characters to skip (usually 0)"
	"paragraph attributes still missing..."

	| kern quarterPoints twips k |

	"familyName and pointSize"
	paragraphStyle pointSize ifNotNil: [ :ps |
		aStream nextPutAll: '\fs'; nextPutAll: ((ps * Text pointSizeConversionFactor ) rounded * 2) asString; space ].

	"emphasis"
	(paragraphStyle emphasis allMask: AbstractFont boldCode) ifTrue: [
		aStream nextPutAll: '\b ' ].
	(paragraphStyle emphasis allMask: AbstractFont italicCode) ifTrue: [
		aStream nextPutAll: '\i ' ].
	(paragraphStyle emphasis allMask: AbstractFont underlinedCode) ifTrue: [
		aStream nextPutAll: '\ul ' ].
	(paragraphStyle emphasis allMask: AbstractFont struckThroughCode) ifTrue: [
		aStream nextPutAll: '\strike \strikec0 ' ].
	(paragraphStyle emphasis allMask: AbstractFont outlineCode) ifTrue: [
		aStream nextPutAll: '\outl\strokewidth60 ' ].

	"color"
	paragraphStyle color ifNotNil: [ :c |
		aStream nextPutAll: '\cf'; nextPutAll: (colorArray indexOf: c) asString; space ].
	
	"kern"
	kern _ paragraphStyle kern.
	kern = 0 ifFalse: [
		quarterPoints _ kern * 4.	"This maybe needs tweaking!!"
		twips _ kern * 12.
		aStream nextPutAll:
			'\kerning1\expnd', quarterPoints asString,
			'\expndtw', twips asString; space ].

	"alignment"
	paragraphStyle alignment caseOf: {
		[ 0 ] -> [ aStream nextPutAll: '\ql ' ].
		[ 1 ] -> [ aStream nextPutAll: '\qr ' ].
		[ 2 ] -> [ aStream nextPutAll: '\qc ' ].
		[ 3 ] -> [ aStream nextPutAll: '\qj ' ] }.

	"Exportar. Cerrar tambien, y despues, importar!!
	tabsArray listBulletPattern
	"

	"indent"
	k _ 20.
	twips _ (paragraphStyle firstIndent-paragraphStyle restIndent) * k.
	twips = 0 ifFalse: [
		aStream nextPutAll: '\fi', twips asString; space ].
	twips _ paragraphStyle restIndent * k.
	twips = 0 ifFalse: [
		aStream nextPutAll: '\li', twips asString; space ].
	"Have no idea why, but it seems that on paste, an extra inch is added!!"
	twips _ (paragraphStyle rightIndent-72) * k.
	twips = 0 ifFalse: [
		aStream nextPutAll: '\ri', twips asString; space ].

	k _ 20.
	"paragraph spacing"
	twips _ paragraphStyle spaceBefore * k.
	twips = 0 ifFalse: [
		aStream nextPutAll: '\sb', twips asString; space ].
	twips _ paragraphStyle spaceAfter * k.
	twips = 0 ifFalse: [
		aStream nextPutAll: '\sa', twips asString; space ].

	^0! !

!ParagraphStyleReference methodsFor: 'rtf exporting' stamp: 'jmv 4/12/2011 09:59'!
writeRTFStopOn: aStream colorTable: colorArray fontTable: fontArray
	"Write the RTF code for attribute stop.
	Do it in the inverse order of writeRTFStartOn:colorTable:fontTable:"

	"alignment"
	aStream nextPutAll: '\ql '.

	"kern"
	aStream nextPutAll: '\kerning1\expnd0\expndtw0 ' .

	"color"
	aStream nextPutAll: '\cf0 '.

	"emphasis"
	(paragraphStyle emphasis allMask: AbstractFont outlineCode) ifTrue: [
		aStream nextPutAll: '\outl0\strokewidth0 ' ].
	(paragraphStyle emphasis allMask: AbstractFont struckThroughCode) ifTrue: [
		aStream nextPutAll: '\strike0\striked0 ' ].
	(paragraphStyle emphasis allMask: AbstractFont underlinedCode) ifTrue: [
		aStream nextPutAll: '\ulnone ' ].
	(paragraphStyle emphasis allMask: AbstractFont italicCode) ifTrue: [
		aStream nextPutAll: '\i0 ' ].
	(paragraphStyle emphasis allMask: AbstractFont boldCode) ifTrue: [
		aStream nextPutAll: '\b0 ' ].

	"familyName and pointSize"
	paragraphStyle pointSize ifNotNil: [ :ps |
		aStream nextPutAll: '\fs0 ' ].

	"paragraph defaults (i.e. paragraph end). Needed to honor spaceBefore and spaceAfter"
	aStream nextPutAll: '\pard '! !

!ParagraphStyleReference class methodsFor: 'instance creation' stamp: 'jmv 9/21/2011 11:09'!
for: aParagraphStyle
	| attribute |
self assert: aParagraphStyle notNil.
	attribute _ self new.
	attribute style: aParagraphStyle.
	^attribute! !

!STETheme methodsFor: 'colors' stamp: 'bp 4/22/2012 10:51'!
textHighlight
	"
	^ Color r: 0.71 g: 0.835 b: 1.0
	^ Color hue: 214 chroma: 0.29 luminance: 0.816
	"
	^Color r: 204 g: 235 b: 214 range: 255! !

!STETheme methodsFor: 'colors' stamp: 'jmv 12/30/2011 11:42'!
unfocusedTextHighlightFrom: aColor
	^Color r: 218 g: 228 b: 228 range: 255! !
STECompleter initialize!
StyledTextEditor initialize!
PluggableStyledTextMorph initialize!
