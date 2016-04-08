'From Cuis 4.2 of 25 July 2013 [latest update: #2723] on 8 April 2016 at 1:48:55.826614 pm'!

!InnerTextMorph commentStamp: 'jmv 4/8/2016 13:46' prior: 0!
InnerTextMorphs support display of text with emphasis.  They also support reasonable text-editing capabilities, as well as embedded hot links, and the ability to embed submorphs in the text. They are 'bare' in the sense that they can not clip contents to some window, or scroll it by themselves.

Text display is clipped to the innerBounds of the rectangle, and text composition is normally performed within a rectangle which is innerBounds inset by the margins parameter.

Comment about Shout specifics:
-----------------------------------------
Instances of me are usually created using my #on:text:accept:readSelection:menu: class method.

In order to colour the text, I use an instance of SHTextStylerST80, which I store in my 'styler' instance variable.

When my setText: method is called, I use my styler to ...
	a) optionally set all assignments to ansi or leftArrow. 
	b) Colour my text (immediately, if the text is less than 4096 chars in length, or in a backgroundProcess otherwise)

	When my text is changed, my hasUnacceptedEdits: method is called with true, and I ask my styler to re-colour my text. This is performed in a background process so that typing remains responsive regardless of the length of the text.

	Just before my styler is about to format/style the text, I send #stylerAboutToStyle:  to my model. This gives my model a chance to veto the styling (by answering false), or to initialize the styler with information it needs in order to parse the text correctly (e.g. the class to which a method belongs, or the workspace in which I am contained).

	My styler informs me that it has finished styling by triggering the #shoutStyled event which I handle. I then update the textAttributes of my text and refresh the display.
 
	My 'unstyledAcceptText' instance variable is used in conjunction with my #acceptTextInModel and #correctFrom:to:with: methods to ensure that when my text is modified during a method compilation (removing unused vars etc), I do not lose those changes.!


!SHTextStyler commentStamp: 'jmv 4/8/2016 13:47' prior: 0!
I am an Abstract class.
Subclasses of me can create formatted, coloured, and styled copies of Text that is given to them.
They may perform their styling asynchronously, in a background process which I create and manage.

My public interface is...

	view: aViewOrMorph - set the view that will receive notifications when styling has completed.
	
	format: aText - modifies aText's string

	style: aText - modifies the TextAttributes of aText, but does not change the string, then triggers #shoutStyled.

	styleInBackgroundProcess: aText - performs style: in a background process, then sends #stylerStylednBackground: to the view.

	styledTextFor: aText - answers a formatted and styled copy of aText

	unstyledTextFrom: aText - answers a copy of aText with all TextAttributes removed

Subclasses of me should re-implement...

	privateFormat: aText - answer a formatted version of aText; the String may be changed
	privateStyle: aText - modify the TextAttributes of aText; but do not change the String
	

	
	
!

!classDefinition: #ShoutTextColor category: #'System-TextAttributes'!
TextColor subclass: #ShoutTextColor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-TextAttributes'!

!ShoutTextColor commentStamp: '<historical>' prior: 0!
Just for code styler (Shout)!

!classDefinition: #ShoutTextEmphasis category: #'System-TextAttributes'!
TextEmphasis subclass: #ShoutTextEmphasis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-TextAttributes'!

!ShoutTextEmphasis commentStamp: '<historical>' prior: 0!
Just for code styler (Shout)!


!Text methodsFor: 'emphasis' stamp: 'jmv 4/8/2016 13:28'!
removeAttributesThat: aBlock
	runs mapValues: [ :attributes |
		attributes reject: aBlock ]! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 4/8/2016 13:30'!
setAttributesIn: aText fromRasnges: ranges in: anInterval

	aText removeAttributesThat: [ :attribute | attribute isForShout ].
	ranges do: [ :range |
		(self attributesFor: range rangeType) ifNotNil: [ :attributes |
			attributes do: [ :attr |
				aText addAttribute: attr from: range start to: range end
			]]]! !


!TextAttribute methodsFor: 'testing' stamp: 'jmv 4/8/2016 13:07'!
isForShout
	"True if to be removed from code before styling"
	^false! !


!ShoutTextColor methodsFor: 'testing' stamp: 'jmv 4/8/2016 13:07'!
isForShout
	"True if to be removed from code before styling"
	^true! !


!ShoutTextEmphasis methodsFor: 'testing' stamp: 'jmv 4/8/2016 13:07'!
isForShout
	"True if to be removed from code before styling"
	^true! !


!TextModel methodsFor: 'shout support' stamp: 'jmv 4/8/2016 12:58'!
privateAddBoldAt: idx
	"Just for shout. No undo."

	actualContents 
		addAttribute: ShoutTextEmphasis bold
		from: idx
		to: idx! !

!TextModel methodsFor: 'shout support' stamp: 'jmv 4/8/2016 12:58'!
privateRemoveBoldAt: idx
	"Just for shout. No undo."

	actualContents 
		removeAttributes: { ShoutTextEmphasis bold }
		from: idx
		to: idx! !


!Browser methodsFor: 'class comment pane' stamp: 'jmv 4/8/2016 13:01'!
newClassComment: aText
	"The user has just entered aText.
	It may be all red (a side-effect of replacing the default comment), so remove the color if it is."
	| theClass |
	theClass _ self selectedClassOrMetaClass.
	theClass ifNotNil: [
		theClass classComment: aText asString ].
	self changed: #classCommentText.
	^ true! !


!TextModelMorph methodsFor: 'updating' stamp: 'jmv 4/8/2016 13:48'!
update: aSymbol
	super update: aSymbol.
	aSymbol ifNil: [^self].
	aSymbol == #flash ifTrue: [^self flash].
	aSymbol == #actualContents ifTrue: [
		"Some day, it would be nice to keep objects and update them
		instead of throwing them away all the time for no good reason..."
		self textMorph
			releaseEditorAndTextComposition;
			installEditorAndTextComposition;
			formatAndStyleIfNeeded.
		self setScrollDeltas.
		self redrawNeeded.
		^self ].
	aSymbol == #acceptedContents ifTrue: [
		self textMorph hasUnacceptedEdits ifTrue: [
			self textMorph hasEditingConflicts: true.
			^self redrawNeeded ].
		model refetch.
		"#actualContents also signalled in #refetch. No need to repeat what's done there."
		^self ].
	aSymbol == #refetched ifTrue: [
		"#actualContents also signalled when #refetched is signalled.
		No need to repeat what's done there."
		self setSelection: model getSelection.
		self hasUnacceptedEdits: false.
		^self ].
	aSymbol == #initialSelection ifTrue: [
		^self setSelection: model getSelection; redrawNeeded ].
	aSymbol == #autoSelect ifTrue: [
		TextEditor abandonChangeText.	"no replacement!!"
		self editor
			setSearch: model autoSelectString;
			findAndReplaceMany: true.
		self textMorph updateFromTextComposition.
		^self scrollSelectionIntoView ].
	"Quite ugly"
	aSymbol == #clearUserEdits ifTrue: [
		^self hasUnacceptedEdits: false].
	aSymbol == #shoutStyled ifTrue: [
		self textMorph stylerStyled.
		^self redrawNeeded ]! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 4/8/2016 13:48'!
formatAndStyle
	"Do the styling on a copy of the model text.
	After finishing, send it to the model, by triggering #shoutStyled
	The model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.
	formattedText := textModel actualContents.
	self privateFormatAndConvert.
	textModel basicActualContents: formattedText.
	formattedText size < 4096
		ifTrue: [
			formattedText := textModel actualContents "copy". "ok not to copy?"
			self privateStyle.
			textModel changed: #shoutStyled ]
		ifFalse: [ self styleInBackgroundProcess ].! !

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 4/8/2016 13:48'!
styleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, by triggering #shoutStyled
	The the model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.

	formattedText _ textModel actualContents "copy". "ok not to copy?"
	self mutex critical: [
		"This part runs at low priority, and signals sem when finished"
		backgroundProcess _  [
			self privateStyle.
			WorldState addDeferredUIMessage: [
				textModel changed: #shoutStyled ].
			] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 4/8/2016 13:38'!
setAttributesIn: aText fromRanges: ranges in: anInterval

	aText removeAttributesThat: [ :attribute | attribute isForShout ].
	ranges do: [ :range |
		(self attributesFor: range rangeType) ifNotNil: [ :attributes |
			attributes do: [ :each |
				aText addAttribute: each from: range start to: range end ]]]! !


!SHTextStylerST80 class methodsFor: 'style table' stamp: 'jmv 4/8/2016 12:57'!
attributeArrayForColor: aColorOrNil emphasis: anEmphasisSymbolOrArrayorNil
	"Answer a new Array containing any non nil TextAttributes specified"
	| answer emphArray |

	answer _ #().
	aColorOrNil ifNotNil: [ answer _ answer, {ShoutTextColor color: aColorOrNil} ].
	anEmphasisSymbolOrArrayorNil ifNotNil: [
		emphArray _ anEmphasisSymbolOrArrayorNil isSymbol 
			ifTrue: [ {anEmphasisSymbolOrArrayorNil} ] 
			ifFalse: [ anEmphasisSymbolOrArrayorNil ].
		emphArray do: [ :each |
			each ~= #normal
				ifTrue: [
					answer _ answer, {ShoutTextEmphasis perform: each}]]].
	^answer! !

!methodRemoval: ShoutTextEmphasis #isOverridenByShout!
ShoutTextEmphasis removeSelector: #isOverridenByShout!
!methodRemoval: ShoutTextColor #isOverridenByShout!
ShoutTextColor removeSelector: #isOverridenByShout!
!methodRemoval: TextAnchor #isOverridenByShout!
TextAnchor removeSelector: #isOverridenByShout!
!methodRemoval: TextAttribute #isOverridenByShout!
TextAttribute removeSelector: #isOverridenByShout!
!methodRemoval: TextModel #stylerStyled:checkForChanges:!
TextModel removeSelector: #stylerStyled:checkForChanges:!

!Text reorganize!
('accessing' append: at: at:put: commandForReplaceFrom:to:with: customizeExplorerContents embeddedMorphs embeddedMorphsFrom:to: findString:startingAt: findString:startingAt:caseSensitive: lineCount prepend: rangeOf:startingAt: replaceFrom:to:with: size string)
('comparing' = hash is:)
('copying' copyFrom:to: copyReplaceFrom:to:with: copyReplaceTokens:with: postCopy)
('converting' asNumber asString asText displayStringOrText replaceFrom:to:with:startingAt: replaceFrom:to:withString:attributes:startingAt: reversed withCuisLineEndings)
('emphasis' addAttribute: addAttribute:from:to: alignmentAt: attributesAt: attributesFrom:to:do: characterStyleOrNilAt: characterStyleOrNilIfApplying: commandForAddAttribute:from:to: commandForRemoveAttributes:from:to: emphasisAt: find: font: fontAt: fontIfApplying: initialFont: paragraphStyleOrNilAt: paragraphStyleOrNilIfApplying: removeAttributes:from:to: removeAttributesThat: runLengthFor: withAttributeValues:do:)
('printing' printOn: storeOn:)
('private' basicReplaceAttributesFrom:to:with: fixParagraphAttributesFrom:to: privateSetParagraphAttributes:from:to: privateSetRuns: runs setString:setRuns:)
('attributes' canJoin:and: hasAnyAttribute hasAnyParagraphAttribute)
('user interface' edit editLabel:)
('paragraph support' encompassLine: encompassParagraph: paragraphStyleChunksDo:)
('adding emphasis' allBold bold italic sub super under)
!


!ShoutTextColor reorganize!
('testing' isForShout)
!


!ShoutTextEmphasis reorganize!
('testing' isForShout)
!

