'From Cuis 4.1 of 12 December 2012 [latest update: #1759] on 17 July 2013 at 12:57:17.457537 am'!
'Description Please enter a description for this package.'!
!provides: 'StyledTextWiki' 1 0!
!requires: 'StyledText' 1 nil!
!classDefinition: #StyledTextWiki category: #StyledTextWiki!
Object subclass: #StyledTextWiki
	instanceVariableNames: 'textModel pages currentPage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledTextWiki'!
!classDefinition: 'StyledTextWiki class' category: #StyledTextWiki!
StyledTextWiki class
	instanceVariableNames: ''!

!classDefinition: #StyledTextWikiEditor category: #StyledTextWiki!
Object subclass: #StyledTextWikiEditor
	instanceVariableNames: 'wiki textModel textMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledTextWiki'!
!classDefinition: 'StyledTextWikiEditor class' category: #StyledTextWiki!
StyledTextWikiEditor class
	instanceVariableNames: ''!

!classDefinition: #StyledTextWikiPage category: #StyledTextWiki!
Object subclass: #StyledTextWikiPage
	instanceVariableNames: 'wiki text'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StyledTextWiki'!
!classDefinition: 'StyledTextWikiPage class' category: #StyledTextWiki!
StyledTextWikiPage class
	instanceVariableNames: ''!


!StyledTextWikiEditor commentStamp: 'bp 12/18/2011 18:09' prior: 0!
(StyledTextWikiEditor wiki: StyledTextWiki new) morphicWindow openInWorld!

!StyledTextWiki methodsFor: 'as yet unclassified' stamp: 'bp 10/24/2010 16:58'!
currentPage
	^currentPage! !

!StyledTextWiki methodsFor: 'as yet unclassified' stamp: 'bp 10/24/2010 17:00'!
currentPageIndex
	^pages indexOf: currentPage! !

!StyledTextWiki methodsFor: 'as yet unclassified' stamp: 'jmv 8/10/2011 10:55'!
currentPageIndex: anInteger
	currentPage := pages at: anInteger.
	textModel actualContents: currentPage text! !

!StyledTextWiki methodsFor: 'as yet unclassified' stamp: 'jmv 8/10/2011 10:56'!
initialize
	| heading1 |
	textModel _ StyledTextModel new.
	heading1 _ textModel styleSet paragraphStyleNamed: 'Heading 1'.
	pages := OrderedCollection new.
	currentPage := self newPage: (
		Text
			string: 'Welcome to the Styled Text Wiki!!'
			attribute: (ParagraphStyleReference for: heading1)).
	textModel actualContents: currentPage text! !

!StyledTextWiki methodsFor: 'as yet unclassified' stamp: 'jmv 1/13/2012 14:03'!
newPage
	| normal |
	normal _ textModel styleSet defaultStyle.
	currentPage := self newPage:
		(Text string: '' attribute: (ParagraphStyleReference for: normal)).
	textModel actualContents: currentPage text! !

!StyledTextWiki methodsFor: 'as yet unclassified' stamp: 'bp 10/24/2010 16:47'!
newPage: aText
	^pages add: ((StyledTextWikiPage wiki: self)
		text: aText;
		yourself)! !

!StyledTextWiki methodsFor: 'as yet unclassified' stamp: 'bp 10/24/2010 16:39'!
pageNames
	^pages collect: [:each | each name]! !

!StyledTextWiki methodsFor: 'as yet unclassified' stamp: 'jmv 8/10/2011 10:53'!
textModel
	^textModel! !

!StyledTextWikiEditor methodsFor: 'as yet unclassified' stamp: 'bp 12/21/2011 10:04'!
createMorph
	| appMorph pagesList newPageButton toolbar |
	textModel := StyledTextModel withText: wiki currentPage text.
	appMorph := PluggableStyledTextMorph withModel: textModel in: STEMainMorph newColumn.
	textMorph := appMorph submorphs first.
	pagesList := PluggableFilteringDropDownListMorph
		model: self
		listGetter: #pageNames
		indexGetter: #currentPageIndex
		indexSetter: #currentPageIndex:.
	pagesList
		borderWidth: 0;
		layoutSpec: (LayoutSpec morphHeightProportionalWidth: 0.5).
	newPageButton := PluggableButtonMorph model: self action: #newPage.
	newPageButton
		extent: 22@22;
		icon: Theme current newIcon;
		layoutSpec: LayoutSpec keepMorphExtent.
	toolbar := appMorph submorphs last.
	toolbar addMorphBack: newPageButton.
	toolbar addMorphBack: pagesList.	"add it at the left of the layout"
	textMorph when: #possiblyChanged send: #modelChanged to: pagesList.
	^appMorph! !

!StyledTextWikiEditor methodsFor: 'as yet unclassified' stamp: 'bp 10/24/2010 17:00'!
currentPageIndex
	^wiki currentPageIndex! !

!StyledTextWikiEditor methodsFor: 'as yet unclassified' stamp: 'bp 7/8/2011 22:49'!
currentPageIndex: anInteger
	self updateCurrentPage.
	wiki currentPageIndex: anInteger.
	textModel actualContents: wiki currentPage text! !

!StyledTextWikiEditor methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2011 11:00'!
morphicWindow
	| window |
	window _ SystemWindow new model: self.
	window setLabel: 'Styled Text Wiki'.
	window layoutMorph
		addMorph: (self createMorph)
		proportionalHeight: 1.
	^ window! !

!StyledTextWikiEditor methodsFor: 'as yet unclassified' stamp: 'bp 7/8/2011 23:11'!
newPage
	| selection pageName |
	selection := textMorph selectionInterval.
	pageName := textModel actualContents copyFrom: selection first to: selection last.
	textMorph currentCharacterStyleIndex: 1.
	wiki newPage.
	textModel actualContents: pageName.
	self updateCurrentPage.
	textMorph
		selectAll;
		currentParagraphStyleIndex: 3! !

!StyledTextWikiEditor methodsFor: 'as yet unclassified' stamp: 'bp 7/8/2011 22:49'!
pageNames
	^wiki pageNames! !

!StyledTextWikiEditor methodsFor: 'as yet unclassified' stamp: 'bp 10/24/2010 16:58'!
setWiki: aStyledTextWiki
	wiki := aStyledTextWiki! !

!StyledTextWikiEditor methodsFor: 'as yet unclassified' stamp: 'jmv 1/6/2012 12:03'!
updateCurrentPage
	textMorph textMorph acceptContents.
	wiki currentPage text: textModel actualContents! !

!StyledTextWikiEditor class methodsFor: 'as yet unclassified' stamp: 'bp 10/24/2010 16:56'!
wiki: aStyledTextWiki
	^self new setWiki: aStyledTextWiki! !

!StyledTextWikiPage methodsFor: 'as yet unclassified' stamp: 'jmv 3/14/2012 08:27'!
name
	| string |
	string _ self text asString lines first.
	string size > 30 ifTrue: [
		string _ string copyFrom: 1 to: 30 ].
	string ifEmpty: [ ^ '(Empty page)' ].
	^ string! !

!StyledTextWikiPage methodsFor: 'as yet unclassified' stamp: 'jmv 1/13/2012 14:03'!
setWiki: aStyledTextWiki
	| normal |
	wiki := aStyledTextWiki.
	normal _ wiki textModel styleSet defaultStyle.
	text := Text string: '' attribute: (ParagraphStyleReference for: normal)! !

!StyledTextWikiPage methodsFor: 'as yet unclassified' stamp: 'bp 10/24/2010 16:17'!
text
	^text! !

!StyledTextWikiPage methodsFor: 'as yet unclassified' stamp: 'bp 10/24/2010 16:45'!
text: aText
	text := aText! !

!StyledTextWikiPage class methodsFor: 'as yet unclassified' stamp: 'bp 10/24/2010 16:56'!
wiki: aStyledTextWiki
	^self new setWiki: aStyledTextWiki! !
