'From Cuis 6.0 [latest update: #5730] on 28 March 2023 at 3:01:43 pm'!
!classDefinition: #SHTextStylerST80 category: #'Tools-Syntax Highlighting'!
SHTextStyler subclass: #SHTextStylerST80
	instanceVariableNames: 'classOrMetaClass workspace parser disableFormatAndConvert lengthIncreaseValues lengthIncreasePositions '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!InnerTextMorph methodsFor: 'selection' stamp: 'jmv 3/28/2023 15:01:25'!
highlightMessage

	| messageSendsRanges theText attr |
	messageSendsRanges := model textProvider messageSendsRangesOf: model autoSelectString.
	messageSendsRanges isEmpty ifTrue: [ ^false ].
	theText := model actualContents.
	attr := `ShoutTextBackgroundColor color: (Color red alpha: 0.2)`.
	messageSendsRanges do: [ :r | | f l |
		f := r first.
		l := r last.
		styler ifNotNil: [
			f := styler positionInFormattedTextFor: f.
			l := styler positionInFormattedTextFor: l ].
		 theText addAttribute: attr from: f to: l ].
	^true! !

!InnerTextMorph methodsFor: 'selection' stamp: 'jmv 3/28/2023 14:53:13'!
highlightString

	| found start cs theText s toHighlight ths attr |
	cs := Preferences at: #caseSensitiveFinds.
	theText := model actualContents.
	s := theText size.
	toHighlight := model autoSelectString.
	ths := toHighlight size.
	attr := `ShoutTextBackgroundColor color: (Color red alpha: 0.2)`.
	start := 1.
	[start < s] whileTrue: [
		found := theText findString: toHighlight startingAt: start caseSensitive: cs.
		start := s.
		found = 0 ifFalse: [
			theText addAttribute: attr from: found to: found+ths-1.
			start := found+ths ]]! !

!InnerTextMorph methodsFor: 'selection' stamp: 'jmv 3/28/2023 14:42:56'!
setSelection: sel

	sel == #all
		ifTrue: [ self editor selectAll ]
		ifFalse: [
			sel
				ifNil: [ self editor selectFrom:1 to: 0 ]
				ifNotNil: [ self editor selectFrom: sel first to: sel last ]].
	self scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'selection' stamp: 'jmv 3/28/2023 14:59:56'!
updateInitialSelection

	| s |
	s := model getSelection.
	s ifNotNil: [
		s = #all ifFalse: [
			styler ifNotNil: [
				s := (styler positionInFormattedTextFor: s first)
					to: (styler positionInFormattedTextFor: s last) ]]].
	self setSelection: s.
	self redrawNeeded.! !

!InnerTextMorph methodsFor: 'selection' stamp: 'jmv 3/28/2023 15:00:04'!
updateRefetched 
	"#actualContents also signalled when #refetched is signalled.
	No need to repeat what's done there."
	| s |
	s := model getSelection.
	s ifNotNil: [
		s = #all ifFalse: [
			styler ifNotNil: [
				s := (styler positionInFormattedTextFor: s first)
					to: (styler positionInFormattedTextFor: s last) ]]].
	self setSelection: s.
	self hasUnacceptedEdits: false.! !


!SHTextStylerST80 methodsFor: 'services' stamp: 'jmv 3/28/2023 14:22:42'!
positionInFormattedTextFor: positionInOriginalText
	| i |
	i := lengthIncreasePositions findLast: [ :p | p <= positionInOriginalText ].
	i = 0 ifTrue: [ ^positionInOriginalText ].
	^ positionInOriginalText + (lengthIncreaseValues at: i).! !


!MessageSet methodsFor: 'shout styling' stamp: 'jmv 3/28/2023 12:03:02'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [ ^false ].
	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.
	autoSelectString ifNotNil: [
		"To make highlight of autoSelectString correct."
		"anSHTextStyler disableFormatAndConvert" ].
	^true! !


!Debugger methodsFor: 'shout styling' stamp: 'jmv 3/28/2023 12:03:09'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	anSHTextStyler 
		classOrMetaClass: self selectedClassOrMetaClass;
		"disableFormatAndConvert;"						"To make higlights of currently executing code correct."
		workspace: self.
	^true! !


!TextModelMorph methodsFor: 'updating' stamp: 'jmv 3/28/2023 14:36:14'!
updateAutoSelect 

	TextEditor abandonChangeText.	"no replacement!!"
	scroller highlightMessage
		ifFalse: [ scroller highlightString ].! !

!TextModelMorph methodsFor: 'updating' stamp: 'jmv 3/28/2023 14:44:36'!
updateInitialSelection

	scroller updateInitialSelection! !

!TextModelMorph methodsFor: 'updating' stamp: 'jmv 3/28/2023 14:44:27'!
updateRefetched 

	scroller updateRefetched! !


!Sample04Pacman methodsFor: 'updating' stamp: 'jmv 3/28/2023 14:55:47'!
redrawNeeded
	"for experimenting with collision detection"

	self isOwnedByHand ifTrue: [
		self world ifNotNil: [ :w |
			w submorphsDo: [ :m |
				(self collides: m) ifTrue: [
					m class == self class
						ifTrue: [ m colorize ]
						ifFalse: [ m delete ]]]]].

	super redrawNeeded.! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 3/28/2023 14:27:51'!
privateFormatAndConvert

	"Note: The handling of these instance variables assumes that only one call to
	#replaceStringForRangesWithType:with: will modify the length of the text.
	If this is ever not the case, a more sophisticated implementation that can combine their effects is needed."
	lengthIncreasePositions isNil ifTrue: [
		lengthIncreasePositions := OrderedCollection new.
		lengthIncreaseValues := OrderedCollection new ]
	ifFalse: [
		lengthIncreasePositions removeAll.
		lengthIncreaseValues removeAll ].

	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow
			If disableFormatAnConvert, don't: changes in source code ranges are not allowed."
			disableFormatAndConvert ifFalse: [
				self replaceStringForRangesWithType: #ansiAssignment with: Character smalltalkLeftArrow asString ].
			"Turn $_ into a left arrow"
			self replaceStringForRangesWithType: #assignment with: Character smalltalkLeftArrow asString.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: Character smalltalkUpArrow asString ]
		ifFalse: [
			"Turn $_ into :=
			If disableFormatAndConvert, no changes in source code ranges are allowed, use single character left arrow."
			disableFormatAndConvert
				ifFalse: [
					self replaceStringForRangesWithType: #assignment with: ':='  ]
				ifTrue: [
					self replaceStringForRangesWithType: #assignment with: Character smalltalkLeftArrow asString  ]].! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 3/28/2023 14:23:56'!
replaceStringForRangesWithType: aSymbol with: aString
	"Answer aText if no replacements, or a copy of aText with 
	each range with a type of aSymbol replaced by aString"

	| toReplace increaseInLength |

	self parseSetWorkspace: false.
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
			increaseInLength := increaseInLength + thisIncrease.
			increaseInLength = 0 ifFalse: [
				lengthIncreasePositions add: each end+1.
				lengthIncreaseValues add: increaseInLength ]].! !

!methodRemoval: TextModelMorph #selectMessage stamp: 'jmv 3/28/2023 14:56:42'!
TextModelMorph removeSelector: #selectMessage!
!methodRemoval: TextModelMorph #selectString stamp: 'jmv 3/28/2023 14:56:45'!
TextModelMorph removeSelector: #selectString!
!classDefinition: #SHTextStylerST80 category: #'Tools-Syntax Highlighting'!
SHTextStyler subclass: #SHTextStylerST80
	instanceVariableNames: 'classOrMetaClass workspace parser disableFormatAndConvert lengthIncreasePositions lengthIncreaseValues'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
