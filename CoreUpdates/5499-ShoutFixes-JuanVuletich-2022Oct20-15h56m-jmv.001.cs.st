'From Cuis 6.0 [latest update: #5496] on 20 October 2022 at 3:56:47 pm'!

!SHTextStyler methodsFor: 'private' stamp: 'jmv 10/20/2022 15:51:17'!
privateFormatAndConvertWorkspace
	self subclassResponsibility ! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 10/20/2022 15:49:16'!
convertAssignmentsToLeftArrowAndSpace
	"If the Preference is to show leftArrowAssignments then answer a copy of  <aText> where each ansi assignment (:=) is replaced with a left arrow. A parser is used so that each ':=' is only replaced if it actually occurs within an assigment statement"

	self replaceStringForRangesWithType: #ansiAssignment with: '_ '! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 10/20/2022 15:53:22'!
privateFormatAndConvertWorkspace
	"Perform any formatting of formattedText necessary and store or a formatted copy in formattedText"
	(Preferences at: #syntaxHighlightingAsYouTypeLeftArrowAssignment) ifTrue: [
		self convertAssignmentsToLeftArrowAndSpace ]! !


!Editor methodsFor: 'private' stamp: 'jmv 10/20/2022 13:06:23'!
moveCursor: directionBlock forward: forward event: aKeyboardEvent
	"Private - Move cursor.
	directionBlock is a one argument Block that computes the new Position from a given one."
	| shift indices newPosition |
	shift _ aKeyboardEvent notNil and: [aKeyboardEvent shiftPressed].
	indices _ self
		setIndices: shift
		forward: forward.
	newPosition _ directionBlock value: (indices at: #moving).
	shift
		ifTrue: [
			self
				selectMark: (indices at: #fixed)
				point: newPosition - 1 ]
		ifFalse: [ self deselectAndPlaceCursorAt: newPosition ]! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 10/20/2022 12:39:40'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	aBoolean == hasUnacceptedEdits ifFalse: [
		hasUnacceptedEdits _ aBoolean].
	aBoolean ifFalse: [ hasEditingConflicts _ false].

	aBoolean ifTrue: [
		self formatAndStyleIfNeeded ].
	needsFit _ aBoolean.! !

!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 10/20/2022 14:52:57'!
formatAndStyleIfNeeded
	"Apply both formatting (changes to the characters in the text, such as 	preferred assignment operators),
	and styling (TextAttributes to make Smalltalk code easier to understand)"

	| previousSize delta isAtEnd |
	previousSize _ model actualContents size.
	isAtEnd _ editor stopIndex > previousSize.
	model formatAndStyleIfNeededWith: styler.
	delta _ model actualContents size - previousSize.
	(isAtEnd and: [ delta < 0 ]) ifTrue: [
		delta _ 0].
	delta = 0 ifFalse: [
		editor moveCursor: [ :p | p+delta] forward: delta>0 event: nil ].! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 10/20/2022 15:51:27'!
formatWorkspace: text
	"Do first stage of styling.
	Afterwards, call #styleWorkspaceFrom:to: as needed.
	Note: classOrMetaClass is assumed to be nil"

	self terminateBackgroundStylingProcess.
	formattedText _ text.
	self privateFormatAndConvertWorkspace.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	
	parser ifNil: [ parser := SHParserST80 new ].
	parser
		workspace: workspace;
		classOrMetaClass: nil! !

