'From Cuis 5.0 [latest update: #3849] on 3 September 2019 at 11:42:23 pm'!

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'EB 9/3/2019 23:41:10'!
newLine: aKeyboardEvent
	"Replace the current text selection with a newLine (i.e. LF) followed by as many tabs
	as there are leading tabs on the current line (+/- bracket count)."

	| char s i tabCount stopIndex newLineString |
	s _ self privateCurrentString.
	stopIndex _ self stopIndex.
	i _ stopIndex.
	tabCount _ 0.
	[ (i _ i-1) > 0 and: [ (char _ s at: i) isLineSeparator not ] ] whileTrue: [
		"Count brackets"
		char = $[ ifTrue: [tabCount _ tabCount + 1].
		char = $] ifTrue: [tabCount _ tabCount - 1]].
	[ (i _ i + 1) < stopIndex and: [ (char _ s at: i) isSeparator ] ] whileTrue: [
		"Count leading tabs"
		char = Character tab ifTrue: [ tabCount _ tabCount + 1 ]].
	"Now inject newline with tabCount tabs, generating a new undoable command"
	newLineString _ String streamContents: [ :strm | strm newLineTab: tabCount ].
	self replaceSelectionWith: newLineString shouldMergeCommandsIfPossible: false.
	^ false! !

