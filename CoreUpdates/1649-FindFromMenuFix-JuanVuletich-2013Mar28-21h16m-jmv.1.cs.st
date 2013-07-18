'From Cuis 4.1 of 12 December 2012 [latest update: #1648] on 28 March 2013 at 9:16:42 pm'!

!TextEditor methodsFor: 'new selection' stamp: 'jmv 3/27/2013 23:28'!
selectFrom: start to: stop
	"Select the specified characters inclusive.
	I.e. if contents is 'hello' and we want to select the $e, do 'self selectFrom: 2 to: 2'
	If we want to put cursor after e, but with no selection, do 'self selectFrom: 3 to: 2', or better yet, call #selectAt:"
	self selectInvisiblyFrom: start to: stop.
	self storeSelectionInParagraph.
	morph redrawNeeded.
	"Preserve current emphasis if selection is empty"
	start > stop ifTrue: [
		self setEmphasisHereFromText ]! !

!TextEditor methodsFor: 'private' stamp: 'jmv 3/27/2013 23:20'!
findAndReplaceOnce: indexStream
	"Find the next occurrence of FindText.  If none, answer false.
	Append the start index of the occurrence to the stream indices, and, if
	ChangeText is not the same object as FindText, replace the occurrence by it.
	Note that the search is case-sensitive for replacements, otherwise not."
	"This is a user command, and generates undo"

	| where |
	where _ model actualContents
		findString: FindText
		startingAt: self stopIndex
		caseSensitive: ((ChangeText ~~ FindText) or: [Preferences caseSensitiveFinds]).
	where = 0 ifTrue: [^ false].

	selectionStartBlocks add: (paragraph characterBlockForIndex: where).
	selectionStopBlocks add: (paragraph characterBlockForIndex: where + FindText size).
	self selectFrom: where to: where + FindText size - 1.	"Repeat it here. Senders beware: only one of these should last"

	ChangeText ~~ FindText ifTrue: [ self replaceSelectionWith: ChangeText ].
	indexStream nextPut: where.
	^ true! !

