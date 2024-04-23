'From Cuis6.3 [latest update: #6323] on 23 April 2024 at 10:02:05 am'!

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'jmv 4/23/2024 10:01:11'!
lastCharacterWantsAutoComplete

	| lastCharIndex lastCharacter |
	textMorph editor hasSelection ifTrue: [ ^ false ].

	lastCharIndex := textMorph editor startIndex-1.
	lastCharIndex <= 0 ifTrue: [ ^ false ].
	lastCharacter := model actualContents at: lastCharIndex.

	(lastCharacter = Character space and: [
		Preferences at: #spaceTabOpensAutoCompletion ])
			ifTrue: [ ^true ].

	^ lastCharacter isAlphaNumeric or: [
		lastCharacter isValidInBinarySelectors or: [ lastCharacter = $: ]]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #spaceTabOpensAutoCompletion category: #gui value: false.!

