'From Cuis 6.0 [latest update: #5809] on 24 May 2023 at 4:32:41 pm'!

!CodeProvider class methodsFor: 'services' stamp: 'jmv 5/24/2023 16:28:06'!
normalizeCodeForCodeFiles: aTextOrString
	"Adapt edited code to be suitable or logging.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' for assignment, '^' for return, and any replacements of
	ASCII by Unicode symbols done by Shout. See SHTextStylerST80 >> #privateConvert"

	| answer |
	answer := aTextOrString asPlainString.
	answer := answer copyReplaceAll: `Character smalltalkLeftArrow asString, UnicodeCodePoint zeroWidthSpace asString` with: ':='.
	answer := answer copyReplaceAll: `Character smalltalkLeftArrow asString` with: ':='.
	answer := answer copyReplaceAll: `Character smalltalkUpArrow asString` with: '^'.
	^answer! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 5/24/2023 16:29:12'!
privateConvert

	| attr cs s start found hs |
	textModel shouldAutoHighlight ifTrue: [
		textModel textProvider autoHighlightString ifNotNil: [ :autoHighlight |
			"Message sends. May include, for instance, arguments of keyword selectors."
			attr := `TextBackgroundColor color: (Color red alpha: 0.2)`.
			(textModel textProvider messageSendsRangesOf: autoHighlight) do: [ :interval |
				formattedText 
					addAttribute: attr
					from: interval first
					to: interval last ].
			"Searched text. Senders of symbols, including nested in literal arrays."
			textModel textProvider autoHighlightAllOccurrences ifTrue: [
				cs := Preferences at: #caseSensitiveFinds.
				s := formattedText size.
				hs := autoHighlight size.
				start := 1.
				[start < s] whileTrue: [
					found := formattedText findString: autoHighlight startingAt: start caseSensitive: cs.
					start := s.
					found = 0 ifFalse: [
						formattedText addAttribute: attr from: found to: found+hs-1.
						start := found+hs ]]]
			].
		textModel textProvider justDidAutoHighlight ].

	"See also #normalizeCodeForCodeFiles:"
	"Turn $_ into a left arrow"
	self replaceStringForRangesWithType: #assignment with: `Character smalltalkLeftArrow asString`.
	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow"
			self
				replaceStringForRangesWithType: #ansiAssignment
				with: (textModel shoutMustKeepContentsSize
					ifTrue: [`Character smalltalkLeftArrow asString, UnicodeCodePoint zeroWidthSpace asString`]
					ifFalse: [`Character smalltalkLeftArrow asString`]).
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: `Character smalltalkUpArrow asString` ].! !

