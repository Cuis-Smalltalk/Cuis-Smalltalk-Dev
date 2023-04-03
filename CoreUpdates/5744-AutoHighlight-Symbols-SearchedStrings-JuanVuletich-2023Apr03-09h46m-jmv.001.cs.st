'From Cuis 6.0 [latest update: #5743] on 3 April 2023 at 10:00:13 am'!

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 4/3/2023 09:55:11'!
privateFormatAndConvert

	| attr cs s start found hs |
	textModel shouldAutoHighlight ifTrue: [
		textModel autoSelectString ifNotNil: [ :autoHighlight |
			"Message sends. May include, for instance, arguments of keyword selectors."
			attr := `TextBackgroundColor color: (Color red alpha: 0.2)`.
			(textModel textProvider messageSendsRangesOf: autoHighlight) do: [ :interval |
				formattedText 
					addAttribute: attr
					from: interval first
					to: interval last ].
			"Searched text. Senders of symbols, including nested in literal arrays."
			cs := Preferences at: #caseSensitiveFinds.
			s := formattedText size.
			hs := autoHighlight size.
			start := 1.
			[start < s] whileTrue: [
				found := formattedText findString: autoHighlight startingAt: start caseSensitive: cs.
				start := s.
				found = 0 ifFalse: [
					formattedText addAttribute: attr from: found to: found+hs-1.
					start := found+hs ]]
			].
		textModel justDidAutoHighlight ].

	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow"
			self replaceStringForRangesWithType: #ansiAssignment with: `Character smalltalkLeftArrow asString`.
			"Turn $_ into a left arrow"
			self replaceStringForRangesWithType: #assignment with: `Character smalltalkLeftArrow asString`.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: `Character smalltalkUpArrow asString` ].! !

