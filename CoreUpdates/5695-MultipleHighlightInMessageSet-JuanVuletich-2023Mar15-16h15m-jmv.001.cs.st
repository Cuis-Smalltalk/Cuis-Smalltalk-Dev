'From Cuis 6.0 [latest update: #5694] on 15 March 2023 at 4:34:09 pm'!

!MessageSet methodsFor: 'private' stamp: 'jmv 3/15/2023 16:17:15'!
autoSelectString
	"Return the string to be automatically highlighted.
	Note: Consider renaming this as autoHighlightString instead.
	Same for #acceptedContentsChanged, #autoSelect"


	^ autoSelectString! !

!MessageSet methodsFor: 'private' stamp: 'jmv 3/15/2023 16:19:45'!
autoSelectString: aString
	"Set the string to be automatically highlighted.
	Note: Consider renaming this as autoHighlightString instead.
	Same for #acceptedContentsChanged, #autoSelect, #selectMessage, #selectString, #updateAutoSelect."

	autoSelectString := aString! !


!TextModelMorph methodsFor: 'updating' stamp: 'jmv 3/15/2023 16:32:52'!
selectMessage
	"Actually highlight, no select.
	See comment at #autoSelectString"

	| messageSendsRanges theText attr |
	messageSendsRanges := model textProvider messageSendsRangesOf: model autoSelectString.
	messageSendsRanges isEmpty ifTrue: [ ^false ].
	theText := model actualContents.
	attr := `ShoutTextBackgroundColor color: (Color red alpha: 0.2)`.
	messageSendsRanges do: [ :r |
		 theText addAttribute: attr from: r first to: r last ].
	^true! !

!TextModelMorph methodsFor: 'updating' stamp: 'jmv 3/15/2023 16:32:29'!
selectString
	"Actually highlight, no select.
	See comment at #autoSelectString"

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

!TextModelMorph methodsFor: 'updating' stamp: 'jmv 3/15/2023 16:20:57'!
updateAutoSelect 

	TextEditor abandonChangeText.	"no replacement!!"
	self selectMessage
		ifFalse: [ self selectString ].! !

!methodRemoval: TextEditor #messageSendsRanges: stamp: 'jmv 3/15/2023 16:33:49'!
TextEditor removeSelector: #messageSendsRanges:!
