'From Haver 5.0 [latest update: #4589] on 8 May 2021 at 3:33:56 pm'!

!Theme methodsFor: 'tool colors' stamp: 'KLG 5/8/2021 15:12:47'!
autoCompleter
	"Answer the autocompleter's color for  entries."

	^ self text! !

!Theme methodsFor: 'tool colors' stamp: 'KLG 5/8/2021 15:26:51'!
autoCompleterDefaultBorderColor
	"Answer the auto comleters default border color."

	^ `Color gray`! !

!Theme methodsFor: 'tool colors' stamp: 'KLG 5/8/2021 15:09:00'!
autoCompleterMaybeInvalid
	"Answer the autocompleter's color for possible invalid entries."

	^ `Color blue`! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'KLG 5/8/2021 15:10:07'!
colorOf: entry

	^(completer isPossibleInvalidEntry: entry) 
		ifTrue: [ Theme current autoCompleterMaybeInvalid ] 
		ifFalse: [ Theme current autoCompleter ]
	! !

!AutoCompleterMorph methodsFor: 'initialization' stamp: 'KLG 5/8/2021 15:27:47'!
defaultBorderColor
	"My default border color.
	
	Note: My background color is derived from this color."
	^ Theme current autoCompleterDefaultBorderColor ! !

