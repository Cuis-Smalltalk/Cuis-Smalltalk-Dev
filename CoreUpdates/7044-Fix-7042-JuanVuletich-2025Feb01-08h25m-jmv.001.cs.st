'From Cuis7.3 [latest update: #7043] on 1 February 2025 at 8:27:09 am'!

!TextModelMorph methodsFor: 'focus handling' stamp: 'jmv 2/1/2025 08:26:52'!
tabToFocus: aBoolean
	"Enable or disable #tabToFocus.
	Disabled means that tab is valid character input."
	scroller tabToFocus: aBoolean! !


!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 2/1/2025 08:25:01'!
tabToFocus: aBoolean
	"Enable or disable #tabToFocus.
	Disabled means that tab is valid character input."
	^self setProperty: #tabIsValidInput toValue: aBoolean not! !

