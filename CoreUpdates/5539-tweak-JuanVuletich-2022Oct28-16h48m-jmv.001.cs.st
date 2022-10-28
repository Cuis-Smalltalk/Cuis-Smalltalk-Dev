'From Cuis 6.0 [latest update: #5538] on 28 October 2022 at 4:50:08 pm'!

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 10/26/2022 19:52:07'!
worldMenuOptions

	^`
		TheWorldMenu worldMainMenuOptions,
		TheWorldMenu additionalMenuOptions,
		TheWorldMenu preferencesMenuOptions,
		TheWorldMenu windowsMenuOptions,
		TheWorldMenu helpMenuOptions,
		TheWorldMenu changesMenuOptions,
		TheWorldMenu debugMenuOptions `! !

