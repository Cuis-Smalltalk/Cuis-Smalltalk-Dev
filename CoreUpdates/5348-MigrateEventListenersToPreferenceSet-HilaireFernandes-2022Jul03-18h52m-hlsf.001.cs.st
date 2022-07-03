'From Cuis 6.0 [latest update: #5347] on 3 July 2022 at 7:02:28 pm'!
!methodRemoval: PreferenceSet class #menuIcon: stamp: 'jmv 7/3/2022 18:52:56'!
PreferenceSet class removeSelector: #menuIcon:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
(Preferences allPreferences select: [:each |
	each actionMap notNil ]) valuesDo: [:aPref | | messageSend |
		messageSend _ aPref actionMap at: #preferenceChanged.
		messageSend receiver = Preference ifTrue: [
		messageSend receiver: PreferenceSet]]!

