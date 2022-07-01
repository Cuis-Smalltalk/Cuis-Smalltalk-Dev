'From Cuis 6.0 [latest update: #5335] on 30 June 2022 at 4:20:20 pm'!

!PreferenceSet class methodsFor: 'migration' stamp: 'hlsf 6/24/2022 10:08:59'!
migrate
" Migrate from Preference class var to PreferenceSet. 
I will copy the Preference dictionary into a PreferenceSet instance, 
then assign this PreferenceSet instance to the global variable Preferences "
| preferences |
	preferences _ self new.
	preferences instVarNamed: #contents put: Preference allPreferences.
	Smalltalk at: #Preferences put: preferences ! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

PreferenceSet migrate
!

