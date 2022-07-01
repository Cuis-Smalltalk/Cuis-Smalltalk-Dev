'From Cuis 6.0 [latest update: #5341] on 30 June 2022 at 7:32:06 pm'!
!methodRemoval: Preference class #selectCategory: stamp: 'jmv 6/30/2022 19:31:43'!
Preference class removeSelector: #selectCategory:!
!methodRemoval: Preference class #at:put: stamp: 'jmv 6/30/2022 19:31:25'!
Preference class removeSelector: #at:put:!
!methodRemoval: Preference class #categories stamp: 'jmv 6/30/2022 19:31:33'!
Preference class removeSelector: #categories!
!methodRemoval: Preference class #instanceAt: stamp: 'jmv 6/30/2022 19:31:37'!
Preference class removeSelector: #instanceAt:!
!methodRemoval: Preference class #allPreferences stamp: 'jmv 6/30/2022 19:21:56'!
Preference class removeSelector: #allPreferences!

!Preference class reorganize!
('accessing' at:)
('error handling' doesNotUnderstand:)
('helpers' detectType:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
PreferenceSet installMiscPreferences!

