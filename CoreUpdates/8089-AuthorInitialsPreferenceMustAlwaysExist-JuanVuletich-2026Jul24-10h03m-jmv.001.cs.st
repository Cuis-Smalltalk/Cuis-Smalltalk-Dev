'From Cuis7.9 [latest update: #8088] on 24 July 2026 at 10:13:56 am'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
(Preferences includesKey: #authorInitials) ifFalse: [ Preferences name: #authorInitials category: #programming value: '' ].
(Preferences includesKey: #authorName) ifFalse: [ Preferences name: #authorName category: #programming value: '' ].
!

