'From Cuis 4.0 of 21 April 2012 [latest update: #1371] on 17 August 2012 at 10:14:48 pm'!

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 8/17/2012 22:06'!
xIllegal
	"An illegal character was encountered"
	self notify: 'Illegal character (char code ' , hereChar asciiValue printString, ' ', hereChar asciiValue hex , ')' at: mark! !


!Parser class methodsFor: 'class initialization' stamp: 'jmv 8/17/2012 22:10'!
initialize
		
	Preferences
		addPreference: #allowBlockArgumentAssignment 
		category: #compiler 
		default: false
		balloonHelp: 'If enabled, the compiler will allow assignment into block arguments.\This provides backward compatibility with the pre-closure compiler.' withNewLines.
	Preferences
		addPreference: #allowUnderscoreAssignments 
		category: #compiler 
		default: true
		balloonHelp: 'When true, $_ (left arrow / underscore) can be used as assignment operator'.
	Preferences
		addPreference: #allowUnderscoreSelectors 
		category: #compiler 
		default: true
		balloonHelp: 'When true, $_ (left arrow / underscore) can be used in selectors and variable names'! !

Parser initialize!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Parser initialize.
Scanner initialize!

