'From Cuis 4.1 of 12 December 2012 [latest update: #1698] on 25 May 2013 at 10:15:35.573037 am'!

!SystemWindow class methodsFor: 'top window' stamp: 'jmv 5/25/2013 10:14'!
noteTopWindowIn: aWorld but: aWindow
	| newTop |
	"TopWindow must be nil or point to the top window in this project."
	TopWindow _ nil.
	aWorld ifNil: [^ nil].
	newTop := aWorld submorphs 
				detect: [:m | (m is: #SystemWindow) and: [m ~~ aWindow]]
				ifNone: [^nil].
	newTop activate.
	^newTop! !

