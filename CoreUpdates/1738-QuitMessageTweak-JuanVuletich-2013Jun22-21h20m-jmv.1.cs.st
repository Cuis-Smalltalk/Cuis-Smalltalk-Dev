'From Cuis 4.1 of 12 December 2012 [latest update: #1736] on 22 June 2013 at 9:21:36.581988 pm'!

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 6/22/2013 21:21'!
quitSession
	Smalltalk 
		snapshot: (self confirm: 'Save the current image before quitting?' orCancel: [^ self])
		andQuit: true! !

