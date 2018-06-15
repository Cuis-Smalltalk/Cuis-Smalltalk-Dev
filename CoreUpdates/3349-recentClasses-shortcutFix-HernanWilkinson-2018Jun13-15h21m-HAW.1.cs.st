'From Cuis 5.0 of 7 November 2016 [latest update: #3345] on 13 June 2018 at 4:12:00 pm'!

!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'HAW 6/13/2018 16:07:19'!
systemCatListKey: aChar from: view

	aChar == $r ifTrue: [^ model recent ].
	
	^super systemCatListKey: aChar from: view! !

