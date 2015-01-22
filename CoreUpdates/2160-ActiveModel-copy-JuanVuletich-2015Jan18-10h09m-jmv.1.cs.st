'From Cuis 4.2 of 25 July 2013 [latest update: #2156] on 18 January 2015 at 10:12:38.533778 am'!

!ActiveModel methodsFor: 'copying' stamp: 'jmv 1/16/2015 16:14'!
postCopy
	super postCopy.
	actionMap _ nil! !


!TextModel methodsFor: 'copying' stamp: 'jmv 1/18/2015 10:11'!
postCopy
	super postCopy.
	actualContents _ actualContents copy.
	undoRedoCommands _ ReadWriteStream on: Array new.
	lastEditTimeStamp _ nil! !

!methodRemoval: ActiveModel #copy!
ActiveModel removeSelector: #copy!
