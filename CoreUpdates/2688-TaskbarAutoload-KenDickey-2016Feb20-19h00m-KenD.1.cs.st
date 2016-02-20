'From Cuis 4.2 of 25 July 2013 [latest update: #2687] on 20 February 2016 at 7:02:04.466369 pm'!

!PasteUpMorph methodsFor: 'taskbar' stamp: 'KenD 2/17/2016 17:24'!
minimize: aMorph

	self showTaskbar.
	taskbar
		ifNil: [ 
			(PopUpMenu 
				confirm: 'The Taskbar is not loaded', String newLineString, 
				'Would you like me to load it for you now?')
			ifTrue:  [Feature require: #'Taskbar'. 
					 self showTaskbar.
					 taskbar minimize: aMorph
			] 
			ifFalse: [ self inform: 'Please install optional package Taskbar.pck.st', String newLineString, 
					 'Feature require: #''Taskbar'' ']
		]
		ifNotNil: [ taskbar minimize: aMorph ]! !

