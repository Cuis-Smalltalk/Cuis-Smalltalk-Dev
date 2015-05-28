'From Cuis 4.2 of 25 July 2013 [latest update: #2338] on 27 May 2015 at 12:38:19.155362 pm'!


!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 5/27/2015 10:43'!
hideTaskbar
	taskbar ifNotNil: [
		taskbar delete.
		taskbar _ nil ]! !


!Taskbar methodsFor: 'change reporting' stamp: 'jmv 5/27/2015 10:12'!
delete

	| w |
	self restoreAll.
	super delete.
	w _ self world ifNil: [ self runningWorld ].
	Display removeActionsWithReceiver: self.
	w ifNotNil: [ w taskbar: nil ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Compiler recompileAll!

