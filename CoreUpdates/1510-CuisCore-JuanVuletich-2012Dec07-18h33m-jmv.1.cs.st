'From Cuis 4.0 of 21 April 2012 [latest update: #1509] on 7 December 2012 at 6:35:28 pm'!

!Process methodsFor: 'accessing' stamp: 'jmv 12/7/2012 18:14'!
isTerminated

	self isActiveProcess ifTrue: [^ false].
	^suspendedContext isNil | suspendedContext pc isNil
	  or: ["If the suspendedContext is the bottomContext it is the block in Process>>newProcess.
		   If so, and the pc is greater than the startpc, the bock has alrteady sent and returned
		   from value and there is nothing more to do."
		suspendedContext isBottomContext
		and: [
			suspendedContext pc > suspendedContext startpc]]! !


!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 12/7/2012 18:04'!
debugProcess: aProcess
"	self resumeProcess: aProcess."
	aProcess debugWithTitle: 'Interrupted from the Process Browser'.
! !


!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 12/7/2012 18:00'!
debugProcess
	| rule |
	rule _ (model class rulesFor: model selectedProcess) second.
	rule
		ifFalse: [PopUpMenu inform: 'Nope, won''t debug ' , model selectedProcess name.
			^ self].
	model class debugProcess: model selectedProcess.! !

