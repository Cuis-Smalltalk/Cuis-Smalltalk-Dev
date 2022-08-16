'From Cuis 6.0 [latest update: #5448] on 15 August 2022 at 4:18:29 pm'!

!TestRunner methodsFor: 'menus' stamp: 'jmv 8/15/2022 16:17:04'!
terminateRun
"
	running ifNotNil: [ running terminate. running _ nil ].
"
	running debugWithTitle: 'Interrupted Test'
! !

