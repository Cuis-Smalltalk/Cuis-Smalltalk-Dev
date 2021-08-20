'From Cuis 5.0 [latest update: #4775] on 20 August 2021 at 12:00:08 pm'!

!Preferences class methodsFor: 'start up' stamp: 'jmv 8/20/2021 11:59:35'!
checkLostChangesOnStartUp
	^ self
		valueOfFlag: #checkLostChangesOnStartUp
		ifAbsent: [ false ].! !

