'From Cuis 5.0 [latest update: #4351] on 18 August 2020 at 4:34:06 pm'!

!Preferences class methodsFor: 'start up' stamp: 'HAW 9/9/2017 12:07:37'!
checkLostChangesOnStartUp
	^ self
		valueOfFlag: #checkLostChangesOnStartUp
		ifAbsent: [ true ].! !

