'From Cuis 4.2 of 25 July 2013 [latest update: #2408] on 9 July 2015 at 10:20:34.184862 pm'!

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 7/9/2015 22:20'!
decorateBrowserButtons
	^ self
		valueOfFlag: #decorateBrowserButtons
		ifAbsent: [ true ].! !

