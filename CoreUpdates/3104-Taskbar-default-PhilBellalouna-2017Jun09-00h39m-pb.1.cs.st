'From Cuis 5.0 of 7 November 2016 [latest update: #3099] on 9 June 2017 at 2:09:35 am'!

!Preferences class methodsFor: 'personalization' stamp: 'pb 6/9/2017 00:39:48'!
taskbarIncludesAllWindows
	"
	true: All windows are included in Taskbar
	false: Only collapsed windows are included in Taskbar
	"
	^ self
		valueOfFlag: #taskbarIncludesAllWindows
		ifAbsent: [ false ].! !
