'From Cuis 6.0 [latest update: #5035] on 3 January 2022 at 10:31:34 pm'!

!Theme methodsFor: 'tool colors' stamp: 'MM 1/3/2022 22:29:24'!
packageDownloader
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color fromHexString: '#d0e973'` ]! !

!Theme methodsFor: 'tool colors' stamp: 'MM 1/3/2022 22:30:22'!
packageInstaller
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color fromHexString: '#88e6d8'` ]! !


!PackageInstallerWindow methodsFor: 'GUI building' stamp: 'MM 1/3/2022 22:30:37'!
windowColor
	^ Theme current packageInstaller! !

