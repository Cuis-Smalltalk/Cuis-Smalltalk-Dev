'From Cuis7.7 [latest update: #7779] on 23 December 2025 at 4:40:44 pm'!
'Description '!
!provides: 'Wallpaper' 1 5!
!requires: 'Graphics-Files-Additional' 1 22 nil!
SystemOrganization addCategory: #Wallpaper!


!classDefinition: #WallpaperChanger category: #Wallpaper!
Object subclass: #WallpaperChanger
	instanceVariableNames: 'world'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wallpaper'!
!classDefinition: 'WallpaperChanger class' category: #Wallpaper!
WallpaperChanger class
	instanceVariableNames: ''!


!WallpaperChanger commentStamp: 'jmv 10/24/2020 16:54:40' prior: 0!
Changes the wallpaper of a given WorldMorph.

Example usage:

WallpaperChanger forRunningWorld changeTo: 'pattern-malachite-green-wallpaper.jpeg' asFileEntry
!

!WallpaperChanger methodsFor: 'initialization' stamp: 'EB 24/Apr/2019 17:47:28'!
initializeWithWorld: aWorld 
	world _ aWorld.! !

!WallpaperChanger methodsFor: 'changing' stamp: 'EB 24/Apr/2019 17:51:53'!
changeTo: anImageFileEntry
	world backgroundImageData: anImageFileEntry binaryContents.! !

!WallpaperChanger class methodsFor: 'file list services' stamp: 'pb 26/Feb/2020 13:50:58'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	(#('jpg' 'jpeg' 'png') includes: suffix)
		ifTrue: [ ^ {self serviceToChangeWallpaper}  ]
		ifFalse: [ ^#() ]! !

!WallpaperChanger class methodsFor: 'file list services' stamp: 'jmv 25/Apr/2019 12:03:03'!
serviceToChangeWallpaper
	^ (SimpleServiceEntry 
		provider: self forRunningWorld
		label: 'use as wallpaper'
		selector: #changeTo:
		description: 'uses this file as the new world wallpaper'
		buttonLabel: 'content'
		icon: #wallpaperIcon)
			sortOrder: 25;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!WallpaperChanger class methodsFor: 'instance creation' stamp: 'EB 24/Apr/2019 22:30:53'!
forRunningWorld
	^self new initializeWithWorld: self runningWorld.! !

!WallpaperChanger class methodsFor: 'instance creation' stamp: 'jmv 25/Apr/2019 12:04:40'!
forWorld: aWorld
	^self new initializeWithWorld: aWorld.! !
