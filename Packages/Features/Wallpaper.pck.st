'From Cuis 5.0 [latest update: #4048] on 26 February 2020 at 1:51:36 pm'!
'Description '!
!provides: 'Wallpaper' 1 4!
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


!WallpaperChanger commentStamp: 'EB 4/24/2019 17:53:39' prior: 0!
Changes the wallpaper of a given PasteUpMorph.

Example usage:

WallpaperChanger forRunningWorld changeTo: 'pattern-malachite-green-wallpaper.jpeg' asFileEntry
!

!WallpaperChanger methodsFor: 'initialization' stamp: 'EB 4/24/2019 17:47:28'!
initializeWithWorld: aWorld 
	world _ aWorld.! !

!WallpaperChanger methodsFor: 'changing' stamp: 'EB 4/24/2019 17:51:53'!
changeTo: anImageFileEntry
	world backgroundImageData: anImageFileEntry binaryContents.! !

!WallpaperChanger class methodsFor: 'file list services' stamp: 'pb 2/26/2020 13:50:58'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	(#('jpg' 'jpeg' 'png') includes: suffix)
		ifTrue: [ ^ {self serviceToChangeWallpaper}  ]
		ifFalse: [ ^#() ]! !

!WallpaperChanger class methodsFor: 'file list services' stamp: 'jmv 4/25/2019 12:03:03'!
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

!WallpaperChanger class methodsFor: 'instance creation' stamp: 'EB 4/24/2019 22:30:53'!
forRunningWorld
	^self new initializeWithWorld: self runningWorld.! !

!WallpaperChanger class methodsFor: 'instance creation' stamp: 'jmv 4/25/2019 12:04:40'!
forWorld: aWorld
	^self new initializeWithWorld: aWorld.! !
