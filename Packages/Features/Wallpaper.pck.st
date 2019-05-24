'From Cuis 5.0 of 7 November 2016 [latest update: #3712] on 25 April 2019 at 12:05:56 pm'!
'Description '!
!provides: 'Wallpaper' 1 3!
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

!WallpaperChanger class methodsFor: 'file list services' stamp: 'FJG 3/17/2019 16:56:22'!
fileReaderServicesForFile: filename suffix: suffix
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
