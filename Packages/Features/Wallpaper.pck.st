'From Cuis 5.0 of 7 November 2016 [latest update: #3667] on 17 March 2019 at 5:04:36 pm'!
'Description Please enter a description for this package'!
!provides: 'Wallpaper' 1 1!
!requires: 'Graphics-Files-Additional' 1 22 nil!
SystemOrganization addCategory: #Wallpaper!


!classDefinition: #WallpaperChanger category: #Wallpaper!
Object subclass: #WallpaperChanger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wallpaper'!
!classDefinition: 'WallpaperChanger class' category: #Wallpaper!
WallpaperChanger class
	instanceVariableNames: ''!


!WallpaperChanger methodsFor: 'user interface' stamp: 'FJG 3/17/2019 16:24:01'!
changeWallpaperTo: anImageFileEntry 
	self runningWorld backgroundImageData: (anImageFileEntry readStream) binary contentsOfEntireFile! !

!WallpaperChanger class methodsFor: 'file list services' stamp: 'FJG 3/17/2019 16:56:22'!
fileReaderServicesForFile: filename suffix: suffix
	(#('jpg' 'jpeg' 'png') includes: suffix)
		ifTrue: [ ^ {self serviceToChangeWallpaper}  ]
		ifFalse: [ ^#() ]! !

!WallpaperChanger class methodsFor: 'file list services' stamp: 'FJG 3/17/2019 16:24:19'!
serviceToChangeWallpaper
	^ (SimpleServiceEntry 
		provider: self new
		label: 'use as wallpaper'
		selector: #changeWallpaperTo:
		description: 'uses this file as the new world wallpaper'
		buttonLabel: 'content'
		icon: #wallpaperIcon)
			sortOrder: 25;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !
