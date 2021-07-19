'From Cuis 5.0 [latest update: #4675] on 18 July 2021 at 8:45:13 pm'!

!Theme methodsFor: 'icons' stamp: 'jmv 7/18/2021 20:39:39'!
fontIcon
	^(Theme content from: 'Theme' get: {'16x16'. 'mimetypes'}) at: 'font-x-generic.png'! !

!Theme methodsFor: 'icons' stamp: 'jmv 7/18/2021 20:40:31'!
imageIcon
	^((Theme content from: 'Theme' get: {'16x16'. 'mimetypes'}) at: 'image-x-generic.png')! !


!SimpleServiceEntry methodsFor: 'accessing' stamp: 'jmv 7/18/2021 20:42:47'!
icon: iconSymbol

	icon := iconSymbol ! !


!SimpleServiceEntry class methodsFor: 'instance creation' stamp: 'jmv 7/18/2021 20:41:32'!
provider: anObject label: aString selector: aSymbol description: anotherString buttonLabel: aLabel icon: iconSymbol
	
	^ (self provider: anObject label: aString selector: aSymbol description: anotherString buttonLabel: aLabel)
		icon: iconSymbol;
		yourself! !

!SimpleServiceEntry class methodsFor: 'instance creation' stamp: 'jmv 7/18/2021 20:42:36'!
provider: anObject label: aString selector: aSymbol description: anotherString icon: iconSymbol

	^(self provider: anObject label: aString selector: aSymbol description: anotherString)
		icon: iconSymbol;
		yourself! !


!ImageReadWriter class methodsFor: 'instance creation' stamp: 'jmv 7/18/2021 20:40:40'!
serviceReadImage
	"Answer the service of importing an image"

	^ (SimpleServiceEntry
		provider: self 
		label: 'import as ImageMorph'
		selector: #imageMorphFromFileEntry:
		description: 'import image as ImageMorph'
		buttonLabel: 'import image'
		icon: #imageIcon
		) argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

