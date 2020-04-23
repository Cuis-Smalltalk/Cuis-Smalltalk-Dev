'From Cuis 5.0 [latest update: #4112] on 22 April 2020 at 10:49:25 pm'!

!ImageReadWriter class methodsFor: 'instance creation' stamp: 'KenD 4/22/2020 16:41:45'!
fileReaderServicesForFileEntry: filename suffix: suffix
	"FileList buttons that read a TrueType font on contents."

	( self allTypicalFileExtensions includes: suffix) ifTrue: [
		^ { self serviceReadImage } ].

	^#()! !

!ImageReadWriter class methodsFor: 'instance creation' stamp: 'KenD 4/22/2020 16:44:34'!
imageMorphFromFileEntry: imageFileEntry
	"Import an image from a file"
	
	(ImageMorph new image: (ImageReadWriter formFromFileEntry: imageFileEntry) ) openInWorld.
	! !

!ImageReadWriter class methodsFor: 'instance creation' stamp: 'KenD 4/22/2020 16:43:53'!
serviceReadImage
	"Answer the service of importing an image"

	^ (SimpleServiceEntry
		provider: self 
		label: 'import as ImageMorph'
		selector: #imageMorphFromFileEntry:
		description: 'import image as ImageMorph'
		buttonLabel: 'import image'
		icon: ((Theme content from: 'Theme' get: {'16x16'. 'mimetypes'}) at: 'image-x-generic.png')
		) argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 4/22/2020 22:41:52'!
formFromFileEntry: aFileEntry
	"Answer a Form stored on the file with the given name."
	
	^ aFileEntry readStreamDo: [ :stream |
		stream useBytes.
		self formFromStream: stream ]! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 4/22/2020 22:41:57'!
formFromFileNamed: fileName
	"Answer a Form stored on the file with the given name."
	
	^fileName asFileEntry readStreamDo: [ :stream |
		stream useBytes.
		self formFromStream: stream ]! !

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 4/22/2020 22:42:34'!
formFromStream: aBinaryStream
	"Answer a Form stored on the given stream."
	| reader readerClass form  featureName |

	readerClass _ self withAllSubclasses
		detect: [ :subclass | subclass understandsImageFormat: aBinaryStream reset ]
		ifNone: [
			featureName _ 'Graphics-Files-Additional'.
			^(FeatureRequirement name: featureName) isAlreadySatisfied
				ifTrue: [ self error: 'Unsupported image file format.' ]
				ifFalse: [
					self error: 'Unsupported image format. Try "', featureName, '".']].
	reader _ readerClass onBinaryStream: aBinaryStream reset.
	form _ reader nextImage.
	^ form! !

