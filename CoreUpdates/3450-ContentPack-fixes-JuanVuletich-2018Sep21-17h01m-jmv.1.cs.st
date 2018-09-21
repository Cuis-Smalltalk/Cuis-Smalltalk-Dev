'From Cuis 5.0 of 7 November 2016 [latest update: #3449] on 21 September 2018 at 5:02:42 pm'!

!ContentPack methodsFor: 'importing' stamp: 'jmv 9/21/2018 16:13:27'!
loadContentFrom: aDirectoryEntry
	
	| contentPacks |

	(self supportedFilesIn: aDirectoryEntry) do: [ :filename |			
		self flag: #todo. "Add hook for other media types here. Also consider renaming this method. --cbr"
		self at: filename name 
			put: (self import: [ Form fromFileEntry: filename ]) "This may yet be a cross-cutting concern, and need to be refactored when other media types become present. --cbr"
	].

	contentPacks _ aDirectoryEntry directoryNames collect: [ :i |
		i ->  (ContentPack new loadContentFrom: aDirectoryEntry / i)
	].
	
	^ self union: (contentPacks as: Dictionary)! !


!ContentPack class methodsFor: 'instance creation' stamp: 'jmv 9/21/2018 16:30:21'!
import
	"
	Feature require: 'Graphics-Files-Additional'.
	Theme content export.
	ContentPack import.
	Theme bootstrap.
	"
	
	^ self new loadContentFrom: self exportDirectory! !

!ContentPack class methodsFor: 'configuration' stamp: 'jmv 9/21/2018 16:07:27'!
exportDirectory
	
	^ DirectoryEntry smalltalkImageDirectory / self defaultContentDirectory / 'Exported'! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 9/21/2018 16:56:49'!
putForm: aForm onFile: fileEntry
	"Store the given form on a file of the given name."

	fileEntry forceWriteStreamDo: [ :stream |
		(self onBinaryStream: stream binary) nextPutImage: aForm ]! !


!ContentPack methodsFor: 'exporting' stamp: 'jmv 9/21/2018 16:07:38'!
export

	"Answer true on success."

	"self break."

	self class exportDirectory exists
		ifTrue: [ Utilities inform: 
			'Before you can export, you must move, rename or delete this directory: ' ,
			self exportDirectory pathName.
			
			^ false.
		].
	
	self exportInto: self class exportDirectory.
	
	^ true! !

!ContentPack methodsFor: 'exporting' stamp: 'jmv 9/21/2018 16:58:00'!
exportInto: aDirectory

	| featureName |
	featureName _ 'Graphics-Files-Additional'.
	(FeatureRequirement name: featureName) isAlreadySatisfied
		ifFalse: [
			self error: 'Please load "', featureName, '".'].

	aDirectory assureExistence.

	self associations do: [ :assoc |
		| klass thisDirectory fullPath | 
			klass _ assoc value class.
			thisDirectory _ aDirectory / assoc key.
			fullPath _  aDirectory // assoc key.

		self flag: #note. "Add support for new file export type mappings here. --cbr"
		klass = ContentPack
			ifTrue: [ assoc value exportInto: thisDirectory ].
		
		klass = ColorForm
			ifTrue: [ assoc value writePNGfile: fullPath ].
			
		klass = Form
			ifTrue: [ assoc value writePNGfile: fullPath ]
	]! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 9/21/2018 16:56:41'!
putForm: aForm onFileNamed: filename
	"Store the given form on a file of the given name."

	self putForm: aForm onFile: filename asFileEntry! !


!Theme methodsFor: 'icons' stamp: 'jmv 9/21/2018 16:24:43'!
resizeBottomIcon
	"
	Theme current resizeBottomIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-bottom' )! !

!Theme methodsFor: 'icons' stamp: 'jmv 9/21/2018 16:25:13'!
resizeBottomLeftIcon
	"
	Theme current resizeBottomLeftIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-bottom-left' )! !

!Theme methodsFor: 'icons' stamp: 'jmv 9/21/2018 16:25:35'!
resizeBottomRightIcon
	"
	Theme current resizeBottomRightIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-bottom-right' )! !

!Theme methodsFor: 'icons' stamp: 'jmv 9/21/2018 16:28:59'!
resizeFullIcon
	"
	Theme current resizeFullIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize' )! !

!Theme methodsFor: 'icons' stamp: 'jmv 9/21/2018 16:26:03'!
resizeLeftIcon
	"
	Theme current resizeLeftIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-left' )! !

!Theme methodsFor: 'icons' stamp: 'jmv 9/21/2018 16:26:25'!
resizeRightIcon
	"
	Theme current resizeRightIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-right' )! !

!Theme methodsFor: 'icons' stamp: 'jmv 9/21/2018 16:26:43'!
resizeTopIcon
	"
	Theme current resizeTopIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-top' )! !

!Theme methodsFor: 'icons' stamp: 'jmv 9/21/2018 16:27:13'!
resizeTopLeftIcon
	"
	Theme current resizeTopLeftIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-top-left' )! !

!Theme methodsFor: 'icons' stamp: 'jmv 9/21/2018 16:27:17'!
resizeTopRightIcon
	"
	Theme current resizeTopRightIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-top-right' )! !

!Theme methodsFor: 'icon lookup' stamp: 'jmv 9/21/2018 16:21:17'!
fetch: aTuple "	#( 'resolution' 'context' 'filename' )	"

	"Get an icon from Content. See icons protocol."

	| contentSpecifier icon themeGuess |
	
	icon _ nil.
	themeGuess _ self class.
	contentSpecifier _ self appendExtensionToContentSpec: aTuple.

	[ icon isNil ] 
		whileTrue: [
			icon _ self class content
				from: themeGuess name
				get: contentSpecifier.
	
			icon ifNotNil: [ ^ icon ].
	
			themeGuess = Theme
				ifTrue: [ ^ nil "See comment in ContentPack>>from:get: --cbr" ].
	
			themeGuess _ themeGuess superclass
		]! !


!Theme class methodsFor: 'importing/exporting' stamp: 'jmv 9/21/2018 16:08:35'!
bootstrap

	"Destructive. Loads up initial content."
	self flag: #todo. "Consider removal. --cbr"
	
	Content _ ContentPack import! !

!methodRemoval: ContentPack class #default!
ContentPack class removeSelector: #default!
!methodRemoval: ContentPack #exportDirectory!
ContentPack removeSelector: #exportDirectory!
!methodRemoval: ContentPack #path:!
ContentPack removeSelector: #path:!
