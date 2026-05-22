"
ContentPack lets you read in and write out the (supported files in the) contents of a directory on your file system. It also allows you to trivially create "messenger" subclasses that capture the information containted in these directory trees, including any implicit communication that's there in the structure of the directory hierarchy itself, which are captured in your changes file. You can then file out a change set that contains a representation of the (supported file/object types and directory structurein) the stuff on your disk, or in your image. This subclass is a dummy which ContentPack compiles methods into containing base 64 encoded data. You can load this into another image, as long as that image has ContentPack loaded. The filed in class can then recreate the ContentPack on the other end with the media files and structure intact.

The current implementation is based on #storeString, but the plan is to change that to SmartRefStream in the long run to support serializing things like morphs.

ContentPack instances hang onto the actual tree of media objects. It has a nice simple EDSL that just interprets an array of strings from beginning to end as a "path" to a file (really a series of dictionary lookups to a Smalltalk object, wherin the dictionaries mirror the structure of what was on the disk, sans unsupported files.) This mechanism will likely change a little bit at some point, 

ContentPack came into the world a little faster than I expected, as I ended up using it to send some icons back in time to fix the Cuis update stream without having to sort my changes all over again. As such it had some unusual design pressures... it had to be able to carry information in and out of both the change set stream and the filesystem, as well as function in a slightly earlier (unreleased) version of Cuis than it was written in, and not break anything on it's way back up through the build to head.

The code, in particular the way things are named, has not settled yet, and that's why this comment contains no code examples. Use with care and read the code first, for now.

Currently, .bmp import and .png import are implemented, and both can be exported. Anything you can import, you can also shuffle into a change set. Plans are in the works to support audio, change sets, and text files. I'll support video if someone has a good importer, exporter, and player under the MIT license that'll work under Cuis. 

Currently, objects are serialized into single methods, which works for small icons, but likely doesn't work well (if at all) for larger files. My intent is to add some behavior that breaks up large objects into smaller chunks so that this becomes a non-issue. I'll likely get to that when I've removed most of the repetitive subtle variations of the same recursive tree walking visitor-trick from the code, and renamed everything. I think in essence this class is slightly smaller than it is as represented currently.

Hopefully I will be able to explain all of this better once I've clarified the code a bit so that I can show off some examples.

	- cbr

-----------------------------------
Alternative description (need to merge both!)

Forms (and potentially other media types) can exist in three forms:

1) As external files, such as jpg, png, etc. This is the representation we need to use external tools (such as image processing apps, cameras, scanners, web, etc) to work on them.
2) As methods. Non human readable, base-64 encoded binary data. We need this to be able to include such stuff in the update stream, or in packages. After we update an image, we usually delete these methods, just keeping 3).
3) Live objects in the image, for example, stored in class variables. This is to make use of them in Cuis.

Most of the time, we use 3). But we need 2) for the update stream. We also need 1) sometimes to work on them. ContentPack supports the conversion between these 3 formats. The implementation is quite simple. What is really great is that Casey realized we need some tool to move comfortably between these 3 representations. And he also implemented it.

	- jmv
	
-------------------------------------------
Usage hints

Feature require: 'Graphics-Files-Additional'.
Theme content export.

"Build 'Import' directory copying the structure of 'Exported', with stuff to be loaded"

"Just build an instance from files"
ContentPack import.

"Build an instance and generate code"
ContentPack generateCode: ContentPack import.

"Build an instance from code generated"
ContentPack decode.

"Build and merge. Usually do this in the postscript of the change set that includes the generated code"
Theme content merge: ContentPack decode.
ContentPack removeCategory: ContentPack generatedMethodsCategory
"
Class {
	#name : 'ContentPack',
	#superclass : 'Dictionary',
	#classInstVars : [
		'payload',
		'count'
	],
	#category : 'Graphics-Themes',
	#package : 'Graphics',
	#tag : 'Themes'
}

{ #category : 'code pack' }
ContentPack class >> compilePayloadWith: contentMap [

	| category |
	category := self generatedMethodsCategory.
	self
		compile: 'contentMap' , String newLineString , '	^ ' , contentMap asString
		classified: category.

	self
		compile: 'objectCount' , String newLineString , '	^ ' , payload size asString
		classified: category.

	payload withIndexDo: [ :blob :index | | selector |
		selector := 'object', index printString.
		self
			compile: selector, String newLineString, '	^ ', blob surroundedBySingleQuotes
			classified: category ]
]

{ #category : 'instance creation' }
ContentPack class >> decode [
	
	^ self new decodeContent
]

{ #category : 'configuration' }
ContentPack class >> defaultContentDirectory [

	^ 'Content'
]

{ #category : 'code pack' }
ContentPack class >> encodeContentFrom: aDictionary [

	| encodedTree |
	
	encodedTree := Dictionary new.
	
	aDictionary associations
		do: [ :assoc |
			assoc value class == Dictionary
				ifTrue: [ 
					encodedTree 
						at: assoc key 
						put: (self encodeContentFrom: assoc value) 
				]
				ifFalse: [ 
					encodedTree 
						at: assoc key
						put: count.
					count := count + 1.
					payload add: assoc value storeString asByteArray base64Encoded
				]
		].

	^ encodedTree associations collect: [ :assoc | 
		{ assoc key . assoc value }
	]
]

{ #category : 'configuration' }
ContentPack class >> exportDirectory [
	
	^ DirectoryEntry userBaseDirectory / self defaultContentDirectory / 'Exported'
]

{ #category : 'code pack' }
ContentPack class >> generateCode: aDictionary [

	| contentMap |

	self resetImporter.
	
	contentMap := self encodeContentFrom: aDictionary.
	
	self compilePayloadWith: contentMap.
	
	self resetImporter.
]

{ #category : 'configuration' }
ContentPack class >> generatedMethodsCategory [
	^ 'generated code'
]

{ #category : 'instance creation' }
ContentPack class >> import [
	"
	Feature require: 'Graphics-Files-Additional'.
	Theme content export.
	ContentPack import.
	Theme bootstrap.
	"
	
	^ self new loadContentFrom: self importDirectory
]

{ #category : 'configuration' }
ContentPack class >> importDirectory [
	
	^ DirectoryEntry userBaseDirectory / self defaultContentDirectory / 'Import'
]

{ #category : 'configuration' }
ContentPack class >> mapping [

	^ {
		ColorForm -> #bmp .
		Form -> #png
	}
]

{ #category : 'code pack' }
ContentPack class >> resetImporter [

	count := 1.
	payload := OrderedCollection new
]

{ #category : 'converting' }
ContentPack >> asContentPack: aDictionary [

	"Recursively make ContentPacks of all objects which include behavior from Dictionary."

	| associations |

	associations := aDictionary keys collect: [ :key | 
		((aDictionary at: key) is: #Dictionary)
			ifTrue: [ key -> (self asContentPack: (aDictionary at: key)) ]
			ifFalse: [ key -> (aDictionary at: key) ]	
	].

	^ associations as: ContentPack
]

{ #category : 'converting' }
ContentPack >> asDictionary [

	"Recursively make Dictionaries of all objects which include behavior from ContentPack."

	| associations |

	associations := self keys collect: [ :key | 
		((self at: key) is: #ContentPack)
			ifTrue: [ key -> (self at: key) asDictionary ]
			ifFalse: [ key -> (self at: key) ]	
	].

	^ associations as: Dictionary
]

{ #category : 'importing' }
ContentPack >> decodeContent [
	"Load content in us.
	Start with an empty instance!"

	self decodeContentFrom: self contentMap
]

{ #category : 'importing' }
ContentPack >> decodeContentFrom: associationList [
	"Load content in us, frin geberated code.
	Start with an empty instance!"

	associationList do: [ :assoc |
		(assoc at: 2) class == Array
			ifTrue: [ 
				self at: (assoc at: 1) put: (ContentPack new decodeContentFrom: (assoc at: 2)) ]
			ifFalse: [ 
				self at: (assoc at: 1) put: (Compiler 
					evaluate: (self 
						perform: 
							('object' , (assoc at: 2) asString) asSymbol) base64Decoded)
			]
	]
]

{ #category : 'exporting' }
ContentPack >> export [

	"Answer true on success."

	"self break."

	self class exportDirectory exists
		ifTrue: [ Utilities inform: 
			'Before you can export, you must move, rename or delete this directory: ' ,
			self exportDirectory pathName.
			
			^ false.
		].
	
	self exportInto: self class exportDirectory.
	
	^ true
]

{ #category : 'exporting' }
ContentPack >> exportInto: aDirectory [

	| featureName |
	featureName := 'Graphics-Files-Additional'.
	(FeatureRequirement name: featureName) isAlreadySatisfied
		ifFalse: [
			self error: 'Please load "', featureName, '".'].

	aDirectory assureExistence.

	self associations do: [ :assoc |
		| klass thisDirectory fullPath | 
			klass := assoc value class.
			thisDirectory := aDirectory / assoc key.
			fullPath :=  aDirectory // assoc key.

		self flag: #note. "Add support for new file export type mappings here. --cbr"
		klass = ContentPack
			ifTrue: [ assoc value exportInto: thisDirectory ].
		
		klass = ColorForm
			ifTrue: [ assoc value writePNGfile: fullPath ].
			
		klass = Form
			ifTrue: [ assoc value writePNGfile: fullPath ]
	]
]

{ #category : 'converting' }
ContentPack >> fixFormKeysIn: aContentPack [

	"Need to add file extension to the keys for the Forms; 
	it made sense to strip them out before I realized that 
	ContentPack could be used for other media types, 
	which suddenly meant that file type was no longer implicit.."

| associations |

	self flag: #todo. "Remove this in a later change set. This is only needed once to get the icons working right again."


	associations := aContentPack keys collect: [ :key |
		((aContentPack at: key) is: #Dictionary)
			ifTrue: [ key -> (self fixFormKeysIn: (aContentPack at: key)) ] "Recurse."
			ifFalse: [ (key , '.png') -> (aContentPack at: key) ] "Fix leaf."
	].

	^ associations as: ContentPack
			
]

{ #category : 'accessing' }
ContentPack >> from: key get: anArray [

"
	Theme content from: #Theme get: #('16x16' 'actions' 'view-refresh.png' )
	Answer nil when the object isn't found.
		--cbr	"
		
	| object | object := self at: key ifAbsent: [ ^ nil ].
	
	anArray do: [ :i | object := object at: i ifAbsent: [ ^ nil ]].
	
	^ object
]

{ #category : 'testing' }
ContentPack >> is: aSymbol [
	^aSymbol == #ContentPack or: [ super is: aSymbol ]
]

{ #category : 'importing' }
ContentPack >> loadContentFrom: aDirectoryEntry [
	"Load content in us, from files.
	Start with an empty instance!"

	(self supportedFilesIn: aDirectoryEntry) do: [ :filename |			
		self flag: #todo. "Add hook for other media types here. Also consider renaming this method. --cbr"
		self at: filename name 
			put: (Form fromFileEntry: filename)
	].

	aDirectoryEntry directoryNames do: [ :i |
		self at: i put: (ContentPack new loadContentFrom: aDirectoryEntry / i)
	]
]

{ #category : 'merge' }
ContentPack >> merge: aDictionaryOrContentPack [
	"Merge aDictionaryOrContentPack into us"

	aDictionaryOrContentPack keysAndValuesDo: [ :key :value |
		(value is: #ContentPack)
			ifFalse: [
				self at: key put: value ]
			ifTrue: [
				(self at: key ifAbsentPut: [ContentPack new])
					merge: value ]]
]

{ #category : 'private' }
ContentPack >> supportedFilesIn: aDirectoryEntry [

	| fileTypes supportedFiles |
		fileTypes := (self class mapping as: Dictionary) values.
		supportedFiles := Set new.
	
	fileTypes do: [ :type | 
		supportedFiles := supportedFiles
			union: (aDirectoryEntry files select: [ :entry |
				'*.' , type match: entry name ])
	].

	^ supportedFiles
]
