'From Cuis 5.0 of 7 November 2016 [latest update: #3463] on 28 September 2018 at 12:16:58 pm'!

!ContentPack commentStamp: 'jmv 9/28/2018 12:16:02' prior: 0!
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
Alternative description (need to merge both!!)

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
ContentPack removeCategory: ContentPack generatedMethodsCategory!


!ContentPack methodsFor: 'importing' stamp: 'jmv 9/28/2018 11:50:48'!
decodeContent
	"Load content in us.
	Start with an empty instance!!"

	self decodeContentFrom: self contentMap! !

!ContentPack methodsFor: 'testing' stamp: 'jmv 9/28/2018 12:02:29'!
is: aSymbol
	^aSymbol == #ContentPack or: [ super is: aSymbol ]! !

!ContentPack methodsFor: 'merge' stamp: 'jmv 9/28/2018 12:03:37'!
merge: aDictionaryOrContentPack
	"Merge aDictionaryOrContentPack into us"

	aDictionaryOrContentPack keysAndValuesDo: [ :key :value |
		(value is: #ContentPack)
			ifFalse: [
				self at: key put: value ]
			ifTrue: [
				(self at: key ifAbsentPut: [ContentPack new])
					merge: value ]]! !


!ContentPack class methodsFor: 'instance creation' stamp: 'jmv 9/28/2018 11:51:51'!
decode
	
	^ self new decodeContent! !

!ContentPack class methodsFor: 'code pack' stamp: 'jmv 9/28/2018 10:47:25'!
generateCode: aDictionary

	| contentMap |

	self resetImporter.
	
	contentMap _ self encodeContentFrom: aDictionary.
	
	self compilePayloadWith: contentMap.
	
	self resetImporter.! !

!ContentPack class methodsFor: 'configuration' stamp: 'jmv 9/28/2018 12:14:18'!
generatedMethodsCategory
	^ 'generated code'! !

!ContentPack class methodsFor: 'configuration' stamp: 'jmv 9/28/2018 10:37:22'!
importDirectory
	
	^ DirectoryEntry smalltalkImageDirectory / self defaultContentDirectory / 'Import'! !


!ContentPack methodsFor: 'importing' stamp: 'jmv 9/28/2018 11:56:08'!
decodeContentFrom: associationList
	"Load content in us, frin geberated code.
	Start with an empty instance!!"

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
	]! !

!ContentPack methodsFor: 'importing' stamp: 'jmv 9/28/2018 11:55:59'!
loadContentFrom: aDirectoryEntry
	"Load content in us, from files.
	Start with an empty instance!!"

	(self supportedFilesIn: aDirectoryEntry) do: [ :filename |			
		self flag: #todo. "Add hook for other media types here. Also consider renaming this method. --cbr"
		self at: filename name 
			put: (Form fromFileEntry: filename)
	].

	aDirectoryEntry directoryNames do: [ :i |
		self at: i put: (ContentPack new loadContentFrom: aDirectoryEntry / i)
	]! !


!ContentPack class methodsFor: 'instance creation' stamp: 'jmv 9/28/2018 10:37:34'!
import
	"
	Feature require: 'Graphics-Files-Additional'.
	Theme content export.
	ContentPack import.
	Theme bootstrap.
	"
	
	^ self new loadContentFrom: self importDirectory! !

!ContentPack class methodsFor: 'code pack' stamp: 'jmv 9/28/2018 12:14:28'!
compilePayloadWith: contentMap

	| category |
	category _ self generatedMethodsCategory.
	self
		compile: 'contentMap' , String newLineString , '	^ ' , contentMap asString
		classified: category.

	self
		compile: 'objectCount' , String newLineString , '	^ ' , payload size asString
		classified: category.

	payload withIndexDo: [ :blob :index | | selector |
		selector _ 'object', index asString.
		self
			compile: selector, String newLineString, '	^ ', blob surroundedBySingleQuotes
			classified: category ]! !

!methodRemoval: ContentPack class #withDictionary:!
ContentPack class removeSelector: #withDictionary:!
!methodRemoval: ContentPack #import:!
ContentPack removeSelector: #import:!

!ContentPack reorganize!
('importing' decodeContent decodeContentFrom: loadContentFrom:)
('exporting' export exportInto:)
('private' supportedFilesIn:)
('accessing' from:get:)
('converting' asContentPack: asDictionary fixFormKeysIn:)
('testing' is:)
('merge' merge:)
!

