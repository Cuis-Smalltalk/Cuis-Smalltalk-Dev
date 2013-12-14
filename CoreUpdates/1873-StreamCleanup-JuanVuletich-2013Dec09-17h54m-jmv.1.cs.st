'From Cuis 4.2 of 25 July 2013 [latest update: #1872] on 9 December 2013 at 5:56:08.578424 pm'!

!RWBinaryOrTextStream commentStamp: '<historical>' prior: 0!
Like FileStream in the sense that I can switch between binary and text, as a FileStream does, without recopying the whole collection. Convert to binary upon input and output. Always keep as text internally.!


!Stream methodsFor: 'accessing' stamp: 'jmv 12/9/2013 17:51'!
ascii
	"Compatibility. If possible, store and retrieve characters."
	self useCharacters! !


!DummyStream methodsFor: 'as yet unclassified' stamp: 'jmv 12/9/2013 17:55'!
useBytes
! !

!DummyStream methodsFor: 'as yet unclassified' stamp: 'jmv 12/9/2013 17:55'!
useCharacters
! !


!ReadWriteStream methodsFor: 'fileIn/Out' stamp: 'jmv 12/7/2013 18:17'!
fileOutObject: theObject
	"Write a file that has both the source code for the named class and an object as bits.  Any instance-specific object will get its class written automatically."

	| srefStream |
	self setFileTypeToObject.
		"Type and Creator not to be text, so can attach correctly to an email msg"
	self timeStamp.

	"Append the object's raw data"
	srefStream _ SmartRefStream on: self.
	srefStream nextPut: theObject.  "and all subobjects"
	srefStream close.		"also closes me"
! !


!FileStream methodsFor: 'file modes' stamp: 'jmv 12/9/2013 17:48'!
useBytes
	"Set this file to binary mode."

	self subclassResponsibility
! !

!FileStream methodsFor: 'file modes' stamp: 'jmv 12/9/2013 17:49'!
useCharacters
	"Set this file to ascii (text) mode."

	self subclassResponsibility
! !


!StandardFileStream methodsFor: 'properties-setting' stamp: 'jmv 12/9/2013 17:48'!
useBytes
	buffer1 := ByteArray new: 1.
	collection ifNotNil: [ collection := collection asByteArray ]! !

!StandardFileStream methodsFor: 'properties-setting' stamp: 'jmv 12/9/2013 17:49'!
useCharacters
	"opposite of binary"
	buffer1 := String new: 1.
	collection ifNotNil: [ collection := collection asString ]! !


!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'jmv 12/9/2013 17:48'!
useBytes
	isBinary _ true! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'jmv 12/9/2013 17:49'!
useCharacters
	isBinary _ false! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 12/7/2013 18:52'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse file |
	nameToUse _ self name.
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asCamelCase. 
	nameToUse _ Preferences changeSetVersionNumbers
				ifTrue: [ self defaultChangeSetDirectory nextNameFor:nameToUse coda: '-', Utilities authorInitials extension: 'cs.st' ]
				ifFalse: [ (nameToUse , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'cs.st') asFileName ].
	file _ self defaultChangeSetDirectory newFileNamed: nameToUse.
	[
		file timeStamp.
		self fileOutPreambleOn: file.
		self fileOutOn: file.
		self fileOutPostscriptOn: file]
			ensure: [ file close ].
	
	self hasUnsavedChanges: false.
	Preferences checkForSlips
		ifFalse: [^ self].
	slips := self checkForSlips.
	(slips size > 0
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [Smalltalk browseMessageList: slips name: 'Possible slips in ' , name]! !


!WordArray class methodsFor: 'as yet unclassified' stamp: 'jmv 12/6/2013 22:34'!
bobsTest
	| wa s1 s2 wa2 answer rawData |
"
WordArray bobsTest
"
	answer _ OrderedCollection new.
	wa _ WordArray with: 16r01020304 with: 16r05060708.
	{false. true} do: [ :pad |
		0 to: 3 do: [ :skip |
			s1 _ ReadWriteStream on: ByteArray new.

			s1 next: skip put: 0.		"start at varying positions"
			wa writeOn: s1.
			pad ifTrue: [s1 next: 4-skip put: 0].	"force length to be multiple of 4"

			rawData _ s1 contents.
			s2 _ ReadWriteStream with: rawData.
			s2 reset.
			s2 skip: skip.			"get to beginning of object"
			wa2 _ WordArray newFromStream: s2.
			answer add: {
				rawData size. 
				skip. 
				wa2 = wa. 
				wa2 asArray collect: [ :each | each radix: 16]
			}
		].
	].
	^answer explore! !


!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'jmv 12/6/2013 22:40'!
mimeDecodeToBytes: aStream 
	"Return a RWBinaryOrTextStream of the original ByteArray.  aStream has only 65 innocuous character values.  aStream is not binary.  (See class comment). 4 bytes in aStream goes to 3 bytes in output."

	| me |
	aStream position: 0.
	me _ self new mimeStream: aStream.
	me dataStream: (ReadWriteStream on: (ByteArray new: aStream size * 3 // 4)).
	me mimeDecodeToByteArray.
	me dataStream position: 0.
	^ me dataStream! !


!Stream methodsFor: 'accessing' stamp: 'jmv 12/9/2013 17:52'!
binary
	"Compatibility. If possible, store and retrieve bytes."
	self useBytes! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 12/7/2013 18:17'!
saveWorldInFile
	"Save the world's submorphs, model, and stepList in a file.  "

	| fileName fileStream |
	fileName _ FillInTheBlankMorph request: 'File name for this morph?'.
	fileName isEmpty ifTrue: [^ self].  "abort"

	"Save only model, stepList, submorphs in this world"
	myWorld submorphsDo: [:m |
		m allMorphsDo: [:subM | subM prepareToBeSaved]].	"Amen"

	fileStream _ FileStream newFileNamed: fileName, '.morph'.
	fileStream fileOutObject: myWorld.
! !

!methodRemoval: RWBinaryOrTextStream #asBinaryOrTextStream!
RWBinaryOrTextStream removeSelector: #asBinaryOrTextStream!
!methodRemoval: RWBinaryOrTextStream #ascii!
RWBinaryOrTextStream removeSelector: #ascii!
!methodRemoval: RWBinaryOrTextStream #binary!
RWBinaryOrTextStream removeSelector: #binary!
!methodRemoval: RWBinaryOrTextStream #contentsOfEntireFile!
RWBinaryOrTextStream removeSelector: #contentsOfEntireFile!
!methodRemoval: StandardFileStream #ascii!
StandardFileStream removeSelector: #ascii!
!methodRemoval: StandardFileStream #binary!
StandardFileStream removeSelector: #binary!
!methodRemoval: FileStream #asBinaryOrTextStream!
FileStream removeSelector: #asBinaryOrTextStream!
!methodRemoval: FileStream #ascii!
FileStream removeSelector: #ascii!
!methodRemoval: FileStream #binary!
FileStream removeSelector: #binary!
!methodRemoval: FileStream #fileInObjectAndCode!
FileStream removeSelector: #fileInObjectAndCode!
!methodRemoval: ReadWriteStream #fileInObjectAndCode!
ReadWriteStream removeSelector: #fileInObjectAndCode!
!methodRemoval: ReadWriteStream #fileOutChangeSet:andObject:!
ReadWriteStream removeSelector: #fileOutChangeSet:andObject:!
!methodRemoval: ReadWriteStream #fileOutClass:andObject:!
ReadWriteStream removeSelector: #fileOutClass:andObject:!
!methodRemoval: ReadWriteStream #fileOutClass:andObject:blocking:!
ReadWriteStream removeSelector: #fileOutClass:andObject:blocking:!
!methodRemoval: ReadStream #ascii!
ReadStream removeSelector: #ascii!
!methodRemoval: ReadStream #binary!
ReadStream removeSelector: #binary!
!methodRemoval: PositionableStream #asBinaryOrTextStream!
PositionableStream removeSelector: #asBinaryOrTextStream!
!methodRemoval: DummyStream #ascii!
DummyStream removeSelector: #ascii!
!methodRemoval: DummyStream #binary!
DummyStream removeSelector: #binary!
!methodRemoval: SmartRefStream class #objectFromStreamedRepresentation:!
SmartRefStream class removeSelector: #objectFromStreamedRepresentation:!
!methodRemoval: SmartRefStream class #streamedRepresentationOf:!
SmartRefStream class removeSelector: #streamedRepresentationOf:!
!methodRemoval: SmartRefStream #nextAndClose!
SmartRefStream removeSelector: #nextAndClose!
!methodRemoval: DataStream #nextAndClose!
DataStream removeSelector: #nextAndClose!
!methodRemoval: DataStream #readBoolean!
DataStream removeSelector: #readBoolean!
!methodRemoval: DataStream #readFloatString!
DataStream removeSelector: #readFloatString!
!methodRemoval: DataStream #writeBoolean:!
DataStream removeSelector: #writeBoolean:!
!methodRemoval: DataStream #writeFloatString:!
DataStream removeSelector: #writeFloatString:!
!methodRemoval: Preferences class #conversionMethodsAtFileOut!
Preferences class removeSelector: #conversionMethodsAtFileOut!
!methodRemoval: ChangeSet #checkForConversionMethods!
ChangeSet removeSelector: #checkForConversionMethods!
