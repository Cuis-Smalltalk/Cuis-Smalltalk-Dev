'From Cuis 4.2 of 25 July 2013 [latest update: #2530] on 16 October 2015 at 2:56:12.349429 pm'!

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/16/2015 14:28'!
baseNameAndExtensionFor: pathName do: aBlock
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis."
	"Return the given file name without its extension, if any. We have to remember that many (most?) OSs allow extension separators within directory names and so the leaf filename needs to be extracted, trimmed and rejoined. Yuck"
	"The test is 
		FmFileIOAccessor default baseNameFor: ((FmDirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim.blam') pathName)
		should end 'foo.bar/blim' (or as appropriate for your platform AND
		
		FmFileIOAccessor default baseNameFor: ((FmDirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim') pathName)
		should be the same and NOT  'foo'
		
		Oh, and
		FmFileIOAccessor default baseNameFor: 'foo.bar'
		should be 'foo' not '/foo' "

	| extension |
	extension _ self extensionFor: pathName.
	extension isEmpty ifTrue: [
		^ aBlock value: pathName value: '' ].
	^ aBlock value: (pathName copyFrom: 1 to: pathName size - extension size - 1) value: extension! !


!String methodsFor: '*fileman-core-accessing' stamp: 'jmv 10/16/2015 14:21'!
indexOfLastPathSeparator
	"
	'pp.txt' indexOfLastPathSeparator
	'/pp.txt' indexOfLastPathSeparator
	'a/pp.txt' indexOfLastPathSeparator
	'b/a/pp.txt' indexOfLastPathSeparator
	'/b/a/pp.txt' indexOfLastPathSeparator
	"
	^self findLast: [ :char | char isPathSeparator ]! !


!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/16/2015 14:41'!
defaultChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultChangesName
	"
	^(FmFileIOAccessor default baseNameFor: self imageName), '.changes'! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/16/2015 14:47'!
fullNameForChangesNamed: aName
	"
	Smalltalk fullNameForChangesNamed: 'newChanges'
	"
	| newName |
	newName _ FmFileIOAccessor default baseNameFor: ((FmDirectoryEntry smalltalkImageDirectory / aName) pathName).
	^newName , '.changes'! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/16/2015 14:48'!
fullNameForImageNamed: aName
	"
	Smalltalk fullNameForImageNamed: 'newImage'
	"
	| newName |
	newName _ FmFileIOAccessor default baseNameFor: ((FmDirectoryEntry smalltalkImageDirectory / aName) pathName).
	^newName , '.image'! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 10/16/2015 14:47'!
saveAsNewVersion
	"Save the image/changes using the next available version number."
	"
	Smalltalk saveAsNewVersion
	"
	| fileName newName changesName systemVersion |
	self okayToSave ifFalse: [ ^ self ].
	systemVersion _ SystemVersion current.
	fileName _ String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: systemVersion versionMajor;
			nextPut: $.;
			print: systemVersion versionMinor;
			nextPut: $-;
			print: systemVersion highestUpdate ].
	newName _ fileName, '.image'.
	(FileDirectory smalltalkImageDirectory includesKey: newName) ifTrue: [
		newName _ FileDirectory smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	(FileDirectory smalltalkImageDirectory includesKey: changesName) ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', newName, '
curiously already present, even though there is
no corresponding .image file.   Please remedy
manually and then repeat your request.' ].
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAs: newName andQuit: false clearAllClassState: true! !


!FmFileEntry methodsFor: 'accessing-file name' stamp: 'jmv 10/16/2015 14:44'!
nameWithoutExtension
	"
	'writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings' asFileEntry nameWithoutExtension
	"
	| nm index |
	nm := self name.
	index := nm lastIndexOf: $..
	index = 0 ifTrue: [^nm].
	nm first = $. ifTrue: [^nm].
	
	^self name copyFrom: 1 to: index - 1! !


!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/16/2015 14:29'!
baseNameFor: pathName
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis."
	"Return the given file name without its extension, if any. We have to remember that many (most?) OSs allow extension separators within directory names and so the leaf filename needs to be extracted, trimmed and rejoined. Yuck"
	"The test is 
		FmFileIOAccessor default baseNameFor: ((FmDirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim.blam') pathName)
		should end 'foo.bar/blim' (or as appropriate for your platform AND
		
		FmFileIOAccessor default baseNameFor: ((FmDirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim') pathName)
		should be the same and NOT  'foo'
		
		Oh, and
		FmFileIOAccessor default baseNameFor: 'foo.bar'
		should be 'foo' not '/foo' "

	self baseNameAndExtensionFor: pathName do: [ :baseName :extension |
		^baseName ]! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/16/2015 14:39'!
extensionFor: pathName 
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis.
	
	FmFileIOAccessor default extensionFor: 'writings.txt'
	FmFileIOAccessor default extensionFor: 'folder.ext/file'
	FmFileIOAccessor default extensionFor: 'optionalstuff.pck.st'
	FmFileIOAccessor default extensionFor: 'code.cs.st'
	FmFileIOAccessor default extensionFor: 'code.cs'
	"
	| index |
	{ '.cs.st' . '.pck.st' } do: [ :specialExtension |
		(pathName endsWith: specialExtension)
			ifTrue: [ ^specialExtension copyFrom: 2 to: specialExtension size ]].
	index _ pathName
				findLast: [ :c | c = $.].
	^ (index = 0 or: [ pathName indexOfLastPathSeparator > index ])
		ifTrue: ['']
		ifFalse: [pathName copyFrom: index + 1 to: pathName size]! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/16/2015 14:31'!
splitNameVersionExtensionFor: fileName
	" answer an array with the root name, version # and extension.
	See comment in nextSequentialNameFor: for more details"

	| baseName version i j |

	self baseNameAndExtensionFor: fileName do: [ :b :extension |
		baseName _ b.
		i := j := baseName findLast: [:c | c isDigit not].
		i = 0
			ifTrue: [version := 0]
			ifFalse: [
				(baseName at: i) = $.
					ifTrue: [
						version := (baseName copyFrom: i+1 to: baseName size) asNumber.
						j := j - 1]
					ifFalse: [version := 0].
				baseName := baseName copyFrom: 1 to: j ].
		^ Array with: baseName with: version with: extension ]! !

!methodMoveToSomePackage: FmFileEntry #shortName!
FmFileEntry removeSelectorIfInBaseSystem: #shortName!
!methodRemoval: FileDirectory class #changeSuffix!
FileDirectory class removeSelector: #changeSuffix!
!methodRemoval: FileDirectory class #imageSuffix!
FileDirectory class removeSelector: #imageSuffix!
!methodRemoval: SystemDictionary #saveAsEmbeddedImage!
SystemDictionary removeSelector: #saveAsEmbeddedImage!
!methodMoveToSomePackage: String #upToLastPathSeparator!
String removeSelectorIfInBaseSystem: #upToLastPathSeparator!
