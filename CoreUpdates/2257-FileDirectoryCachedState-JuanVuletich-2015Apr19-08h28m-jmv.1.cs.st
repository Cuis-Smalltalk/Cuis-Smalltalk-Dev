'From Cuis 4.2 of 25 July 2013 [latest update: #2257] on 19 April 2015 at 8:46:09.069912 am'!

!FileDirectory class methodsFor: 'cacher state access' stamp: 'jmv 4/19/2015 08:43'!
releaseClassCachedState

	DirectoryClass _ DefaultDirectory _ nil! !


!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 4/19/2015 08:34'!
directoryEntryFor: filenameOrPath
	"Answer the directory entry for the given file or path. Sorta like a poor man's stat()."
	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:[:filePath :name |
		fName _ name.
		filePath isEmpty
			ifTrue: [dir _ self]
			ifFalse: [dir _ FileDirectory on: filePath]].
	self isCaseSensitive 
		ifTrue:[^dir entries detect:[:entry| entry name = fName] ifNone: nil]
		ifFalse:[^dir entries detect:[:entry| entry name sameAs: fName] ifNone: nil]! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 4/19/2015 08:34'!
directoryExists: filenameOrPath
	"Answer true if a directory of the given name exists. The given name may be either a full path name or a local directory within this directory."
	"FileDirectory default directoryExists: FileDirectory default pathName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:
		[:filePath :name |
			fName _ name.
			filePath isEmpty
				ifTrue: [dir _ self]
				ifFalse: [dir _ self directoryNamed: filePath]].

	^dir exists and: [
		self isCaseSensitive 
			ifTrue:[dir directoryNames includes: fName]
			ifFalse:[dir directoryNames anySatisfy: [:name| name sameAs: fName]]].
! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 4/19/2015 08:34'!
fileExists: filenameOrPath
	"Answer true if a file of the given name exists. The given name may be either a full path name or a local file within this directory."
	"FileDirectory default fileExists: Smalltalk sourcesName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:
		[:filePath :name |
			fName _ name.
			filePath isEmpty
				ifTrue: [dir _ self]
				ifFalse: [dir _ FileDirectory on: filePath]].
	self isCaseSensitive 
		ifTrue:[^dir fileNames includes: fName]
		ifFalse:[^dir fileNames anySatisfy: [:name| name sameAs: fName]].	! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 4/19/2015 08:34'!
fileOrDirectoryExists: filenameOrPath
	"Answer true if either a file or a directory file of the given name exists. The given name may be either a full path name or a local name within this directory."
	"FileDirectory default fileOrDirectoryExists: Smalltalk sourcesName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:
		[:filePath :name |
			fName _ name.
			filePath isEmpty
				ifTrue: [dir _ self]
				ifFalse: [dir _ FileDirectory on: filePath]].

	^ (dir includesKey: fName) or: [ fName = '' and:[ dir entries size > 1]]! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 4/19/2015 08:35'!
fullNameFor: fileName
	"Return a corrected, fully-qualified name for the given file name. If the given name is already a full path (i.e., it contains a delimiter character), assume it is already a fully-qualified name. Otherwise, prefix it with the path to this directory. In either case, correct the local part of the file name."
	"Details: Note that path relative to a directory, such as '../../foo' are disallowed by this algorithm.  Also note that this method is tolerent of a nil argument -- is simply returns nil in this case."

	| correctedLocalName prefix |
	fileName ifNil: [^ nil].
	self class activeDirectoryClass splitName: fileName to:
		[:filePath :localName |
			correctedLocalName _ localName isEmpty 
				ifFalse: [self checkName: localName fixErrors: true]
				ifTrue: [localName].
			prefix _ self fullPathFor: filePath].
	prefix isEmpty
		ifTrue: [^correctedLocalName].
	prefix last = self pathNameDelimiter
		ifTrue:[^ prefix, correctedLocalName]
		ifFalse:[^ prefix, self slash, correctedLocalName]! !


!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 4/19/2015 08:41'!
default
	"Answer the default directory."

	DefaultDirectory ifNil: [
		DefaultDirectory _ self on: Smalltalk imagePath ].
	^ DefaultDirectory
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 4/19/2015 08:37'!
on: pathString
	"Return a new file directory for the given path, of the appropriate FileDirectory subclass for the current OS platform."

	| pathName |
	"If path ends with a delimiter (: or /) then remove it"
	((pathName _ pathString) endsWith: self activeDirectoryClass pathNameDelimiter asString) ifTrue: [
		pathName _ pathName copyFrom: 1 to: pathName size - 1].
	^ self activeDirectoryClass new setPathName: pathName
! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:41'!
checkName: fileName fixErrors: flag
	"Check a string fileName for validity as a file name on the current default file system. Answer the original file name if it is valid. If the name is not valid (e.g., it is too long or contains illegal characters) and fixing is false, raise an error. If fixing is true, fix the name (usually by truncating and/or tranforming characters), and answer the corrected name. The default behavior is to truncate the name to 31 chars. Subclasses can do any kind of checking and correction appropriate to the underlying platform."

	^ self default
		checkName: fileName
		fixErrors: flag
! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:35'!
dirPathFor: fullName 
	"Return the directory part the given name."
	self activeDirectoryClass
		splitName: fullName
		to: [:dirPath :localName | ^ dirPath]! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:35'!
extensionFor: pathName
	"Return the extension of given file name, if any.
	FileDirectory extensionFor: 'writings.txt'
	FileDirectory extensionFor: 'optionalstuff.pck.st'
	FileDirectory extensionFor: 'optionalstuff.pck'
	FileDirectory extensionFor: 'code.cs.st'
	FileDirectory extensionFor: 'code.cs'
	"

	| delim i |
	self splitName: pathName to: [ :path :filename |
		delim _ self activeDirectoryClass extensionDelimiter.
		{ 'cs.st' . 'pck.st' } do: [ :specialExtension |
			(filename endsWith: delim asString, specialExtension)
				ifTrue: [ ^specialExtension ]].
		i _ filename findLast: [ :c | c = delim ].
		^ i = 0
			ifTrue: [ '']
			ifFalse: [ filename copyFrom: i + 1 to: filename size ]]! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:41'!
isLegalFileName: fullName
	"Return true if the given string is a legal file name."

	^ self default isLegalFileName: (self localNameFor: fullName)
! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:35'!
localNameFor: fullName 
	"Return the local part the given name."
	self activeDirectoryClass
		splitName: fullName
		to: [:dirPath :localName | ^ localName]! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:41'!
startUp

	Smalltalk openSourceFiles! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jmv 4/19/2015 08:38'!
pathNameDelimiter
	"return the active directory class's directory seperator character"
	"Warning: endless recursion if no specific #activeDirectoryClass found..."
	^ self activeDirectoryClass pathNameDelimiter! !

!FileDirectory class methodsFor: 'private' stamp: 'jmv 4/19/2015 08:34'!
activeDirectoryClass
	"Return the concrete FileDirectory subclass for the platform on which we are currently running."

	DirectoryClass ifNotNil: [ ^DirectoryClass ].
	
	FileDirectory allSubclasses do: [ :cls |
		cls isActiveDirectoryClass ifTrue: [
			DirectoryClass _ cls.
			^ DirectoryClass ]].

	"no responding subclass; use FileDirectory"
	DirectoryClass _ FileDirectory.
	^DirectoryClass! !


!MacFileDirectory methodsFor: 'as yet unclassified' stamp: 'jmv 4/19/2015 08:39'!
fullNameFor: fileName
	"Return a corrected, fully-qualified name for the given file name. If the given name is already a full path (i.e., it contains a delimiter character), assume it is already a fully-qualified name. Otherwise, prefix it with the path to this directory. In either case, correct the local part of the file name."
	"Details: Note that path relative to a directory, such as '../../foo' are disallowed by this algorithm.  Also note that this method is tolerent of a nil argument -- is simply returns nil in this case."
	"Fix by hmm: for a file in the root directory of a volume on MacOS, the filePath (name of the directory) is not  recognizable as an absolute path anymore (it has no delimiters). Therefore, the original fileName is tested for absoluteness, and the filePath is only made absolute if the original fileName was not absolute"

	| correctedLocalName prefix |
	fileName isEmptyOrNil ifTrue: [^ fileName].
	self class activeDirectoryClass splitName: fileName to:
		[:filePath :localName |
			correctedLocalName _ localName isEmpty 
				ifFalse: [self checkName: localName fixErrors: true]
				ifTrue: [localName].
			prefix _ (self class  isAbsolute: fileName)
						ifTrue: [filePath]
						ifFalse: [self fullPathFor: filePath]].
	prefix isEmpty
		ifTrue: [^correctedLocalName].
	prefix last = self pathNameDelimiter
		ifTrue:[^ prefix, correctedLocalName]
		ifFalse:[^ prefix, self slash, correctedLocalName]! !

!methodRemoval: FileDirectory class #setDefaultDirectory:!
FileDirectory class removeSelector: #setDefaultDirectory:!
!methodRemoval: FileDirectory class #setDefaultDirectoryClass!
FileDirectory class removeSelector: #setDefaultDirectoryClass!
!methodRemoval: FileDirectory class #setDefaultDirectoryFrom:!
FileDirectory class removeSelector: #setDefaultDirectoryFrom:!

!FileDirectory class reorganize!
('instance creation' default forFileName: on: root)
('name utilities' baseNameFor: changeSuffix checkName:fixErrors: dirPathFor: directoryEntryFor: extensionFor: fileName:extension: imageSuffix isLegalFileName: localNameFor: sourceSuffix splitName:to: startUp)
('create/delete file' deleteFilePath: lookInUsualPlaces:)
('system start up' shutDown)
('platform specific' dot extensionDelimiter isCaseSensitive makeAbsolute: makeRelative: maxFileNameLength pathNameDelimiter slash)
('private' activeDirectoryClass isActiveDirectoryClass primPathNameDelimiter)
('cacher state access' releaseClassCachedState)
!

