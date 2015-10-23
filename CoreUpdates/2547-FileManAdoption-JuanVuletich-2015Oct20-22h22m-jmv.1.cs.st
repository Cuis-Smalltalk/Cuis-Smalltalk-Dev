'From Cuis 4.2 of 25 July 2013 [latest update: #2546] on 20 October 2015 at 10:38:11.94597 pm'!

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/20/2015 22:38'!
checkName: aFileName fixErrors: fixing
	"Check if the file name contains any invalid characters"
	| badChars hasBadChars |
	badChars _ #( $: $< $> $| $/ $\ $? $* $") asSet.
	hasBadChars _ aFileName includesAnyOf: badChars.
	(hasBadChars and:[fixing not]) ifTrue: [^self error:'Invalid file name'].
	hasBadChars ifFalse:[^ aFileName].
	^ aFileName collect: [ :char |
			(badChars includes: char) 
				ifTrue:[$#] 
				ifFalse:[char]]! !


!String methodsFor: 'converting' stamp: 'jmv 10/20/2015 22:32'!
asFileName
	"Answer a String made up from the receiver that is an acceptable file 
	name."

	^FmFileIOAccessor default checkName: self fixErrors: true! !

!String methodsFor: 'arithmetic' stamp: 'jmv 10/20/2015 22:29'!
/ arg
	"If working with file paths, just use $/
	Or better yet, use FmDirectoryEntry protocol"

	self shouldNotImplement! !


!ContentPack methodsFor: 'exporting' stamp: 'jmv 10/20/2015 22:27'!
export

	"Answer true on success."

	"self break."

	self exportDirectory exists
		ifTrue: [ Utilities inform: 
			'Before you can export, you must move, rename or delete this directory: ' ,
			self exportDirectory pathName.
			
			^ false.
		].
	
	self exportInto: self exportDirectory.
	
	^ true! !

!ContentPack methodsFor: 'exporting' stamp: 'jmv 10/20/2015 22:27'!
exportInto: aDirectory

	aDirectory assureExistence.

	self associations do: [ :assoc |
		| klass thisDirectory fullPath | 
			klass _ assoc value class.
			thisDirectory _ aDirectory directoryNamed: assoc key.
			fullPath _  aDirectory /assoc key.

		self flag: #note. "Add support for new file export type mappings here. --cbr"
		klass = ContentPack
			ifTrue: [ assoc value exportInto: thisDirectory ].
		
		klass = ColorForm
			ifTrue: [ assoc value writeBMPfileNamed: fullPath ].
			
		klass = Form
			ifTrue: [ assoc value writeBMPfileNamed: fullPath ]
	]! !

!ContentPack methodsFor: 'private' stamp: 'jmv 10/20/2015 22:28'!
exportDirectory
	
	^ (FmDirectoryEntry smalltalkImageDirectory directoryNamed: self class defaultContentDirectory)
		directoryNamed: 'Exported'! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 10/20/2015 22:35'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount newVersionString oldChangesName newChangesName newSourcesName |
	newVersionString _ FillInTheBlankMorph request: 'Please name the new sources file' initialAnswer: SourceFileVersionString.
	newVersionString ifNil: [^ self].
	newVersionString = SourceFileVersionString ifTrue: [
		^ self error: 'The new source file must not be the same as the old.'].
	SourceFileVersionString _ newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName _ self defaultSourcesName.
	newSourcesName asFileEntry writeStream: [ :f |
		f timeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :bar |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class |
					bar value: (classCount _ classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChangesName _ self currentChangesName.
	self closeSourceFiles.
	oldChangesName ifNotNil: [
		FmDirectoryEntry smalltalkImageDirectory / oldChangesName rename: oldChangesName, '.old' ].
	newChangesName _ self defaultChangesName.
	newChangesName asFileEntry writeStream: [ :stream |
		stream timeStamp ].
	LastQuitLogPosition _ 0.

	self openSourceFiles.
	self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/20/2015 22:34'!
defaultSourcesName
	"Answer the default full path to the sources file corresponding to the image file name."
	"
	Smalltalk defaultSourcesName
	"
	^ self imagePath, '/', SourceFileVersionString, '.sources'! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/20/2015 22:33'!
imagePath
	"Answer the path for the directory containing the image file."
	"
	Smalltalk imagePath
	"
	| pathName |
	pathName _ self imageName.
	^ pathName copyFrom: 1 to: pathName indexOfLastPathSeparator-1! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 10/20/2015 22:36'!
allUnusedClassesWithout: classesAndMessagesPair
	"Enumerates all classes in the system and returns a list of those that are apparently unused. A class is considered in use if it (a) has subclasses or (b) is referred to by some method or (c) has its name in use as a literal (but not in the same class) or: (d) some instance is a global."
	"
	Smalltalk unusedClasses
	"

	| unused cl |
	Smalltalk garbageCollect.
	unused _ Smalltalk classNames asIdentitySet
				copyWithoutAll: (self allGlobalRefsWithout: classesAndMessagesPair).
				
	Smalltalk do: [ :global |
		unused remove: global class name ifAbsent: nil].
	
	^ unused reject: [ :cName |
		cl _ Smalltalk at: cName.
		cl subclasses notEmpty
			"or: [ cl someInstance notNil ]"
			"or: [cl inheritsFrom: FileDirectory]"]! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 10/20/2015 22:22'!
write: aForm onFileNamed: filename

	(self subclassFor: (FmFileIOAccessor default extensionFor: filename))
		ifNotNil: [ :cls |
			cls putForm: aForm onFileNamed: filename ]
		ifNil: [
			self write: aForm onFileNamed: filename, '.bmp' ]! !


!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 10/20/2015 22:26'!
hasContents
	"Return whether this directory has subfolders. The value is cached to 
	avoid a performance penalty.	Also for performance reasons, the code 
	below will just assume that the directory does indeed have contents in a 
	few of cases:  
	1. If the item is not a FileDirectory (thus avoiding the cost 
	of refreshing directories that are not local)  (jmv: REMOVED)
	2. If it's the root directory of a given volume 
	3. If there is an error computing the FileDirectory's contents (jmvL: REMOVED)
	"
	hasContents
		ifNil: [
			hasContents := true. "default"
			item isRoot
				ifFalse: [hasContents := self contents notEmpty ]].
	^ hasContents! !


!FmFileIOAccessor reorganize!
('actions' asAbsolutePathName: copy:to: createDirectory: deleteDirectory: deleteFile: deleteFile:ifAbsent: fileOrDirectoryExists:in: rename:to: renameDirectory:to:)
('private' basicDirectoryExists: containingDirectoryPathOf: entriesIn: try:forFileNamed:)
('utilities' baseNameAndExtensionFor:do: baseNameFor: checkName:fixErrors: copyFile:toFile: directoryNamesIn: entryNamesIn: extensionFor: fileNamesIn: splitNameVersionExtensionFor:)
('accessing' concreteStreamClass currentDriveName drives pathNameDelimiter slash)
('file stream creation' privateFileNamed: privateForceNewFile: privateNewFile: privateReadOnlyFile:)
('testing' isCaseSensitive isDriveSupported onMac onUnix onWindows)
('primitives' primCreateDirectory: primDeleteDirectory: primDeleteFileNamed: primLookupEntryIn:index: primPathNameDelimiter primRename:to:)
!

