'From Cuis 4.2 of 25 July 2013 [latest update: #2494] on 15 September 2015 at 2:47:40.54378 pm'!
!classDefinition: #FmDirectoryEntry category: #'FileMan-Core'!
FmFileEntry subclass: #FmDirectoryEntry
	instanceVariableNames: 'children '
	classVariableNames: 'CurrentDirectory VMDirectory ImageDirectory '
	poolDictionaries: ''
	category: 'FileMan-Core'!
!classDefinition: #FmFileIOAccessor category: #'FileMan-Core'!
Object subclass: #FmFileIOAccessor
	instanceVariableNames: 'defaultDirectoryPath slash drives '
	classVariableNames: 'Default '
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmFileEntry class methodsFor: 'cached state access' stamp: 'jmv 9/15/2015 13:05'!
releaseClassCachedState

	self allSubInstancesDo: [:each | each refresh]! !


!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 9/15/2015 13:41'!
currentDirectory
	"Answer the current directory.

	In Unix it is the current directory in the OS shell that started us.
	In Windows the same happens if the image file is in a subree of the Windows current directory.

	But it defaults to the directory in wich this Smalltalk image was started (or last saved) if this fails
	(this usually happens, for example, if the image is dropped on the VM in a Windows explorer).
	See #getCurrentWorkingDirectory

	FmDirectoryEntry currentDirectory
	"

	CurrentDirectory ifNil: [
		CurrentDirectory _ self pathName: (Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]) ].
	^ CurrentDirectory! !

!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 9/15/2015 13:41'!
smalltalkImageDirectory
	"Answer the directory on which this Smalltalk image was started (or last saved)

	FmDirectoryEntry smalltalkImageDirectory
	"

	ImageDirectory ifNil: [
		ImageDirectory _ self pathName: Smalltalk imagePath ].
	^ ImageDirectory! !

!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 9/15/2015 13:42'!
vmDirectory
	"Answer the directory containing the VM that runs us.

	FmDirectoryEntry vmDirectory
	"

	VMDirectory ifNil: [
		VMDirectory _ self pathName: Smalltalk vmPath ].
	^ VMDirectory! !

!FmDirectoryEntry class methodsFor: 'cached state access' stamp: 'jmv 9/15/2015 13:39'!
releaseClassCachedState

	ImageDirectory _ nil.
	VMDirectory _ nil.
	CurrentDirectory _ nil! !


!FmFileIOAccessor class methodsFor: 'cached state access' stamp: 'jmv 9/15/2015 13:01'!
releaseClassCachedState
	Default _ nil! !


!String methodsFor: 'arithmetic' stamp: 'jmv 9/15/2015 14:03'!
/ arg
	"So we can write
	'folderName'/ 'fileName.ext'
	and it works on any platdorm
	
	Given that FileMan supports $/ as path delimiter regardless of the platform,
	and this method is not part of it, revert to commented implementation below 
	when FileDirectory is removed from Cuis.
	"
	^String streamContents: [ :strm |
		strm
			nextPutAll: self;
			nextPut: FileDirectory pathNameDelimiter;
			nextPutAll: arg ]
	"
	self shouldNotImplement.
	"! !


!SystemDictionary class methodsFor: 'initialization' stamp: 'jmv 9/15/2015 13:06'!
initialize
	"
	SystemDictionary initialize
	"
	
	"Hacer esto bien explicito al menos para las clases en la imagen base?
	Quisiera un mecanismo que sea igual de bueno para clases base que para clases en paquetes.
	Excepto para MUY pocas cosas quizas, como la 1ra parte de StartUpList... El resto deberia usar el mismo esquema
	este en la imagen base o en paquetes. Y seria quizas onda initialize.
	O sea, quizas al cargar un paquete deberia llamarse initialize y startup, no se.
	Como que initialize es cuando se crea la clase (o en cualquier momento que quieras reinicializar)
	y startUp se ejecuta TAMBIEN cuando levanta la imagen.
	Como initialize clashea con las instancias, quizas uniformar todo en startUp: y shutDown: / startUp y shutDown .... 
	
	Cargar todos los paquetes. estudiar implementors y senders de startUp: y shutDown: / startUp y shutDown
	ver senders de addToStartUpList: y addToShutDownList:"

	StartUpList _ OrderedCollection new.
	"These get processed from the top down..."
	#(
		#Delay
		#DisplayScreen
		#Cursor
		#InputSensor
		#ProcessorScheduler 	"Starts low space watcher and bkground."
		#FileDirectory 			"Enables file stack dump and opens sources."
		#ShortIntegerArray
		#ShortRunArray
		#PasteUpMorph
		#WeakArray
		#Taskbar)
			do: [ :clsName | 	
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToStartUpList: cls]].
			
	ShutDownList _ OrderedCollection new.
	"These get processed from the bottom up..."
	#(
		#Delay
		#DisplayScreen
		#InputSensor
		#PasteUpMorph
		#StrikeFont
		#FileDirectory
		#Symbol
		#ProcessBrowser
		#CPUWatcher)
			do: [ :clsName | 
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToShutDownList: cls]]! !


!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 9/15/2015 14:39'!
default 
	"Compatibility with other implementations of FileMan.
	For Cuis specific code, it is advisable to call #currentDirectory, #smalltalkImageDirectory or #vmDirectory
	FmDirectoryEntry default
	"
	^self currentDirectory! !


!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 9/15/2015 14:44'!
asAbsolutePathName: aString
	"Not complete, but in most cases it is OK"

	| tokens curDir childPath |

	tokens := aString asPathTokens.

	"Already an absolute path name? just normalize path separators"
	aString isAbsolutePathName ifTrue: [
		^String streamContents: [ :strm |
			aString first isPathSeparator ifTrue: [ strm nextPutAll: self slash ].
			tokens
				do: [ :each | strm nextPutAll: each ]
				separatedBy: [ strm nextPutAll: self slash ]]].

	aString = '.' ifTrue: [ ^ FmDirectoryEntry currentDirectory pathName ].
	aString = '..' ifTrue:  [^self containingDirectoryPathOf: FmDirectoryEntry currentDirectory pathName ].


	curDir := FmDirectoryEntry currentDirectory pathName.
	tokens reverseDo: [:each |
		each = '..' ifTrue: [curDir := self containingDirectoryPathOf: curDir]
	].

	tokens removeAllSuchThat: [:each | #('.' '..') includes: each ].
	childPath := WriteStream on: String new.
	tokens do: [:each | childPath nextPutAll: each]
					separatedBy: [childPath nextPutAll: self slash].
		
	^curDir, self slash, childPath contents! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 9/15/2015 14:02'!
extensionDelimiter
	"Return the character used to delimit filename extensions on this platform. Most platforms use the period (.) character.
	This method is mostly for FileMan's own use, and general usage is discouraged.
	Just use $. "

	^ $.! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 9/15/2015 14:00'!
pathNameDelimiter
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use $/ "
	^self primPathNameDelimiter! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 9/15/2015 13:59'!
slash
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use '/' "
	slash ifNil: [ slash _ self pathNameDelimiter asString ].
	^slash! !

!methodRemoval: FmFileIOAccessor class #defaultDirectoryPath!
FmFileIOAccessor class removeSelector: #defaultDirectoryPath!

!FmFileIOAccessor class reorganize!
('class initialization' initialize)
('instance creation' default)
('cached state access' releaseClassCachedState)
!

!methodRemoval: FmFileIOAccessor #defaultDirectoryPath!
FmFileIOAccessor removeSelector: #defaultDirectoryPath!
!methodRemoval: FmFileIOAccessor #initDefaultDirectoryPath!
FmFileIOAccessor removeSelector: #initDefaultDirectoryPath!
!methodRemoval: FmFileIOAccessor #initDefaultDirectoryPathWith:!
FmFileIOAccessor removeSelector: #initDefaultDirectoryPathWith:!
!classDefinition: #FmFileIOAccessor category: #'FileMan-Core'!
Object subclass: #FmFileIOAccessor
	instanceVariableNames: 'slash drives'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmFileIOAccessor reorganize!
('actions' asAbsolutePathName: copy:to: createDirectory: deleteDirectory: deleteFile: deleteFile:ifAbsent: directoryExists: directoryExists:in: fileExists: fileExists:in: fileOrDirectoryExists:in: rename:to: renameDirectory:to:)
('private' basicDirectoryExists: containingDirectoryPathOf: entriesIn:from: imageDirectoryPath lookupEntryIn:index: try:forFileNamed:)
('utilities' baseNameFor: copyFile:toFile: directoryNamesIn: encode:on: entryNamesIn: extensionFor: fileNamesIn: splitNameVersionExtensionFor:)
('accessing' concreteStreamClass currentDriveName drives extensionDelimiter pathNameDelimiter slash)
('file stream creation' fileNamed: forceNewFileNamed: newFileNamed: readOnlyFileNamed:)
('testing' isCaseSensitive isDriveSupported onMac onUnix onWindows)
('primitives' primCreateDirectory: primDeleteDirectory: primDeleteFileNamed: primImagePath primLookupEntryIn:index: primPathNameDelimiter primRename:to:)
('initialize-release')
!

!methodRemoval: FmDirectoryEntry class #on:!
FmDirectoryEntry class removeSelector: #on:!
!classDefinition: #FmDirectoryEntry category: #'FileMan-Core'!
FmFileEntry subclass: #FmDirectoryEntry
	instanceVariableNames: 'children'
	classVariableNames: 'CurrentDirectory ImageDirectory VMDirectory'
	poolDictionaries: ''
	category: 'FileMan-Core'!
!methodRemoval: FmFileEntry class #shutDown!
FmFileEntry class removeSelector: #shutDown!
!methodRemoval: FmFileEntry class #startUp!
FmFileEntry class removeSelector: #startUp!
SystemDictionary initialize!
