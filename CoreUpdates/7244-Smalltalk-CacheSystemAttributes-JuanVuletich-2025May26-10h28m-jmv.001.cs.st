'From Cuis7.3 [latest update: #7242] on 26 May 2025 at 10:29:59 am'!
!classDefinition: #SystemDictionary category: #'MinimalKernel-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments '
	classVariableNames: 'ChangesInitialFileSize EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore PreviousUserChangesName ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp SystemAttributeCache '
	poolDictionaries: ''
	category: 'MinimalKernel-Support'!

!SystemDictionary methodsFor: '[mkVMnImage] miscelaneous' stamp: 'jmv 5/24/2025 20:38:06'!
cachedSystemAttribute: attributeID
	(attributeID between: 1001 and: 1009)
		ifFalse: [ ^nil ].
	SystemAttributeCache ifNil: [
		SystemAttributeCache := Array new: 9.
		1 to: 9 do: [ :i |
			SystemAttributeCache at: i put: (self primGetSystemAttribute: 1000+i) ]].
	^SystemAttributeCache at: attributeID-1000! !

!SystemDictionary methodsFor: '[mkVMnImage] miscelaneous' stamp: 'jmv 5/26/2025 10:29:02'!
primGetSystemAttribute: attributeID
  	"Optional. Answer the string for the system attribute with the given 
  	integer ID. Answer nil if the given attribute is not defined on this 
  	platform. On platforms that support invoking programs from command 
  	lines (e.g., Unix), this mechanism can be used to pass command line 
  	arguments to programs written in Squeak.
  
  	By convention, the first command line argument that is not a VM
  	configuration option is considered a 'document' to be filed in. Such a
  	document can add methods and classes, can contain a serialized object,
  	can include code to be executed, or any combination of these.
  
  	Currently defined attributes include: 
  	-1000	1000th command line argument that specify VM options
  	...
  	-1		first command line argument that specify VM options
  	0		the full path name for currently executing VM
  			(or, on some platforms, just the path name of the VM's directory) 
  	1		path name of this image (better use #imageName instead)
  	2		command line argument See #processCommandLineArguments
  	3		command line argument
  	...
  	1000	last possible command line argument
  	1001	this platform's operating system 'Mac OS', 'Win32', 'unix', ...
  	1002	operating system version
  	1003	this platform's processor type
  	1004	vm version
  	1005	window system name
  	1006	vm build id
  	1007	Interpreter class (Cog VM only)
  	1008	Cogit class (Cog VM only)
 	1009	Platform source version (Cog VM only?)
  	1201	max filename length (Mac OS only)
  	1202	file last error (Mac OS only)
  	10001	hardware details (Win32 only)
  	10002	operating system details (Win32 only)
  	10003	graphics hardware details (Win32 only)
  	"

	<primitive: 149>
	^ nil! !


!SystemDictionary methodsFor: '[mkVMnImage] miscelaneous' stamp: 'jmv 5/24/2025 20:38:36'!
getSystemAttribute: attributeID
  	"Optional. Answer the string for the system attribute with the given 
  	integer ID. Answer nil if the given attribute is not defined on this 
  	platform. On platforms that support invoking programs from command 
  	lines (e.g., Unix), this mechanism can be used to pass command line 
  	arguments to programs written in Squeak.
  
  	By convention, the first command line argument that is not a VM
  	configuration option is considered a 'document' to be filed in. Such a
  	document can add methods and classes, can contain a serialized object,
  	can include code to be executed, or any combination of these.
  
  	Currently defined attributes include: 
  	-1000	1000th command line argument that specify VM options
  	...
  	-1		first command line argument that specify VM options
  	0		the full path name for currently executing VM
  			(or, on some platforms, just the path name of the VM's directory) 
  	1		path name of this image (better use #imageName instead)
  	2		command line argument See #processCommandLineArguments
  	3		command line argument
  	...
  	1000	last possible command line argument
  	1001	this platform's operating system 'Mac OS', 'Win32', 'unix', ...
  	1002	operating system version
  	1003	this platform's processor type
  	1004	vm version
  	1005	window system name
  	1006	vm build id
  	1007	Interpreter class (Cog VM only)
  	1008	Cogit class (Cog VM only)
 	1009	Platform source version (Cog VM only?)
  	1201	max filename length (Mac OS only)
  	1202	file last error (Mac OS only)
  	10001	hardware details (Win32 only)
  	10002	operating system details (Win32 only)
  	10003	graphics hardware details (Win32 only)
  	"
	(self cachedSystemAttribute: attributeID) ifNotNil: [ :cached |
		^cached ].
	^self primGetSystemAttribute: attributeID.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/24/2025 20:34:19'!
processShutDownList: quitting
	"Send #shutDown to each class that needs to wrap up before a snapshot."

	self send: #shutDown: toClassesNamedIn: ShutDownList with: quitting.
	EndianCache := nil.
	SystemAttributeCache := nil.
	PreviousUserChangesName := nil.! !

!classDefinition: #SystemDictionary category: #'MinimalKernel-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments'
	classVariableNames: 'ChangesInitialFileSize EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore PreviousUserChangesName ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp SystemAttributeCache'
	poolDictionaries: ''
	category: 'MinimalKernel-Support'!
