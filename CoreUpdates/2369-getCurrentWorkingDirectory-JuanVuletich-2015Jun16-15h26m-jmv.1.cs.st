'From Cuis 4.2 of 25 July 2013 [latest update: #2368] on 16 June 2015 at 3:27:48.190158 pm'!

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 6/16/2015 15:25'!
getCurrentWorkingDirectory
	"Do our best effort to answer the path from wich Cuis was started.

	Smalltalk getCurrentWorkingDirectory


On Linux, starting as
	juan@juandebian:/media/sf_SharedWithLinux/testPayload03/payload$      coglinux/bin/squeak        Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image
we get:
	Smalltalk primGetCurrentWorkingDirectoryUnix    '/media/sf_SharedWithLinux/testPayload03/payload'
	Smalltalk vmPath                       					'/media/sf_SharedWithLinux/testPayload03/payload/coglinux/lib/squeak/4.5-3370/'
	FileDirectory default   		UnixFileDirectory on '/media/sf_SharedWithLinux/testPayload03/payload/Cuis-Smalltalk-Dev-master'
	Smalltalk getCurrentWorkingDirectory 			'/media/sf_SharedWithLinux/testPayload03/payload'


On Windows 7, starting as
	C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload>        cogwin\squeak.exe            Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image
we get:
	Smalltalk primGetCurrentWorkingDirectoryWindows       	'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master'
	Smalltalk vmPath    										'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\cogwin\'
	FileDirectory default   				DosFileDirectory on 'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master'
	Smalltalk getCurrentWorkingDirectory  					'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload'

	Easy on Unix. Harder on Windows, because #primGetCurrentWorkingDirectoryWindows insists on lying and answers the directory containing the image.
	"
	
	| fullImagePathName imageSpecCmdLineArgument |

	"Easy on Unix"
	 self primGetCurrentWorkingDirectoryUnix ifNotNil: [ :cwd | ^ cwd ].

	"On Windows, extract shell path if image is in subtree"
	fullImagePathName _ Smalltalk imageName.
	imageSpecCmdLineArgument _ Smalltalk getSystemAttribute: 1.
	(imageSpecCmdLineArgument size < fullImagePathName size and: [			"Not if they are equal, as if they are both fully qualified (absolute) paths"
		fullImagePathName asPathTokens endsWith: imageSpecCmdLineArgument asPathTokens]) ifTrue: [
			^ fullImagePathName copyFrom: 1 to: fullImagePathName size - imageSpecCmdLineArgument size - 1 ].

	"If we don't know better, answer nil. If appropriate, senders do:
		Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]
	to have, at least, some valid directory
	"
	^ nil! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 6/16/2015 12:17'!
primGetCurrentWorkingDirectoryUnix
	"Call getcwd() to get the current working directory.
	Shamelessly taken from OSProcess.pck.st and copied here.
	Answer nil on fail (i.e., non-Unix)"

	"
	Smalltalk primGetCurrentWorkingDirectoryUnix
	"

	<primitive: 'primitiveGetCurrentWorkingDirectory' module: 'UnixOSProcessPlugin'>
	^ nil! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 6/16/2015 12:17'!
primGetCurrentWorkingDirectoryWindows
	"Call getcwd() to get the current working directory.
	Shamelessly taken from OSProcess.pck.st and copied here.
	Answer nil on fail (i.e., non-Windows)
	Note:
		Unfortunately, yhis windows implementation seems to always answer the directory 
		containing the image, and not the real Windows current directory.
		See #getCurrentWorkingDirectory"

	"
	Smalltalk primGetCurrentWorkingDirectoryWindows
	"

	<primitive: 'primitiveGetCurrentWorkingDirectory' module: 'Win32OSProcessPlugin'>
	^ nil! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 6/16/2015 13:21'!
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

	<primitive: 149>
	^ nil! !

