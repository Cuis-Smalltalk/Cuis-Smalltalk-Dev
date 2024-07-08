'From Cuis7.1 [latest update: #6506] on 7 July 2024 at 4:29:24 pm'!

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 7/7/2024 16:29:05'!
getCurrentWorkingDirectory
	"Do our best effort to answer the path from wich Cuis was started.

	Smalltalk getCurrentWorkingDirectory


On Linux, starting as
	juan@juandebian:/media/sf_SharedWithLinux/testPayload03/payload$      coglinux/bin/squeak        Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image
we get:
	Smalltalk vmPath                       					'/media/sf_SharedWithLinux/testPayload03/payload/coglinux/lib/squeak/4.5-3370/'
	Smalltalk imagePath   							'/media/sf_SharedWithLinux/testPayload03/payload/Cuis-Smalltalk-Dev-master'
	Smalltalk primGetCurrentWorkingDirectoryUnix    '/media/sf_SharedWithLinux/testPayload03/payload'
	Smalltalk getCurrentWorkingDirectory 			'/media/sf_SharedWithLinux/testPayload03/payload'


On Windows 7, starting as
	C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload>        cogwin\squeak.exe            Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image
we get:
	Smalltalk vmPath    										'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\cogwin\'
	Smalltalk imagePath   									'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master'
	Smalltalk primGetCurrentWorkingDirectoryWindows       	'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master'
	Smalltalk getCurrentWorkingDirectory  					'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload'

	Easy on Unix. Harder on Windows, because #primGetCurrentWorkingDirectoryWindows insists on lying and answers the directory containing the image.
	"
	
	| fullImagePathName imageSpecCmdLineArgument  |

	"Easy on Unix"
	 self primGetCurrentWorkingDirectoryUnix ifNotNil: [ :utf8BytesInAStringInstance | | answer |
		"It seems on Mac, when dropping image on VM, we get '/', that is not really meaningful"
		(utf8BytesInAStringInstance = '/' and: [ self platformName = 'Mac OS' ]) ifFalse: [
			"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
			contents are invalid!! Convert it ASAP into a reasonable ByteArray + UnicodeString."
			answer := utf8BytesInAStringInstance.
			(String isAscii: answer) ifFalse: [
				ByteArray adoptInstance: answer.
				answer := UnicodeString fromUtf8Bytes: answer ].
			^ answer ]].

	"On Windows, extract shell path if image is in subtree"
	fullImagePathName := self imageName.

	"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
	contents are invalid!! Convert it ASAP into a reasonable ByteArray + UnicodeString."
	imageSpecCmdLineArgument := self getSystemAttribute: 1.
	"May be nil, for instance, when running in SqueakJS."
	imageSpecCmdLineArgument notNil ifTrue: [
		(String isAscii: imageSpecCmdLineArgument) ifFalse: [
			ByteArray adoptInstance: imageSpecCmdLineArgument.
			imageSpecCmdLineArgument := UnicodeString fromUtf8Bytes: imageSpecCmdLineArgument ].

		(imageSpecCmdLineArgument size < fullImagePathName size and: [			
			"Not if they are equal, as if they are both fully qualified (absolute) paths"
			fullImagePathName asPathTokens endsWith: imageSpecCmdLineArgument asPathTokens]) ifTrue: [
				^ fullImagePathName copyFrom: 1 to: fullImagePathName size - imageSpecCmdLineArgument size - 1 ]].

	"If we don't know better, answer nil. If appropriate, senders do:
		Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]
	to have, at least, some valid directory
	"
	^ nil! !


"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
PreferenceSet setDefaultFont: 'DejaVu Sans'.!


