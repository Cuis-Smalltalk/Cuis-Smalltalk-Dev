'From Cuis6.3 [latest update: #6362] on 13 May 2024 at 10:01:53 am'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 5/13/2024 10:01:19'!
changeImageNameTo: aString
	"Convert to the format the VM expects: an instance of String that
	holds UTF-8 bytes. (If a pure ascii byte String, that's already the case)"
	| utf8BytesInAStringInstance |
	(aString isByteString and: [ aString isAscii ])
		ifTrue: [ utf8BytesInAStringInstance := aString ]
		ifFalse: [
			utf8BytesInAStringInstance := aString asUtf8Bytes.
			String adoptInstance: utf8BytesInAStringInstance ].
	self primImageName: utf8BytesInAStringInstance.
	LastImageName := self imageName! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 5/13/2024 09:58:49'!
imageName
	"Answer the full path name for the current image."
	"
	Smalltalk imageName
	"

	| answer |
	"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
	contents are invalid!! Convert it ASAP into a reasonable ByteArray + UnicodeString."
	answer := self primImageName.
	(String isAscii: answer) ifFalse: [
		ByteArray adoptInstance: answer.
		answer := UnicodeString fromUtf8Bytes: answer ].

	"On some setups (Cog VM in Git Bash under Windows)
		Smalltalk primVmPath 
				answer begins with 'c:\' (lowercase)
	but 
		(FileDirectory on: '') primLookupEntryIn: '' index: 1
				answer is  #('C:' 0 0 true 0) (uppercase)
	Make Windows drive letters be capitalized!!
	"
	(answer size > 1 and: [
		answer first isLowercase and: [
			answer second isDriveSeparator ]]) ifTrue: [
					answer := answer capitalized ].
	^answer! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 5/13/2024 09:58:16'!
vmPath
	"Answer the path for the directory containing the Smalltalk virtual machine. Return the empty string if this primitive is not implemented."
	"
	Smalltalk vmPath
	"

	| answer |

	"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
	contents are invalid!! Convert it ASAP into a reasonable ByteArray + UnicodeString."
	answer := self primVmPath.
	(String isAscii: answer) ifFalse: [
		ByteArray adoptInstance: answer.
		answer := UnicodeString fromUtf8Bytes: answer ].

	"On some setups (Cog VM in Git Bash under Windows)
		Smalltalk primVmPath 
				answer begins with 'c:\' (lowercase)
	but 
		(FileDirectory on: '') primLookupEntryIn: '' index: 1
				answer is  #('C:' 0 0 true 0) (uppercase)
	Make Windows drive letters be capitalized!!
	"
	(answer size > 1 and: [
		answer first isLowercase and: [
			answer second isDriveSeparator ]]) ifTrue: [
					answer := answer capitalized ].
	^answer! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 5/13/2024 09:55:51'!
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
	(String isAscii: imageSpecCmdLineArgument) ifFalse: [
		ByteArray adoptInstance: imageSpecCmdLineArgument.
		imageSpecCmdLineArgument := UnicodeString fromUtf8Bytes: imageSpecCmdLineArgument ].

	(imageSpecCmdLineArgument size < fullImagePathName size and: [			
		"Not if they are equal, as if they are both fully qualified (absolute) paths"
		fullImagePathName asPathTokens endsWith: imageSpecCmdLineArgument asPathTokens]) ifTrue: [
			^ fullImagePathName copyFrom: 1 to: fullImagePathName size - imageSpecCmdLineArgument size - 1 ].

	"If we don't know better, answer nil. If appropriate, senders do:
		Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]
	to have, at least, some valid directory
	"
	^ nil! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 5/13/2024 09:57:42'!
vmFileName
	"Answer the absolute file name of the the Smalltalk virtual machine"
	"
	Smalltalk vmFileName
	"

	| answer |
	"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
	contents are invalid!! Convert it ASAP into a reasonable ByteArray + UnicodeString."
	answer := self primVmFileName.
	(String isAscii: answer) ifFalse: [
		ByteArray adoptInstance: answer.
		answer := UnicodeString fromUtf8Bytes: answer ].
	^answer! !

