'From Cuis 4.2 of 25 July 2013 [latest update: #2520] on 5 October 2015 at 11:57:22.72142 am'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/5/2015 09:20'!
fileOutCategory: catName
	"FileOut the named category"

	FmDirectoryEntry smalltalkImageDirectory / (self name , '-' , catName , '.st') writeStream: [ :fileStream |
		fileStream timeStamp.
		self fileOutCategory: catName on: fileStream moveSource: false toFile: 0 ]! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/5/2015 09:20'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asString, ' not found'].
	nameBody _ self name , '-' , (selector copyReplaceAll: ':' with: '').
	FmDirectoryEntry smalltalkImageDirectory / (nameBody , '.st') writeStream: [ :fileStream |
		fileStream timeStamp.
		self printMethodChunk: selector withPreamble: true
			on: fileStream moveSource: false toFile: 0 ]! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 10/5/2015 09:26'!
fileOutAllCategories
	"
	Cursor write showWhile: [
		SystemOrganization fileOutAllCategories ]
	"
	((Smalltalk imageName withoutSuffix: '.image'), '-AllCode.st') asFileEntry writeStream: [ :stream |
		self categories do: [ :category |
			self fileOutCategoryNoPoolsNoInit: category on: stream ]]! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 10/5/2015 09:23'!
fileOutCategory: category
	"FileOut all the classes in the named system category."

	FmDirectoryEntry smalltalkImageDirectory / (category asFileName , '.st') writeStream: [ :fileStream |
		self fileOutCategory: category on: fileStream initializing: true ]! !


!String methodsFor: '*fileman-core-converting' stamp: 'jmv 10/5/2015 08:46'!
asDriveName

	FmFileIOAccessor default onWindows ifTrue: [
		self beginsWithWindowsDriveName ifTrue: [ 
		^self copyFrom: 1 to: 2 ]].

	(FmFileIOAccessor default onMac) ifTrue: [ 
		^self upToFirstPathSeparator ].
	
	^ nil! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 10/5/2015 09:23'!
condenseChanges
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount oldChangesLocalName oldChangesPathName |
	FmDirectoryEntry smalltalkImageDirectory / 'ST80.temp' forceWriteStream: [ :f |
		f timeStamp.
		'Condensing Changes File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :bar |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class | 
					bar value: (classCount _ classCount + 1).
					class moveChangesTo: f.
					class putClassCommentToCondensedChangesFile: f.
					class class moveChangesTo: f ]].
		LastQuitLogPosition _ f position ].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges _ SourceFiles at: 2.
	oldChangesPathName _ oldChanges name.
	oldChangesLocalName _ oldChanges localName.
	oldChanges close.
	(oldChangesPathName, '.old') asFileEntry delete.
	oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old'.
	FmDirectoryEntry smalltalkImageDirectory / 'ST80.temp' rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: (FileStream oldFileNamed: oldChangesPathName).

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/5/2015 08:30'!
imageName
	"Answer the full path name for the current image."
	"
	Smalltalk imageName
	"

	| answer |
	answer _ self primImageName.
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
					answer _ answer capitalized ].
	^answer ifNil: [ '' ]! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/5/2015 08:31'!
imagePath
	"Answer the path for the directory containing the image file."
	"
	Smalltalk imagePath
	"

	^ self imageName path
! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/5/2015 08:30'!
primImageName
	"Answer the full path name for the current image."
	"
	Smalltalk primImageName
	"

	<primitive: 121>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/5/2015 08:34'!
vmPath
	"Answer the path for the directory containing the Smalltalk virtual machine. Return the empty string if this primitive is not implemented."
	"
	Smalltalk vmPath
	"

	| answer |
	answer _ self primVmPath.
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
					answer _ answer capitalized ].
	^answer ifNil: [ '' ]! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 10/5/2015 09:23'!
logError: errMsg inContext: aContext to: localFileName
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	[
		FmDirectoryEntry smalltalkImageDirectory / localFileName forceWriteStream: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: Error do: [] "avoid recursive errors"! !


!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 10/5/2015 08:47'!
currentDriveName
	"
	self default currentDriveName
	"
	^Smalltalk imageName asDriveName! !


!InstructionPrinter class methodsFor: 'printing' stamp: 'jmv 10/5/2015 09:20'!
printClass: class 
	"Create a file whose name is the argument followed by '.bytes'. Store on 
	the file the symbolic form of the compiled methods of the class."

	FmDirectoryEntry smalltalkImageDirectory / (class name , '.bytes') writeStream: [ :file |
		class selectorsDo: [ :sel | 
			file newLine; nextPutAll: sel; newLine.
			(self on: (class compiledMethodAt: sel)) printInstructionsOn: file ]].

	"
	InstructionPrinter printClass: Parser.
	"! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 10/5/2015 09:20'!
fileOut

	FmDirectoryEntry smalltalkImageDirectory / (self name, '.st') writeStream: [ :stream |
		self fileOutOn: stream.
		self needsInitialize ifTrue: [
			stream newLine; nextChunkPut: self name,' initialize' ]]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 10/5/2015 09:20'!
fileOutCategory: categoryName

	FmDirectoryEntry smalltalkImageDirectory / (self name,'-',categoryName,'.st') writeStream: [ :f |
		self fileOutMethods: (self organization listAtCategoryNamed: categoryName) on: f ]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 10/5/2015 09:20'!
fileOutMethod: selector

	FmDirectoryEntry smalltalkImageDirectory / (name,'-', selector, '.st') writeStream: [ :f |
		self fileOutMethods: (Array with: selector) on: f ]! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 10/5/2015 09:21'!
printSpaceAnalysis	
	"
	SpaceTally new printSpaceAnalysis
	"

	FmDirectoryEntry smalltalkImageDirectory / 'STspace.text' writeStream: [ :stream |
		self printSpaceAnalysis: 1 on: stream ]! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 10/5/2015 09:21'!
saveTo: aFileName
	"
	| st |
	st := SpaceTally new.
	st spaceTally: (Array with: EllipseMorph with: Point).
	st saveTo: 'spaceTally2'
	"

	FmDirectoryEntry smalltalkImageDirectory / aFileName forceWriteStream: [ :stream |
		results do: [ :each |
				stream nextPutAll: each analyzedClassName asString; 
						nextPutAll: ' '; nextPutAll: each codeSize printString; 
						nextPutAll: ' '; nextPutAll: each instanceCount printString; 
						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; newLine ]]! !

!methodRemoval: FmFileIOAccessor #imageDirectoryPath!
FmFileIOAccessor removeSelector: #imageDirectoryPath!
!methodRemoval: FmFileIOAccessor #imagePath!
FmFileIOAccessor removeSelector: #imagePath!
!methodRemoval: FmFileIOAccessor #primImagePath!
FmFileIOAccessor removeSelector: #primImagePath!
!methodRemoval: FmDirectoryEntry class #default!
FmDirectoryEntry class removeSelector: #default!
