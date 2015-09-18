'From Cuis 4.2 of 25 July 2013 [latest update: #2500] on 17 September 2015 at 11:44:53.079465 am'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 9/17/2015 11:19'!
primImageName
	"Answer the full path name for the current image."
	"Smalltalk primImageName"

	<primitive: 121>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 9/17/2015 11:19'!
primVmPath
	"Answer the path for the directory containing the Smalltalk virtual machine. Return nil if this primitive is not implemented."
	"Smalltalk primVmPath"

	<primitive: 142>! !


!FmFileIOAccessor methodsFor: 'primitives' stamp: 'jmv 9/17/2015 11:31'!
imagePath
	"Answer the full path name for the current image."
	"
	self default imagePath
	"

	| answer |
	answer _ self primImagePath.
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


!String methodsFor: '*fileman-core-testing' stamp: 'jmv 9/17/2015 10:36'!
beginsWithWindowsDriveName
	"Answer doesn't depend on running on Windows or not"
	self size < 2 ifTrue: [ ^false ].
	^self first isDriveLetter
		and: [ self second isDriveSeparator
			and: [ self size = 2 or: [ self third isPathSeparator ]]]! !


!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 9/17/2015 11:29'!
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

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 9/17/2015 11:28'!
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


!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 9/17/2015 10:56'!
entriesIn: directoryFullPath from: parentEntryOrNil
	"
	Warning: Private. Only to be called from within FileMan.
	Warning: If parentEntryOrNil is nil, answered instances are broken, 
	and essentially only useful for asking back the attributes we have just set.
	Not pretty.

		FmFileIOAccessor default entriesIn: '' from: nil
	
		FmFileIOAccessor default entriesIn: '.' from: '.' asFileEntry
	"
	| entries index done entryArray entry isDirectory |
	entries := OrderedCollection new: 200.
	index := 1.
	done := false.
	[done] whileFalse: [
		entryArray := self lookupEntryIn: directoryFullPath index: index.
		#badDirectoryPath = entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				entry _ isDirectory
					ifTrue: [FmDirectoryEntry new]
					ifFalse: [FmFileEntry new].
				entry isDirectory: isDirectory.
				entry name: (entryArray at: 1).
				entry creationTime: (entryArray at: 2).
				entry modificationTime: (entryArray at: 3).
				entry fileSize: (entryArray at: 5).
				parentEntryOrNil ifNotNil: [
					entry parent: parentEntryOrNil ].
				entries addLast: entry ].
		index := index + 1].

	^entries asArray! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 9/17/2015 11:31'!
imageDirectoryPath
	"Note that pathname is not encoded
	self default imageDirectoryPath
	"
	| imagePath lastIndex |
	imagePath := self imagePath.
	lastIndex := imagePath lastIndexOf: self pathNameDelimiter.
	^ imagePath copyFrom: 1 to: lastIndex! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 9/17/2015 11:32'!
currentDriveName
	"
	self default currentDriveName
	"
	^self imagePath asDriveName! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 9/17/2015 11:11'!
drives
	"
	Answer a collection of Strings
	FmFileIOAccessor default drives
	"
	drives ifNil: [
		drives _ self onUnix
			ifTrue: [#()]
			ifFalse: [(self entriesIn: '' from: nil) collect: [ :each | each name]]].
	^drives! !

