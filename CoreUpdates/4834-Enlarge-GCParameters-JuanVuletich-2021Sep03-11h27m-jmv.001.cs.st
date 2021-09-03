'From Cuis 5.0 [latest update: #4832] on 3 September 2021 at 12:43:37 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 9/3/2021 12:43:27'!
setDefaultGCParameters
	"Adjust the VM's default GC parameters to avoid premature tenuring, and too frequent scavenging.
	Parameters set here persist in saved images, so we set them image save for release.
	See #setGCParameters"

	"Desired Eden size: "
	Smalltalk vmParameterAt: 45 put: `16*1024*1024`.! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 9/3/2021 12:28:41'!
saveAsNewReleaseAndQuit
	"Save the image/changes using the next available version number.
	This is usually only used to prepare updated Cuis images for distribution."
	"
	Smalltalk saveAsNewReleaseAndQuit
	"
	| fileName newName changesName systemVersion |
	systemVersion _ SystemVersion current.
	fileName _ String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: systemVersion versionMajor;
			nextPut: $.;
			print: systemVersion versionMinor;
			nextPut: $-;
			print: systemVersion highestUpdate.
		Smalltalk isSpur
			ifTrue: [	
				Smalltalk wordSize = 4 ifTrue: [
					strm nextPutAll: '-32' ]]
			ifFalse: [
				strm nextPutAll: '-v3' ]].
	newName _ fileName, '.image'.
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName _ DirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFileEntry exists ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', changesName, '
curiously already present, even though there is no
corresponding .image file.
Please remedy manually and then repeat your request.' ].
	self setDefaultGCParameters.
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAndQuitAs: newName clearAllClassState: true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 9/3/2021 12:28:53'!
saveAsNewReleaseAndStay
	"Save the image/changes using the next available version number.
	This is usually only used to prepare updated Cuis images for distribution."
	"
	Smalltalk saveAsNewReleaseAndStay
	"
	| fileName newName changesName systemVersion |
	systemVersion _ SystemVersion current.
	fileName _ String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: systemVersion versionMajor;
			nextPut: $.;
			print: systemVersion versionMinor;
			nextPut: $-;
			print: systemVersion highestUpdate.
		Smalltalk isSpur
			ifTrue: [	
				Smalltalk wordSize = 4 ifTrue: [
					strm nextPutAll: '-32' ]]
			ifFalse: [
				strm nextPutAll: '-v3' ]].
	newName _ fileName, '.image'.
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName _ DirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFileEntry exists ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', changesName, '
curiously already present, even though there is no
corresponding .image file.
Please remedy manually and then repeat your request.' ].
	self setDefaultGCParameters.
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAndStayAs: newName clearAllClassState: true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 9/3/2021 12:43:22'!
setGCParameters
	"Adjust the VM's default GC parameters to avoid premature tenuring, and too frequent scavenging.
	Parameters set here don't persist, so we set them on startup.
	See #setDefaultGCParameters"

	"Grow old memory in chunks of: "
	Smalltalk vmParameterAt: 25 put: `32*1024*1024`.
	"Shrink heap when unused memory is at least: "
	Smalltalk vmParameterAt: 24 put: `64*1024*1024`.

	Smalltalk isSpur
		ifTrue: [
			"Note: (jmv, 9/2021) It seems modern Spur VMs ignore parameter 6. This all seems to be bogus."
			| proportion edenSize survivorSize averageObjectSize numObjects |
			proportion := 0.9. "tenure when 90% of pastSpace is full"
			edenSize := self vmParameterAt: 44.
			survivorSize := edenSize / 5.0. "David's paper uses 140Kb eden + 2 x 28kb survivor spaces; Spur uses the same ratios :-)"
			averageObjectSize := 8 * self wordSize. "a good approximation"
			numObjects := (proportion * survivorSize / averageObjectSize) rounded.
			self vmParameterAt: 6 put: numObjects.  "tenure when more than this many objects survive the GC"
			"/Note: (jmv, 9/2021)"
			
			"Do a full GC when used memory grows by this factor. Fails on non-Spur VMs.
			Default is 0.333.
			2.0 means gull GC when heap size triples."
			Smalltalk vmParameterAt: 55 put: 2.0.
			]
		ifFalse: [
			Smalltalk vmParameterAt: 5 put: 4000.  "do an incremental GC after this many allocations"
			Smalltalk vmParameterAt: 6 put: 2000.  "tenure when more than this many objects survive the GC"
			].! !

