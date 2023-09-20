'From Cuis 6.0 [latest update: #6031] on 20 September 2023 at 3:40:33 pm'!

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 9/20/2023 15:39:48'!
fileOutHierarchy
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Morph fileOutHierarchy
	"

	DirectoryEntry smalltalkImageDirectory // (self name, '-hierarcy.st') writeStreamDo: [ :stream |
		stream timeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: stream ]].
		self withAllSubclassesDo: [ :each |
			each fileOutOn: stream moveSource: false toFile: 0 ]].! !

