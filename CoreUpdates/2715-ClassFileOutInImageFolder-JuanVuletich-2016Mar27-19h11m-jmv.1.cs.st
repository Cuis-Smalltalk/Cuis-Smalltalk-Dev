'From Cuis 4.2 of 25 July 2013 [latest update: #2710] on 27 March 2016 at 7:11:47.722115 pm'!

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 3/27/2016 17:26'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver."

	FmDirectoryEntry smalltalkImageDirectory // (self name, '.st') writeStream: [ :stream |
		stream timeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: stream ]].
		self fileOutOn: stream moveSource: false toFile: 0 ]! !

