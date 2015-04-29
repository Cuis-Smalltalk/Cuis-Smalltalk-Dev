'From Cuis 4.2 of 25 July 2013 [latest update: #2257] on 19 April 2015 at 8:50:37.022912 am'!
!classDefinition: #AsyncFile category: #'System-Files'!
Object subclass: #AsyncFile
	instanceVariableNames: 'name writeable semaphore fileHandle '
	classVariableNames: 'Busy Error '
	poolDictionaries: ''
	category: 'System-Files'!

!AsyncFile methodsFor: 'as yet unclassified' stamp: 'jmv 4/19/2015 08:49'!
readByteCount: byteCount fromFilePosition: fPosition onCompletionDo: aBlock
	"Start a read operation to read byteCount's from the given position in this file. and fork a process to await its completion. When the operation completes, evaluate the given block. Note that, since the completion block may run asynchronous, the client may need to use a SharedQueue or a semaphore for synchronization."

	| buffer |
	buffer _ String new: byteCount.
	self primReadStart: fileHandle fPosition: fPosition count: byteCount.
	"here's the process that awaits the results:"
	[
		| n |
		[	semaphore wait.
		  	n _ self primReadResult: fileHandle intoBuffer: buffer at: 1 count: byteCount.
		  	n = -1 "Busy".
		] whileTrue.  "loop while busy in case the semaphore had excess signals"
		n = -2 "Error" ifTrue: [^ self error: 'asynchronous read operation failed'].
		aBlock value: buffer.
	] forkAt: Processor userInterruptPriority.
! !

!AsyncFile methodsFor: 'as yet unclassified' stamp: 'jmv 4/19/2015 08:49'!
writeBuffer: buffer atFilePosition: fPosition onCompletionDo: aBlock
	"Start an operation to write the contents of the buffer at given position in this file, and fork a process to await its completion. When the write completes, evaluate the given block. Note that, since the completion block runs asynchronously, the client may need to use a SharedQueue or a semaphore for synchronization."

	self primWriteStart: fileHandle
		fPosition: fPosition
		fromBuffer: buffer
		at: 1
		count: buffer size.
	"here's the process that awaits the results:"
	[
		| n |
		[	semaphore wait.
		  	n _ self primWriteResult: fileHandle.
		  	n = -1 "Busy".
		] whileTrue.  "loop while busy in case the semaphore had excess signals"
		n = -2 "Error" ifTrue: [^ self error: 'asynchronous write operation failed'].
		n = buffer size ifFalse: [^ self error: 'did not write the entire buffer'].
		aBlock value.
	] forkAt: Processor userInterruptPriority.
! !

!methodRemoval: AsyncFile class #initialize!
AsyncFile class removeSelector: #initialize!
AsyncFile initialize!
!classDefinition: #AsyncFile category: #'System-Files'!
Object subclass: #AsyncFile
	instanceVariableNames: 'name writeable semaphore fileHandle'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!
