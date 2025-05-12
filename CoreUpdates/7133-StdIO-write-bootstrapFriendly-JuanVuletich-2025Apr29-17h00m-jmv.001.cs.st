'From Cuis7.3 [latest update: #7115] on 29 April 2025 at 5:02:36 pm'!

!StdIOWriteStream methodsFor: 'minimalKernel - Bootstrap' stamp: 'jmv 4/29/2025 17:01:32'!
primWrite: id from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	self error: 'StdIOWriteStream [', name, '] write failed'.! !


!StdIOWriteStream reorganize!
('minimalKernel - Bootstrap' flush newLine nextPut: nextPutAll: openOnHandle:name: primFlush: primWrite:from:startingAt:count: printOn: space)
!

