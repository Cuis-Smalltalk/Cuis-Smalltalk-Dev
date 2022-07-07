'From Cuis 6.0 [latest update: #5366] on 7 July 2022 at 2:34:24 pm'!

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 14:34:02'!
padToEndIfCantTruncate
	"Only makes sense for file streams with existing content.
	On file systems that don't support truncating this is needed.
	If truncating is supported, try that first"

	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."

	| pad |
	self atEnd ifTrue: [^ self].
	self truncate.
	self atEnd ifTrue: [^ self].
	pad := Character space numericValue.
	self nextPutAll: (ByteArray new: ((self size - self position) min: 20000) 
							withAll: pad)! !

