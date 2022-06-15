'From Cuis 6.0 [latest update: #5290] on 15 June 2022 at 9:53:27 am'!

!UniFileStream class methodsFor: 'file creation' stamp: 'jmv 6/15/2022 09:53:15'!
crc16OfFileNamed: fileName
	"
	UniFileStream crc16OfFileNamed: 'cursor.jpeg'
	UniFileStream crc16OfFileNamed: 'deafultPID.txt'
	"

	^[fileName asFileEntry readStreamDo: [ :stream |
		stream crc16 ]]
			on: FileDoesNotExistException do: nil! !

