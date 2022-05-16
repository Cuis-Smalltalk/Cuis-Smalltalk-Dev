'From Cuis 6.0 [latest update: #5169] on 16 May 2022 at 11:17:31 am'!

!DropFilesEvent methodsFor: 'files' stamp: 'jmv 5/16/2022 11:16:19'!
fileNamesDo: aBlock

	1 to: numberOfFiles do: [ :fileNumber | | utf8Bytes |
		(self primDropRequestFileName: fileNumber) ifNotNil: [ :primUtf8Bytes |
		utf8Bytes _ primUtf8Bytes.
		utf8Bytes class format = ByteArray format  												"True unless image is in 32bit V3 format"
			ifTrue: [ ByteArray adoptInstance: utf8Bytes ]
			ifFalse: [ utf8Bytes _ ByteArray withAll: utf8Bytes ].
		aBlock value: (String fromUtf8: utf8Bytes) ]].! !

