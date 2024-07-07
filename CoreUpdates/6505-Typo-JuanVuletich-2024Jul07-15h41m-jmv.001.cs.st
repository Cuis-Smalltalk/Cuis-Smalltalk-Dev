'From Cuis7.1 [latest update: #6504] on 7 July 2024 at 3:41:55 pm'!

!String class methodsFor: 'services' stamp: 'jmv 7/7/2024 15:41:39'!
is: aStringOrByteArray equalTo: another
	"Take advantage of String primitive for comparing, for example, ByteArrays"

	^ (self compare: aStringOrByteArray with: another collated: self caseSensitiveOrder) = 2! !

