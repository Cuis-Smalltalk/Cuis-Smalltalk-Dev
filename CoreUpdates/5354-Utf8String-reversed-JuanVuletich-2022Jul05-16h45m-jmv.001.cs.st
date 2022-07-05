'From Cuis 6.0 [latest update: #5355] on 5 July 2022 at 4:57:47 pm'!

!Utf8String methodsFor: 'converting' stamp: 'jmv 7/5/2022 16:57:21'!
reversed
	"
	'frog' asUtf8String reversed
	"
	^Utf8String fromUtf32Words: self asUtf32Words reversed! !

