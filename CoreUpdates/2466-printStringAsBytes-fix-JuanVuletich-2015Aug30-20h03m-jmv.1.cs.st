'From Cuis 4.2 of 25 July 2013 [latest update: #2464] on 30 August 2015 at 8:15:08.027488 pm'!

!Integer methodsFor: 'printing' stamp: 'jmv 8/30/2015 20:14'!
printStringAsBytesDecimal
	"Answer a terse, easily-readable representation of this Integer reprsenting a number of bytes.  Useful for file-browsers.
	123 printStringAsBytesDecimal
	(12*1000) printStringAsBytesDecimal
	(1000*1000) printStringAsBytesDecimal
	
	1024 printStringAsBytesDecimal
	(12*1024) printStringAsBytesDecimal
	(1024*1024) printStringAsBytesDecimal
	(1024*1024*1024) printStringAsBytesDecimal
	(1024*1024*1024*1024) printStringAsBytesDecimal
	(30 factorial) printStringAsBytesDecimal
	
	See https://en.wikipedia.org/wiki/Kibibyte
	See #printStringAsBytes
	"
	| suffixes |
	suffixes _ {
		'KB'	"Kilobytes".
		'MB'	"Megabytes".
		'GB'	"Gigabytes".
		'TB'	"Terabytes".
		'PB'	"Petabytes".
		'EB'	"Exabytes".
		'ZB'	"Zettabytes".
		'YB'	"Yottabytes"}.
	suffixes size to: 1 by: -1 do: [ :index |  | units |
		units _ 1000 raisedTo: index.
		self >= units ifTrue: [
			^(String streamContents: [ :strm |
				(self / units) printOn: strm fractionDigits: 2.
				strm nextPutAll: (suffixes at: index)
				])]].
	^ self asString, 'B'		"bytes"! !


!Integer methodsFor: 'printing' stamp: 'jmv 8/30/2015 20:13'!
printStringAsBytes
	"Answer a terse, easily-readable representation of this Integer reprsenting a number of bytes.  Useful for file-browsers.
	123 printStringAsBytes
	1024 printStringAsBytes
	(12*1024) printStringAsBytes
	(1024*1024) printStringAsBytes
	(1024*1024*1024) printStringAsBytes
	(1024*1024*1024*1024) printStringAsBytes
	(30 factorial) printStringAsBytes
	
	See https://en.wikipedia.org/wiki/Kibibyte
	See #printStringAsBytes
	"
	| suffixes |
	suffixes _ {
		'KiB'	"Kibibytes".
		'MiB'	"Mebibytes".
		'GiB'	"Gibibytes".
		'TiB'	"Tebibytes".
		'PiB'	"Pebibytes".
		'EiB'	"Exbibytes".
		'ZiB'	"Zebibytes".
		'YiB'	"Yobibytes"}.
	suffixes size to: 1 by: -1 do: [ :index |  | units |
		units _ 1024 raisedTo: index.
		self >= units ifTrue: [
			^(String streamContents: [ :strm |
				(self / units) printOn: strm fractionDigits: 2.
				strm nextPutAll: (suffixes at: index)
				])]].
	^ self asString, 'B'		"bytes"! !

!methodRemoval: Integer #printStringAsBytesTraditionalStyle!
Integer removeSelector: #printStringAsBytesTraditionalStyle!
