'From Cuis 5.0 of 7 November 2016 [latest update: #3338] on 27 May 2018 at 7:35:34 pm'!

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/27/2018 19:35:13'!
browseMethodsWithString: aString
	"Launch a browser on all methods that contain string literals with aString as a substring. The search is case-insensitive, unless the shift key is pressed, in which case the search is case-sensitive."

	^ self browseMethodsWithString: aString matchCase: false

	"Smalltalk browseMethodsWithString: 'Testing' matchCase: false"
	"Smalltalk browseMethodsWithString: 'Testing' matchCase: true"! !

