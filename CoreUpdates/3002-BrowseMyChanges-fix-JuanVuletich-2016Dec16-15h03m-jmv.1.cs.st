'From Cuis 5.0 of 7 November 2016 [latest update: #3000] on 16 December 2016 at 3:13:12 pm'!

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 12/16/2016 15:05:52'!
browseMyChanges
	"Browse only the changes (in the changes file) by the current author.
	Smalltalk browseMyChanges
	"
	self browseAllSelect: [ :method |
       method fileIndex > 1 "only look at changes file"
               and: [ method timeStamp beginsWith: Utilities authorInitials, ' ' ]]! !

