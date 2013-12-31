'From Cuis 4.0 of 21 April 2012 [latest update: #1262] on 25 April 2012 at 11:22:21 am'!

!CodeProvider methodsFor: 'testing' stamp: 'jmv 4/25/2012 11:20'!
isRefusingToAccept
	"Answer whether receiver, given its current contentsSymbol, could accept code happily if asked to"

	^ (#(byteCodes documentation) includes: self contentsSymbol)! !


!PluggableTextModel methodsFor: 'testing' stamp: 'jmv 4/25/2012 11:18'!
refusesToAccept
	^textProvider is: #refusingToAccept! !


!CodeProvider methodsFor: 'testing' stamp: 'jmv 4/25/2012 11:20'!
is: aSymbol
	aSymbol = #refusingToAccept
		ifTrue: [ ^self isRefusingToAccept ].
	^(#(CodeProvider #ShoutEnabled) pointsTo: aSymbol)
		or: [ super is: aSymbol ]! !

!methodRemoval: CodeProvider #refusesToAccept!
CodeProvider removeSelector: #refusesToAccept!
!methodRemoval: TextProvider #refusesToAccept!
TextProvider removeSelector: #refusesToAccept!

!TextProvider reorganize!
('contents' acceptedContents acceptedContentsChanged acceptedStringOrText)
('accessing' contentsSelection)
!

