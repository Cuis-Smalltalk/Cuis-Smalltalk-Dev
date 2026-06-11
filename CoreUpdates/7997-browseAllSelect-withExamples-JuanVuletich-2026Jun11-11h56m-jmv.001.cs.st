'From Cuis7.9 [latest update: #7995] on 11 June 2026 at 12:32:11 pm'!

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 11/Jun/2026 12:15:27'!
browseAllSelect: aBlock label: aString
	"Create and schedule a message browser on each method that, when used 
	as the block argument to aBlock gives a true result. For example,
	
	Smalltalk browseAllSelect: [:method | method numLiterals > 10] label: 'Methods with more than ten literals'.
	""
	Smalltalk
		browseAllSelect: [:method | (method propertyValueAt: #libraryName) notNil]
		label: 'Methods in some library'.
	""
	(Smalltalk allSelect: [:method | (method propertyValueAt: #libraryName) notNil])
		collect: [ :m | m compiledMethod propertyValueAt: #libraryName ] :: asSet asArray sort
			do: [ :libraryName | Smalltalk browseAllSelect: [:method | (method propertyValueAt: #libraryName) = libraryName] label: 'Library: ', libraryName].
	"
	^self browseMessageList: (self allSelect: aBlock) name: aString! !
