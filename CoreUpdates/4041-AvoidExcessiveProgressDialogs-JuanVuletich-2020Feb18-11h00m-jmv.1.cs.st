'From Cuis 5.0 [latest update: #4040] on 18 February 2020 at 11:01:29 am'!

!CodePackage methodsFor: 'listing' stamp: 'jmv 2/18/2020 11:00:34'!
sortedExtensionMethodsDo: aBlock displayingProgress: aString
	"Include both class and instance methods we define, for classes we don't define."
	| externalClasses methods |
	externalClasses _ self externalClasses.
	aString
		displayProgressAt: Sensor mousePoint
		from: 0 to: externalClasses size
		during: [ :barBlock |
			externalClasses withIndexDo: [ :classOrMetaClass :i |
				barBlock value: i.
				methods _ Array streamContents: [ :stream |
					(self extensionCategoriesForClass: classOrMetaClass) do: [ :cat |
						self methodsInCategory: cat ofClass: classOrMetaClass do: [ :m |
							stream nextPut: m ]]].
				methods sort: [ :a :b |
					a methodSymbol < b methodSymbol ].
				methods do: aBlock.
				]
			]! !

