'From Cuis 6.0 [latest update: #5832] on 5 June 2023 at 6:49:54 pm'!
'Description '!
!provides: 'ExamplesBrowser' 1 18!
SystemOrganization addCategory: 'ExamplesBrowser'!


!classDefinition: #ExamplesBrowserWindow category: 'ExamplesBrowser'!
SystemWindow subclass: #ExamplesBrowserWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ExamplesBrowser'!
!classDefinition: 'ExamplesBrowserWindow class' category: 'ExamplesBrowser'!
ExamplesBrowserWindow class
	instanceVariableNames: ''!

!classDefinition: #ExamplesBrowser category: 'ExamplesBrowser'!
Object subclass: #ExamplesBrowser
	instanceVariableNames: 'examples categoriesIndex subCategoriesIndex examplesListIndex subCategoriesList examplesList categoriesList'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ExamplesBrowser'!
!classDefinition: 'ExamplesBrowser class' category: 'ExamplesBrowser'!
ExamplesBrowser class
	instanceVariableNames: ''!


!ExamplesBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 18:29:45'!
buildMorphicWindow

	| navigationLayout categoriesList subCategoriesList examplesList content buttons exampleSourceMorph |
	
	navigationLayout := LayoutMorph newColumn.
	
	categoriesList := PluggableListMorph model: model 
					listGetter: #examplesCategories 
					indexGetter: #examplesCategoriesIndex 
					indexSetter: #examplesCategoriesIndex:.
					
	subCategoriesList := PluggableListMorph model: model 
						listGetter: #examplesSubCategories
						indexGetter: #examplesSubCategoriesIndex 
						indexSetter: #examplesSubCategoriesIndex:.
						
	examplesList := PluggableListMorph model: model 
						listGetter: #examplesList
						indexGetter: #examplesListIndex
						indexSetter: #examplesListIndex:.
						
	navigationLayout 
		addMorph: categoriesList proportionalHeight: 0.33;
		addAdjusterAndMorph: subCategoriesList proportionalHeight: 0.33;
		addAdjusterAndMorph: examplesList proportionalHeight: 0.33.
		
	layoutMorph addMorph: navigationLayout proportionalWidth: 0.33.
	
	content := LayoutMorph newColumn.
	
	exampleSourceMorph := TextModelMorph textProvider: model textGetter: #exampleSource.
	exampleSourceMorph disableEditing.
	content addMorphUseAll: exampleSourceMorph.
	
	buttons := LayoutMorph newRow.
	
	buttons addMorph: (PluggableButtonMorph model: model action: #runExample label: 'Run').
	buttons addMorph: (PluggableButtonMorph model: model action: #inspectExample label: 'Inspect').
	buttons addMorph: (PluggableButtonMorph model: model action: #exploreExample label: 'Explore').
	buttons addMorph: (PluggableButtonMorph model: model action: #browseExample label: 'Browse').
	content addMorph: buttons fixedHeight: 30.
	
	layoutMorph addAdjusterAndMorph: content layoutSpec: LayoutSpec useAll.! !

!ExamplesBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 11:12:16'!
initialize

	super initialize.
	
	self beRow.! !

!ExamplesBrowserWindow class methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 11:51:07'!
open
	"self open"
	^ self open: ExamplesBrowser new label: 'Examples browser'! !

!ExamplesBrowserWindow class methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 18:49:30'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 				-> 	10.
			#itemOrder 				-> 	50.
			#label 				->	'Examples Browser'.
			#object 				-> 	ExamplesBrowserWindow.
			#selector 				-> 	#open.
			#icon 				-> 	#morphsIcon.
			#balloonText 				-> 	'A browser tool for running examples'.
		} asDictionary}`! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 12:11:46'!
browseExample

	self currentExample ifNotNil: [:example | example browse]
		! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/14/2021 12:09:03'!
collectExamples

	"Collect examples in Smalltalk system"
	
	"Traverse all system classes looking for class methods that start with #example or are annotated with an 'example' pragma."
	
	examples _ Dictionary new.

	Smalltalk organization categories do: [:cat |
		(Smalltalk organization classesAt: cat) do: [:class | | annotatedMethods namedMethods exampleMethods |
			annotatedMethods _ (Pragma allNamed: #example in: class) collect: #method.
			namedMethods _ class class methodsSelect: [:method | method selector asString includesSubstring: 'example' caseSensitive: false].
			exampleMethods _ (annotatedMethods, namedMethods) asSet asOrderedCollection.
			exampleMethods ifNotEmpty: [ | catExamples |
				examples at: cat ifAbsent: [examples at: cat asSymbol put: Dictionary new].
				catExamples _ examples at: cat.
				catExamples at: class name put: exampleMethods]]].! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 11:49:59'!
currentCategory

	^ categoriesIndex ifNotNil: [:index | categoriesList at: index]! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 11:49:41'!
currentExample

	^ examplesListIndex ifNotNil: [:index | examplesList at: index]! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 11:49:21'!
currentSubCategory

	^ subCategoriesIndex ifNotNil: [:index | subCategoriesList at: index]! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 18:38:16'!
editorClassFor: textGetter
	^SmalltalkEditor! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 18:27:48'!
exampleSource

	^ self currentExample 
		ifNotNil: [:example | 
			String streamContents: [:s |
				s nextPutAll: example getSource]]
		ifNil: ['']! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/14/2021 12:13:01'!
examplesCategories
	^ categoriesList! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 10:40:42'!
examplesCategoriesIndex
	^ categoriesIndex ifNil: [0]! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/12/2021 11:00:21'!
examplesCategoriesIndex: anIndex

	anIndex isZero ifTrue: [^ categoriesIndex _ nil].

	categoriesIndex _ anIndex.
	
	subCategoriesList _ (examples at: self currentCategory) keys asSortedCollection: [:x :y | x < y] .
	self changed: #examplesSubCategories.
	
	self examplesSubCategoriesIndex: 1! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 10:24:11'!
examplesInCategory: aCategory
	^ examples at: aCategory! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 13:06:35'!
examplesList
	^ examplesList collect: [:example | self formatExampleName: example selector asString]! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 10:43:04'!
examplesListIndex
	^ examplesListIndex ifNil: [0]! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 18:27:48'!
examplesListIndex: anIndex

	anIndex isZero ifTrue: [^ examplesListIndex _ nil].
	examplesListIndex _ anIndex.
	self changed: #exampleSource! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 11:53:07'!
examplesSubCategories

	^ subCategoriesList ! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 10:42:10'!
examplesSubCategoriesIndex
	^ subCategoriesIndex ifNil: [0]! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/12/2021 11:03:00'!
examplesSubCategoriesIndex: anIndex
	anIndex isZero ifTrue: [^ subCategoriesIndex _ nil].
	subCategoriesIndex _ anIndex.
	
	examplesList _ ((examples at: self currentCategory) at: self currentSubCategory)
					asSortedCollection: [:x :y | x selector < y selector].
	self changed: #examplesList.
	self examplesListIndex: 1.! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 12:35:32'!
exploreExample
	self currentExample ifNotNil: [:example | (example methodClass soleInstance perform: example selector) explore]! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 13:09:46'!
formatExampleName: aString

	^ String streamContents: [:s |
		s nextPut: aString first asUppercase.
		aString allButFirstDo: [:char |
			char isDigit ifTrue: [
				s nextPut: Character space; nextPut: char]
			ifFalse: [
				char isUppercase ifTrue: [
					s nextPut: Character space;					nextPut: char asLowercase]
				ifFalse: [s nextPut: char]]]]! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/14/2021 12:13:29'!
initialize

	self collectExamples.
	
	categoriesList _ examples keys asSortedCollection: [:x :y | x < y].
	subCategoriesList _ OrderedCollection new.
	examplesList _ OrderedCollection new.! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 12:35:24'!
inspectExample
	self currentExample ifNotNil: [:example | (example methodClass soleInstance perform: example selector) inspect]! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 12/11/2021 12:37:28'!
runExample
	self currentExample ifNotNil: [:example | |result|
		result _ example methodClass soleInstance perform: example selector.
		(result isKindOf: Morph) ifTrue: [result openInWorld]]! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 18:39:25'!
shouldAutoHighlight
	^ false! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 18:44:25'!
shouldStyle: text with: anSHTextStyler
	self currentExample ifNotNil: [:example | anSHTextStyler classOrMetaClass: example methodClass].
	^ true! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 18:39:53'!
shoutMustKeepContentsSize
	^ false! !

!ExamplesBrowser methodsFor: 'as yet unclassified' stamp: 'MM 6/5/2023 18:38:51'!
textStylerClassFor: textGetter
	^SHTextStylerST80! !

!ExamplesBrowser class methodsFor: 'as yet unclassified' stamp: 'MM 12/12/2021 10:57:10'!
windowColor
	^ Color r: 0.2 g: 1.0 b: 0.6! !
