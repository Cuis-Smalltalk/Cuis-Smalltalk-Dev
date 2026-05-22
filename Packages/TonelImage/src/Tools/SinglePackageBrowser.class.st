Class {
	#name : 'SinglePackageBrowser',
	#superclass : 'Browser',
	#instVars : [
		'package'
	],
	#category : 'Tools-Packages',
	#package : 'Tools',
	#tag : 'Packages'
}

{ #category : 'lists' }
SinglePackageBrowser >> basicClassList [
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."
	| answer |
	answer := selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [
			(package includesSystemCategory: selectedSystemCategory)
				ifTrue: [ systemOrganizer fullListAtCategoryNamed: selectedSystemCategory ]
				ifFalse: [ 
					package extensionClassNamesIn:
						(selectedSystemCategory copyFrom: 2 to: selectedSystemCategory size) ]].
	selectedClassName ifNil: [
		answer isEmpty ifFalse: [
			selectedClassName := answer first.
			self setClassOrganizer.
			self editSelection: #editClass ]].
	^answer
]

{ #category : 'accessing' }
SinglePackageBrowser >> defaultBrowserTitle [
	^ 'Browser for package: ', package packageName
]

{ #category : 'lists' }
SinglePackageBrowser >> messageCategoryList [
	"Answer the selected category of messages."

	"Do not include the -- all -- category"
	| answer |
	answer := self rawMessageCategoryList.
	selectedMessageCategory ifNil: [
		answer isEmpty ifFalse: [
			(package includesSystemCategory: selectedSystemCategory) ifFalse: [
				selectedMessageCategory := answer first ]]].
	^answer
]

{ #category : 'lists' }
SinglePackageBrowser >> messageList [
	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!) (??), it is reset to zero."

	| answer |
	answer := selectedMessageCategory
		ifNil: [ #() ]
		ifNotNil: [
			(self classOrMetaClassOrganizer listAtCategoryNamed: selectedMessageCategory) ifNil: [
				selectedMessageCategory := nil.
				#() ]].
	selectedMessage ifNil: [
		answer isEmpty ifFalse: [
			(package includesSystemCategory: selectedSystemCategory) ifFalse: [
				selectedMessage := answer first.
				self editSelection: #editMessage ]]].
	^answer
]

{ #category : 'system category functions' }
SinglePackageBrowser >> newSystemCategoryNameInitialAnswer [
	"Make a suggestion for a new sytems category.
	
	Provide something more usefull, e.g. the package name or a sensible derivate of it."

	| packageName | 
	^ (self systemCategoryList includes: (packageName := package packageName))
		ifTrue: [ packageName, ' - Sub-Category-Name' ]
		ifFalse: [ packageName ]

]

{ #category : 'accessing' }
SinglePackageBrowser >> package: aCodePackage [

	package := aCodePackage
]

{ #category : 'lists' }
SinglePackageBrowser >> rawMessageCategoryList [
	"Answer the selected category of messages."


	^ (package includesSystemCategory: selectedSystemCategory)
		ifTrue: [
			super rawMessageCategoryList reject: [ :cat | package isForeignClassExtension: cat ]]
		ifFalse: [
			super rawMessageCategoryList select: [ :cat | package isYourClassExtension: cat ]]
]

{ #category : 'lists' }
SinglePackageBrowser >> systemCategoryList [

	^package systemCategories,
		(package systemCategoriesWithExtensionMethods collect: [ :cat |
			'*', cat ])
]
