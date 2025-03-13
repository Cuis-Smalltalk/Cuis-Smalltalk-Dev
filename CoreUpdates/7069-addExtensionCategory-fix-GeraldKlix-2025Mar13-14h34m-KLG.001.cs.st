'From Cuis7.3 [latest update: #7068] on 13 March 2025 at 2:35:29 pm'!

!Browser methodsFor: 'message category functions' stamp: 'KLG 3/8/2025 19:04:27'!
addExtensionCategory
	"Present a choice package names and let the user enter a new category suffx.
	Take the suffix form the selected category."

	| packageNames packageNameIndex newCategoryName oldMessageCategoryListIndex |
	
	selectedClassName ifNil: [ ^self ].

	packageNames := (CodePackage installedPackages collect: [ :package |
		package packageName ]) sorted.
	packageNames ifEmpty: [ ^ self ].
	
	packageNameIndex := PopUpMenu 
		withCaption: 'Which package to extend?'
		chooseFrom: packageNames.
	packageNameIndex = 0 ifTrue: [ ^ self ].

	(newCategoryName := self 
		request: 'Enter the category name'
		initialAnswer: 
			(String streamContents: [ :stream |
				stream
					nextPut: $*;
					nextPutAll: (packageNames at: packageNameIndex).
				selectedMessageCategory ifNotNil: [
					stream
						nextPut: $-;
						nextPutAll: selectedMessageCategory ] ])) ifEmpty: [ ^ self ].
	(newCategoryName at: 1) = $*
		ifFalse: [
			self confirm:
				"I know about with #withNewlines ..."
				`'The category choosen does not start with an asterix (*).',
				String newLineString,
				'Do you want to use it anyway?'` :: ifFalse: [ ^ self ] ]
		ifTrue: [
			packageNames includes: newCategoryName allButFirst ::
				ifTrue: [
					self confirm:
						`'The name choosen denotes a package without a documenting suffix',
						String newLineString,
						'Do you want to use it anyway?'`:: ifFalse: [ ^ self ] ]
				ifFalse: [
					packageNames includes: (newCategoryName copyUpToLast: $-) allButFirst :: ifFalse: [
						self confirm:
							`'The category or prefix choosen does not match any package name.', 
							String newLineString,
							'Do you want to use it anyway?'` :: ifFalse: [ ^ self ] ] ] ].
	oldMessageCategoryListIndex := self messageCategoryListIndex.
		self classOrMetaClassOrganizer
			addCategory: newCategoryName
			before: selectedMessageCategory.
		self changed: #messageCategoryList.
		self messageCategoryListIndex:
			(oldMessageCategoryListIndex = 0
				ifTrue: [ self classOrMetaClassOrganizer categories size + 1 ]
				ifFalse: [ oldMessageCategoryListIndex ]).
		self changed: #messageCategoryList! !

