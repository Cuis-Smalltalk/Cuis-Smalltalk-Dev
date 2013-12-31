'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 29 March 2012 at 7:03:36 pm'!

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 3/29/2012 19:02'!
                          messageList
	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!!) (??), it is reset to zero."

	| answer |
	answer _ selectedMessageCategory
		ifNil: [ #() ]
		ifNotNil: [
			(self classOrMetaClassOrganizer listAtCategoryNamed: selectedMessageCategory) ifNil: [
				selectedMessageCategory _ nil.
				#() ]].
	selectedMessage ifNil: [
		answer size = 0 ifFalse: [
			(package includesSystemCategory: selectedSystemCategory) ifFalse: [
				selectedMessage _ answer first.
				self editSelection: #editMessage ]]].
	^answer! !

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 3/29/2012 19:03'!
        rawMessageCategoryList
	"Answer the selected category of messages."


	^ (package includesSystemCategory: selectedSystemCategory)
		ifTrue: [
			super rawMessageCategoryList reject: [ :cat | package isForeignClassExtension: cat ]]
		ifFalse: [
			super rawMessageCategoryList select: [ :cat | package isYourClassExtension: cat ]]! !

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 3/29/2012 19:03'!
          systemCategoryList

	^package systemCategories,
		(package systemCategoriesWithExtensionMethods collect: [ :cat |
			'*', cat ])! !

