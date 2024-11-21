'From Cuis7.1 [latest update: #6830] on 21 November 2024 at 2:31:05 pm'!

!Browser methodsFor: 'initialization' stamp: 'jmv 11/21/2024 14:28:32'!
setSelector: aSymbol
	"Make the receiver point at the given selector, in the currently chosen class"

	| aClass messageCatIndex |
	aSymbol ifNil: [^ self].
	(aClass := self selectedClassOrMetaClass) ifNil: [^ self].

	(selectedMessageCategory isNil or: [ selectedMessageCategory == ClassOrganizer allCategory ])
		ifTrue: [
			self messageListIndex:
				(aClass organization allMethodSelectors indexOf: aSymbol) ]
		ifFalse: [
			messageCatIndex := aClass organization numberOfCategoryOfElement: aSymbol.
			messageCatIndex = 0 ifFalse: [
				self messageCategoryListIndex: messageCatIndex + 1.
				self messageListIndex:
					((aClass organization listAtCategoryNumber: messageCatIndex)
						indexOf: aSymbol) ]].! !

