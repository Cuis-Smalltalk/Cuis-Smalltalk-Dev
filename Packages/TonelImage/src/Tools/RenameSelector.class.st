Class {
	#name : 'RenameSelector',
	#superclass : 'ChangeSelectorKeepingParameters',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'create new implementors - private' }
RenameSelector >> addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords [

	rangesToNewKeywords add: aKeywordRange -> (newSelectorKeywords at: index) 	
]
