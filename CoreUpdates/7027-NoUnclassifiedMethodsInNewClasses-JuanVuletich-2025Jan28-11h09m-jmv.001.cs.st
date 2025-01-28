'From Cuis7.3 [latest update: #7026] on 28 January 2025 at 11:10:42 am'!

!ClassOrganizer methodsFor: 'accessing' stamp: 'jmv 1/28/2025 11:07:43'!
setDefaultList: anArray
	| oldDict oldCategories |
	oldDict := self elementCategoryDict.
	oldCategories := self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		categoryArray := anArray isEmpty ifTrue: [#()] ifFalse: [{self class defaultCategory}].
		categoryStops := Array with: anArray size.
		elementArray := anArray ].
	self notifyOfChangedSelectorsOldDict: oldDict newDict: self elementCategoryDict.
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.! !

